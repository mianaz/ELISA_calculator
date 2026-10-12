# Main Shiny application file for ELISA Standard Curve Calculator (Beta)
library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(readr)
library(writexl)
library(drc)
library(ggplot2)
library(ggprism)
library(dplyr)
library(tidyr)
library(ggrepel)
library(plotly)
library(magrittr)
library(tibble)
library(R6)

# Load modular components
source("R/ui.R")
source("R/io/plate_reader_parser.R")  # Plate reader format parser
source("R/analysis/analyzer.R")  # Generalized analyzer with all dependencies

# Create UI
ui <- create_ui()

# ---- Top-level helper: OD -> hex color for heatmap mode ----
od_to_hex <- function(od_val, well_type_map = NULL) {
  od_val <- max(0, min(3, as.numeric(od_val)))
  intensity <- round(od_val / 3 * 255)
  r <- 255 - round(intensity * 0.8)
  g <- 255 - round(intensity * 0.6)
  b <- 255
  sprintf("rgb(%d,%d,%d)", r, g, b)
}

# ---- Top-level helper: render plate HTML (used by plateViewOutput) ----
render_plate_html <- function(well_type_map, well_od_map, input_obj,
                               parsed_plate_val = NULL, well_assigns = list(),
                               excluded = character(0), mode = "type",
                               plate_format = "auto") {

  std_label   <- if (!is.null(input_obj)) input_obj$std_label   %||% "Standard" else "Standard"
  blank_label <- if (!is.null(input_obj)) input_obj$blank_label %||% "Blank"    else "Blank"
  nsb_label   <- if (!is.null(input_obj)) input_obj$nsb_label   %||% "NSB"      else "NSB"
  b0_label    <- if (!is.null(input_obj)) input_obj$b0_label    %||% "B0"       else "B0"

  classify_well <- function(type) {
    if (is.null(type) || is.na(type) || type == "" || type == "Empty") return("empty")
    if (type == std_label || type == "Standard") return("std")
    if (type == blank_label || type == "Blank") return("blank")
    if (type %in% c(nsb_label, b0_label, "NSB", "B0", "QC_Low", "QC_Mid", "QC_High")) return("control")
    return("sample")
  }

  # Determine plate dimensions: respect plate_format selector, then auto-detect, then default 96-well
  if (!is.null(parsed_plate_val$plate_corrected)) {
    pm <- parsed_plate_val$plate_corrected
    row_labels <- LETTERS[1:nrow(pm)]
    col_labels <- 1:ncol(pm)
  } else if (!is.null(plate_format) && plate_format != "auto") {
    dims <- switch(plate_format,
      "96"  = c(8, 12), "384" = c(16, 24),
      "48"  = c(6,  8), "24"  = c(4,   6), c(8, 12))
    row_labels <- LETTERS[1:dims[1]]
    col_labels <- 1:dims[2]
  } else {
    # Auto-detect: accept any letter + 1-2 digits as a well ID
    all_well_ids <- c(names(well_type_map), names(well_od_map), names(well_assigns))
    all_well_ids <- unique(all_well_ids[grepl("^[A-Za-z][0-9]{1,2}$", all_well_ids)])
    if (length(all_well_ids) > 0) {
      row_letters <- toupper(gsub("[0-9]+$", "", all_well_ids))
      col_nums    <- suppressWarnings(as.integer(gsub("^[A-Za-z]+", "", all_well_ids)))
      max_row_idx <- max(match(row_letters, LETTERS), na.rm = TRUE)
      min_row_idx <- min(match(row_letters, LETTERS), na.rm = TRUE)
      max_col     <- max(col_nums, na.rm = TRUE)
      if (!is.finite(max_row_idx)) { max_row_idx <- 8; min_row_idx <- 1 }
      if (!is.finite(max_col))     max_col <- 12
      # Snap to standard plate only when data fits neatly within one
      fits_96  <- max_row_idx <= 8  && max_col <= 12
      fits_384 <- max_row_idx <= 16 && max_col <= 24
      fits_48  <- max_row_idx <= 6  && max_col <= 8
      fits_24  <- max_row_idx <= 4  && max_col <= 6
      if (fits_24) {
        plate_rows_n <- 4; plate_cols_n <- 6
      } else if (fits_48) {
        plate_rows_n <- 6; plate_cols_n <- 8
      } else if (fits_96) {
        plate_rows_n <- 8; plate_cols_n <- 12
      } else if (fits_384) {
        plate_rows_n <- 16; plate_cols_n <- 24
      } else {
        # Non-standard: render exactly the data range
        plate_rows_n <- max_row_idx
        plate_cols_n <- max_col
      }
      # Always start rows from A (row 1), not from min_row_idx
      row_labels <- LETTERS[1:plate_rows_n]
      col_labels <- 1:plate_cols_n
    } else {
      row_labels <- LETTERS[1:8]; col_labels <- 1:12
    }
  }

  plate_rows <- list()

  # Column headers
  header_items <- list(tags$span(class = "plate-row-label", ""))
  for (col in col_labels) {
    header_items <- c(header_items, list(tags$span(class = "plate-col-header", as.character(col))))
  }
  plate_rows <- c(plate_rows, list(div(header_items)))

  for (row in row_labels) {
    row_items <- list(tags$span(class = "plate-row-label", row))
    for (col in col_labels) {
      well_id <- paste0(row, col)
      type    <- if (!is.null(well_type_map)) well_type_map[well_id] else NA_character_
      # Apply manual assignment override
      if (!is.null(well_assigns) && !is.null(well_assigns[[well_id]])) type <- well_assigns[[well_id]]
      od_val  <- if (!is.null(well_od_map)) well_od_map[well_id] else NA_real_

      # Also check parsed plate matrix for OD value
      if (is.null(od_val) || is.na(od_val %||% NA)) {
        if (!is.null(parsed_plate_val$plate_corrected)) {
          r_idx <- which(LETTERS == row)
          if (length(r_idx) > 0 && r_idx <= nrow(parsed_plate_val$plate_corrected) &&
              col <= ncol(parsed_plate_val$plate_corrected)) {
            od_val <- parsed_plate_val$plate_corrected[r_idx, col]
          }
        }
      }

      is_excluded <- well_id %in% excluded
      well_class <- if (is_excluded) "excluded" else classify_well(type)
      type_label <- if (!is.null(type) && !is.na(type %||% NA) && type != "") type else "Empty"
      od_label   <- if (!is.null(od_val) && !is.na(od_val %||% NA)) sprintf("%.4f", as.numeric(od_val)) else ""
      tooltip    <- paste0(well_id, ": ", type_label,
                           if (od_label != "") paste0(" | OD=", od_label) else "")
      # Heatmap mode: use OD value to set background color
      extra_style <- ""
      if (mode == "heatmap" && !is.null(od_val) && !is.na(od_val %||% NA)) {
        od_num <- as.numeric(od_val)
        if (!is_excluded) {
          well_class <- "heatmap-well"
          extra_style <- paste0("background-color:", od_to_hex(od_num), ";")
        }
      }

      row_items <- c(row_items, list(
        tags$span(
          class          = paste("plate-well", well_class),
          `data-well-id` = well_id,
          title          = tooltip,
          style          = extra_style
          # No display text — color-code only; OD shown in tooltip and right-panel table
        )
      ))
    }
    plate_rows <- c(plate_rows, list(div(row_items)))
  }

  div(class = "plate-container", plate_rows)
}

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store analysis results
  results <- reactiveVal(NULL)
  raw_data <- reactiveVal(NULL)
  parsed_plate <- reactiveVal(NULL)  # Stores parsed plate grid data from plate reader files
  exclusion_audit_trail <- reactiveVal(list(entries = list(), created_at = Sys.time()))
  well_assignments <- reactiveVal(list())  # Manual well type overrides from Plate View click
  excluded_wells <- reactiveVal(character(0))  # Wells excluded from analysis

  # ---- Paste data parser ----
  parse_pasted_data <- function(text) {
    text <- trimws(gsub("\r\n", "\n", gsub("\r", "\n", text)))
    if (nchar(text) == 0) return(list(format = "error", message = "No data pasted"))

    lines <- strsplit(text, "\n")[[1]]
    lines <- trimws(lines)
    lines <- lines[nchar(lines) > 0]
    if (length(lines) < 2) return(list(format = "error", message = "Not enough rows"))

    # Split by tab
    rows <- lapply(lines, function(l) trimws(strsplit(l, "\t")[[1]]))

    # Detect plate format: first row has numeric column headers (1-12), data rows start A-H
    first_row <- rows[[1]]
    maybe_nums <- suppressWarnings(as.numeric(first_row[first_row != ""]))
    data_row_labels <- trimws(sapply(rows[-1], `[`, 1))
    is_plate <- sum(!is.na(maybe_nums)) >= 6 &&
                all(toupper(data_row_labels) %in% c(LETTERS[1:8], ""))

    if (is_plate) {
      # Plate grid format
      start_col <- if (first_row[1] == "" || is.na(suppressWarnings(as.numeric(first_row[1])))) 2L else 1L
      plate_mat <- matrix(NA_real_, 8, 12)
      for (i in seq_along(rows[-1])) {
        if (i > 8) break
        r <- rows[[i + 1]]
        row_letter <- toupper(trimws(r[1]))
        row_idx <- which(LETTERS[1:8] == row_letter)
        if (length(row_idx) == 0) next
        vals <- suppressWarnings(as.numeric(r[start_col:min(length(r), start_col + 11)]))
        for (j in seq_along(vals)) {
          if (j > 12) break
          plate_mat[row_idx, j] <- vals[j]
        }
      }
      # Convert to long with Well ID + OD
      result_rows <- list()
      for (r in 1:8) {
        for (c in 1:12) {
          result_rows[[length(result_rows) + 1]] <- data.frame(
            `Well ID`     = paste0(LETTERS[r], c),
            OD            = plate_mat[r, c],
            `Sample Type` = "",
            check.names   = FALSE
          )
        }
      }
      df <- do.call(rbind, result_rows)
      df <- df[!is.na(df$OD), ]
      return(list(format = "plate", data = df, plate_matrix = plate_mat,
                  message = paste("Plate grid parsed:", nrow(df), "wells with OD values")))
    } else {
      # Long / tabular format
      tryCatch({
        df <- as.data.frame(readr::read_tsv(
          I(paste(lines, collapse = "\n")), show_col_types = FALSE))
        return(list(format = "long", data = df,
                    message = paste("Long format parsed:", nrow(df), "rows,", ncol(df), "columns")))
      }, error = function(e) {
        list(format = "error", message = paste("Parse error:", e$message))
      })
    }
  }

  # Helper function to parse dilution factors from text input
  parse_dilution_factors <- function(text) {
    if (is.null(text) || text == "") return(NULL)

    lines <- strsplit(text, "\n")[[1]]
    lines <- trimws(lines)
    lines <- lines[lines != ""]

    if (length(lines) == 0) return(NULL)

    factors <- list()
    for (line in lines) {
      parts <- strsplit(line, "=")[[1]]
      if (length(parts) == 2) {
        sample_type <- trimws(parts[1])
        factor_val <- as.numeric(trimws(parts[2]))
        if (!is.na(factor_val)) {
          factors[[sample_type]] <- factor_val
        }
      }
    }

    if (length(factors) == 0) return(NULL)
    return(unlist(factors))
  }

  # Helper function to read data file based on extension
  read_data_file <- function(file_path, file_name, sheet = NULL) {
    ext <- tolower(tools::file_ext(file_name))

    if (ext %in% c("xlsx", "xls")) {
      # Excel file
      if (is.null(sheet)) {
        sheet_names <- readxl::excel_sheets(file_path)
        sheet <- if(length(sheet_names) > 0) sheet_names[1] else 1
      }
      return(readxl::read_excel(file_path, sheet = sheet))
    } else if (ext == "csv") {
      # CSV file (comma-separated)
      return(readr::read_csv(file_path, show_col_types = FALSE))
    } else if (ext == "tsv") {
      # TSV file (tab-separated)
      return(readr::read_tsv(file_path, show_col_types = FALSE))
    } else {
      stop("Unsupported file format. Please use .xlsx, .xls, .csv, or .tsv files.")
    }
  }

  # Helper function to check if file is Excel
  is_excel_file <- function(file_name) {
    ext <- tolower(tools::file_ext(file_name))
    return(ext %in% c("xlsx", "xls"))
  }

  # Helper to find a column by fuzzy matching (case-insensitive, partial match)
  find_column <- function(column_names, patterns, fallback_index = NULL) {
    lc_names <- tolower(column_names)
    for (pat in patterns) {
      # Exact match first
      if (pat %in% column_names) return(pat)
      # Case-insensitive exact match
      idx <- which(lc_names == tolower(pat))
      if (length(idx) > 0) return(column_names[idx[1]])
      # Case-insensitive partial match (column starts with pattern)
      lc_pat <- tolower(pat)
      idx <- which(startsWith(lc_names, lc_pat))
      if (length(idx) > 0) return(column_names[idx[1]])
      # Check if pattern is contained in any column name
      idx <- which(grepl(lc_pat, lc_names, fixed = TRUE))
      if (length(idx) > 0) return(column_names[idx[1]])
    }
    # Fallback to positional index
    if (!is.null(fallback_index) && fallback_index <= length(column_names)) {
      return(column_names[fallback_index])
    }
    return(column_names[1])
  }

  # Helper to update column dropdowns from a set of column names
  update_column_dropdowns <- function(session, column_names) {
    well_sel <- find_column(column_names, c("Well ID", "Well", "WellID", "Well_ID"), 1)
    type_sel <- find_column(column_names, c("Sample Type", "SampleType", "Sample_Type", "Type"), 2)
    conc_sel <- find_column(column_names, c("Concentration", "Conc", "Concentration (ng/ml)", "Concentration (pg/ml)"), 3)
    od_sel   <- find_column(column_names, c("OD", "Absorbance", "Abs"), 4)

    # For corrected OD, try several patterns; default to "None"
    od_corr_sel <- "None"
    for (pat in c("OD (corrected)", "OD Corrected", "OD_corrected", "OD_Corrected", "Corrected", "OD_cor")) {
      match <- grep(pat, column_names, ignore.case = TRUE, fixed = TRUE)
      if (length(match) > 0) { od_corr_sel <- column_names[match[1]]; break }
    }

    updateSelectInput(session, "well_col", choices = column_names, selected = well_sel)
    updateSelectInput(session, "type_col", choices = column_names, selected = type_sel)
    updateSelectInput(session, "conc_col", choices = column_names, selected = conc_sel)
    updateSelectInput(session, "od_col",   choices = column_names, selected = od_sel)
    updateSelectInput(session, "od_corr_col", choices = c("None", column_names), selected = od_corr_sel)
  }

  # Set flags for UI conditional elements
  output$fileUploaded <- reactive({
    return(!is.null(input$file) || !is.null(raw_data()))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  output$analysisComplete <- reactive({
    return(!is.null(results()))
  })
  outputOptions(output, "analysisComplete", suspendWhenHidden = FALSE)

  # Flag for whether uploaded file is Excel (controls sheet selector visibility)
  output$isExcelFile <- reactive({
    req(input$file)
    return(is_excel_file(input$file$name))
  })
  outputOptions(output, "isExcelFile", suspendWhenHidden = FALSE)

  # Data preview output
  output$rawPreview <- renderDT({
    req(input$file)

    # If no raw data is stored yet, try to read it
    if(is.null(raw_data())) {
      tryCatch({
        # Check if user indicated plate reader format
        if (isTRUE(input$is_plate_format)) {
          # Try plate reader format parsing
          format_info <- detect_plate_format(input$file$datapath, input$file$name)

          if (format_info$format == "biotek") {
            plate_result <- parse_biotek_synergy(input$file$datapath)
            parsed_plate(plate_result)
            # Show the corrected plate as a preview table
            plate_df <- as.data.frame(plate_result$plate_corrected)
            colnames(plate_df) <- paste0("Col_", 1:12)
            plate_df$Row <- LETTERS[1:8]
            plate_df <- plate_df[, c("Row", paste0("Col_", 1:12))]
            raw_data(plate_df)
            showNotification(
              paste("Detected", format_info$instrument, "plate reader format.",
                    if (plate_result$dual_wavelength) "Dual wavelength (background corrected)." else "Single wavelength."),
              type = "message"
            )
          } else {
            # Try generic plate grid parsing
            plate_result <- parse_generic_plate(input$file$datapath, input$file$name)
            parsed_plate(list(plate_corrected = plate_result$plate))
            plate_df <- as.data.frame(plate_result$plate)
            colnames(plate_df) <- paste0("Col_", 1:12)
            plate_df$Row <- LETTERS[1:8]
            plate_df <- plate_df[, c("Row", paste0("Col_", 1:12))]
            raw_data(plate_df)
            showNotification("Detected plate grid format. Assign sample types in Plate View.", type = "message")
          }
        } else {
          # Standard columnar format
          if (is_excel_file(input$file$name)) {
            sheet_to_use <- if(!is.null(input$sheet)) input$sheet else NULL
            data <- read_data_file(input$file$datapath, input$file$name, sheet = sheet_to_use)
          } else {
            data <- read_data_file(input$file$datapath, input$file$name)
          }
          raw_data(data)
        }
      }, error = function(e) {
        # Just return an error without storing data
        return(
          DT::datatable(data.frame(Error = paste("Could not read data:", e$message)),
                      options = list(pageLength = 10, dom = 't'))
        )
      })
    }

    # Use the stored raw data for display
    if(!is.null(raw_data())) {
      return(
        DT::datatable(raw_data(),
                  options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'),
                  rownames = FALSE)
      )
    } else {
      # Fallback for when there's no data
      return(
        DT::datatable(data.frame(Message = "No data available. Please upload a file."),
                    options = list(pageLength = 10, dom = 't'))
      )
    }
  })

  # Observer for analyze button
  observeEvent(input$analyze, {
    req(input$file)
    # Only require sheet selection for Excel files
    if (is_excel_file(input$file$name)) {
      req(input$sheet)
    }

    # Show a progress notification
    withProgress(message = "Analyzing ELISA data...", value = 0, {

      # Increase the progress bar
      incProgress(0.2, detail = "Processing data")

      # Parse dilution factors from text input
      dilution_factors <- parse_dilution_factors(input$dilution_factors_text)

      # Get models to fit (ensure at least one)
      models <- input$models_to_fit
      if (is.null(models) || length(models) == 0) {
        models <- c("4PL", "Linear")
      }

      incProgress(0.1, detail = "Fitting models")

      # Determine sheet name (NULL for CSV files)
      sheet_to_use <- if (is_excel_file(input$file$name)) input$sheet else NULL

      # Run the analysis with user-selected parameters
      tryCatch({
        analysis_results <- analyze_elisa(
          file_path = input$file$datapath,
          sheet_name = sheet_to_use,
          file_name = input$file$name,
          well_col = input$well_col,
          type_col = input$type_col,
          conc_col = input$conc_col,
          od_col = input$od_col,
          od_corr_col = { ocv <- input$od_corr_col; if(is.null(ocv) || length(ocv) == 0 || isTRUE(ocv == "None")) input$od_col else ocv },
          use_corrected = input$use_corrected %||% FALSE,
          assay_type = input$assay_type %||% "auto",
          nsb_label = input$nsb_label %||% "NSB",
          b0_label = input$b0_label %||% "B0",
          std_label = input$std_label %||% "Standard",
          blank_label = input$blank_label %||% "Blank",
          skip_normalization = input$skip_normalization %||% FALSE,
          log_transform = input$log_transform %||% TRUE,
          remove_unlabeled = input$remove_unlabeled %||% TRUE,
          dilution_factors = dilution_factors,
          weight_type = input$weight_type %||% "none",
          models_to_fit = models,
          calculate_limits = input$calculate_limits %||% TRUE,
          normalize_per_plate = input$normalize_per_plate %||% FALSE
        )

        incProgress(0.5, detail = "Generating outputs")

        # Store updated results
        results(analysis_results)

        # Final progress
        incProgress(0.2, detail = "Complete")

        # Auto-navigate to Results tab
        updateNavbarPage(session, "mainNav", selected = "Results")

        # Show a success notification with QC status
        qc_status <- if (!is.null(analysis_results$qc_summary)) {
          analysis_results$qc_summary$overall_status
        } else {
          "COMPLETE"
        }

        showNotification(
          paste("Analysis completed! QC Status:", qc_status),
          type = if(qc_status == "PASS") "message" else "warning"
        )

      }, error = function(e) {
        message("=== Analysis Error ===")
        message("Message: ", e$message)
        message("Call: ", paste(deparse(e$call), collapse=" "))
        tryCatch(traceback(), error = function(e2) NULL)
        showNotification(
          paste("Error in analysis:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })

  # Observer for file input
  observeEvent(input$file, {
    req(input$file)

    # Reset raw data when new file is uploaded
    raw_data(NULL)

    # Get file extension
    ext <- tolower(tools::file_ext(input$file$name))

    tryCatch({
      # Read file based on extension
      if (ext %in% c("xlsx", "xls")) {
        # Excel file - get sheet names
        sheet_names <- readxl::excel_sheets(input$file$datapath)
        updateSelectInput(session, "sheet", choices = sheet_names)

        # Use first sheet by default
        sheet_to_use <- if(length(sheet_names) > 0) sheet_names[1] else 1

        # Read first sheet to get column names
        first_sheet_data <- readxl::read_excel(input$file$datapath, sheet = sheet_to_use)
        column_names <- colnames(first_sheet_data)

      } else if (ext %in% c("csv", "tsv")) {
        # CSV/TSV file - no sheets, set to "Data"
        updateSelectInput(session, "sheet", choices = c("Data"), selected = "Data")

        # Read data to get column names
        if (ext == "csv") {
          first_sheet_data <- readr::read_csv(input$file$datapath, show_col_types = FALSE)
        } else {
          first_sheet_data <- readr::read_tsv(input$file$datapath, show_col_types = FALSE)
        }
        column_names <- colnames(first_sheet_data)

      } else {
        showNotification("Unsupported file format. Please use .xlsx, .xls, .csv, or .tsv files.", type = "error")
        return()
      }

      # Update column selection dropdowns with fuzzy matching
      update_column_dropdowns(session, column_names)

      # Store raw data for preview
      raw_data(first_sheet_data)

      # Show a notification that file is loaded and ready for analysis
      file_type <- if(ext %in% c("xlsx", "xls")) "Excel" else toupper(ext)
      showNotification(paste0(file_type, " file loaded successfully. Please click 'Analyze Data' when ready."),
                      type = "message", duration = 5)

    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
    })
  })

  # Observer for sheet changes — re-read data and update column dropdowns
  observeEvent(input$sheet, {
    req(input$file)
    # Only relevant for Excel files with real sheet names
    if (!is_excel_file(input$file$name)) return()
    sheet_name <- input$sheet
    if (is.null(sheet_name) || sheet_name == "") return()

    tryCatch({
      sheet_data <- readxl::read_excel(input$file$datapath, sheet = sheet_name)
      column_names <- colnames(sheet_data)

      # Update column dropdowns for the newly selected sheet
      update_column_dropdowns(session, column_names)

      # Update raw data preview
      raw_data(sheet_data)

      message("Sheet changed to '", sheet_name, "': ", nrow(sheet_data), " rows, ",
              ncol(sheet_data), " columns")
    }, error = function(e) {
      showNotification(paste("Error reading sheet:", e$message), type = "error", duration = 5)
    })
  })

  # Helper function to load example data
  load_example_data <- function(example_file, example_name) {
    tryCatch({
      # Read the example data
      example_path <- file.path("example_data", example_file)

      if (!file.exists(example_path)) {
        showNotification(paste("Example file not found:", example_file), type = "error")
        return()
      }

      # Read the CSV data
      data <- readr::read_csv(example_path, show_col_types = FALSE)
      column_names <- colnames(data)

      # Store raw data for preview
      raw_data(data)

      # Update sheet selection (CSV has no sheets)
      updateSelectInput(session, "sheet", choices = c("Data"), selected = "Data")

      # Update column selections with fuzzy matching
      update_column_dropdowns(session, column_names)

      # Run analysis with the example data
      withProgress(message = paste("Loading", example_name, "..."), value = 0, {
        incProgress(0.3, detail = "Reading data")

        # Parse any dilution factors
        dilution_factors <- parse_dilution_factors(input$dilution_factors_text)

        # Get models to fit
        models <- input$models_to_fit
        if (is.null(models) || length(models) == 0) {
          models <- c("4PL", "5PL", "Linear")
        }

        incProgress(0.3, detail = "Fitting models")

        analysis_results <- analyze_elisa(
          file_path = example_path,
          sheet_name = "Data",
          file_name = example_file,
          well_col = "Well ID",
          type_col = "Sample Type",
          conc_col = "Concentration",
          od_col = "OD",
          od_corr_col = "OD (corrected)",
          use_corrected = FALSE,
          assay_type = "auto",
          nsb_label = input$nsb_label %||% "NSB",
          b0_label = input$b0_label %||% "B0",
          std_label = input$std_label %||% "Standard",
          blank_label = input$blank_label %||% "Blank",
          skip_normalization = input$skip_normalization %||% FALSE,
          log_transform = input$log_transform %||% TRUE,
          remove_unlabeled = input$remove_unlabeled %||% TRUE,
          dilution_factors = dilution_factors,
          weight_type = input$weight_type %||% "none",
          models_to_fit = models,
          calculate_limits = input$calculate_limits %||% TRUE,
          normalize_per_plate = input$normalize_per_plate %||% FALSE
        )

        incProgress(0.4, detail = "Complete")

        # Store results
        results(analysis_results)

        # Auto-navigate to Results tab
        updateNavbarPage(session, "mainNav", selected = "Results")

        # Show success notification
        qc_status <- if (!is.null(analysis_results$qc_summary)) {
          analysis_results$qc_summary$overall_status
        } else {
          "COMPLETE"
        }

        showNotification(
          paste(example_name, "loaded successfully! QC Status:", qc_status),
          type = if(qc_status == "PASS" || qc_status == "WARNING") "message" else "warning"
        )
      })

    }, error = function(e) {
      message("=== Example Data Error ===")
      message("Message: ", e$message)
      message("Call: ", paste(deparse(e$call), collapse=" "))
      showNotification(paste("Error loading example data:", e$message), type = "error", duration = 10)
    })
  }

  # Observer for competitive ELISA example
  observeEvent(input$loadExampleCompetitive, {
    load_example_data("competitive_elisa_melatonin.csv", "Competitive ELISA (real data)")
  })

  # Observer for direct ELISA example
  observeEvent(input$loadExampleDirect, {
    load_example_data("direct_elisa_IgG.csv", "Direct ELISA (simulated)")
  })

  # Observer for indirect ELISA example
  observeEvent(input$loadExampleIndirect, {
    load_example_data("indirect_elisa_TNFa.csv", "Indirect ELISA IgG (real plate reader data)")
  })

  # Observer for sandwich ELISA example
  observeEvent(input$loadExampleSandwich, {
    load_example_data("sandwich_elisa_IL6.csv", "Sandwich ELISA (real data, gtools::ELISA)")
  })

  # ============ Plate View Tab Outputs ============

  # Generate plate visualization from uploaded data (with click support)
  # Helper: check if a character vector contains real plate well IDs (e.g. A1, B12, P24)
  is_valid_well_id <- function(x) grepl("^[A-Za-z][0-9]{1,2}$", as.character(x))

  output$plateViewOutput <- renderUI({
    req(raw_data())
    data     <- raw_data()
    well_col <- input$well_col
    type_col <- input$type_col
    od_col   <- input$od_col
    mode     <- input$plateViewMode %||% "type"
    excluded <- excluded_wells()

    no_well_msg <- tagList(
      div(class = "text-center", style = "padding:2rem 1rem;color:#888;",
        icon("circle-question", class = "fa-2x", style = "margin-bottom:.5rem;"),
        p(strong("No well position data detected.")),
        p(class = "small", "Your data is in long format without plate coordinates."),
        p(class = "small", "To see a plate layout, your data needs a well column with IDs like A1, B2, H12, etc."),
        p(class = "small text-muted", "You can still assign well positions manually by clicking cells above, or
          paste plate-format data in the Upload tab.")
      )
    )

    # Case 1: no well column selected or it's not in the data
    if (is.null(well_col) || !(well_col %in% colnames(data))) {
      if (!is.null(parsed_plate())) {
        return(tagList(
          p(class = "text-info small", icon("info-circle"),
            " OD values loaded from plate grid. Click a well to assign its sample type."),
          render_plate_html(NULL, NULL, NULL, parsed_plate(), well_assignments(), excluded, mode)
        ))
      }
      return(no_well_msg)
    }

    wells <- as.character(data[[well_col]])

    # Case 2: well column exists but values are not plate well IDs — show long-format message
    valid_pct <- mean(is_valid_well_id(wells), na.rm = TRUE)
    if (valid_pct < 0.5) {
      return(no_well_msg)
    }

    types <- if (!is.null(type_col) && type_col %in% colnames(data)) as.character(data[[type_col]]) else rep("", length(wells))
    ods   <- if (!is.null(od_col)   && od_col   %in% colnames(data)) data[[od_col]]               else rep(NA_real_, length(wells))

    well_type_map <- setNames(types, wells)
    well_od_map   <- setNames(ods,   wells)

    assigns <- well_assignments()
    for (w in names(assigns)) {
      if (!grepl("_conc$", w)) well_type_map[[w]] <- assigns[[w]]
    }

    fmt <- input$plateFormat %||% "auto"
    render_plate_html(well_type_map, well_od_map, input, parsed_plate(), well_assignments(), excluded, mode, plate_format = fmt)
  })

  # Plate format selector
  output$plateFormatSelector <- renderUI({
    req(raw_data())
    selectInput("plateFormat", "Plate Format",
                choices = c("Auto-detect" = "auto", "96-well (8×12)" = "96",
                            "384-well (16×24)" = "384", "48-well (6×8)" = "48",
                            "24-well (4×6)" = "24"),
                selected = "auto")
  })

  # Unassigned wells warning: wells with OD data but no type assignment
  output$unassigned_warning <- renderUI({
    req(raw_data())
    data     <- raw_data()
    well_col <- input$well_col
    type_col <- input$type_col
    assigns  <- well_assignments()

    if (is.null(well_col) || !well_col %in% colnames(data)) return(NULL)
    wells <- as.character(data[[well_col]])
    if (mean(is_valid_well_id(wells), na.rm = TRUE) < 0.5) return(NULL)

    # Wells that have OD but no type info
    has_type <- if (!is.null(type_col) && type_col %in% colnames(data)) {
      types <- as.character(data[[type_col]])
      !is.na(types) & types != "" & types != "NA"
    } else {
      rep(FALSE, length(wells))
    }
    unassigned <- wells[!has_type & !(wells %in% names(assigns))]
    unassigned <- unassigned[is_valid_well_id(unassigned)]

    if (length(unassigned) == 0) return(NULL)
    div(class = "unassigned-warning",
      icon("triangle-exclamation"), " ",
      strong(length(unassigned), " well(s) have no sample type assigned"),
      " (", paste(head(unassigned, 6), collapse = ", "),
      if (length(unassigned) > 6) paste0("... +", length(unassigned) - 6, " more") else "",
      "). Analysis continues using type column; assign types in plate view for visual clarity."
    )
  })

  # Drag-select bulk assignment modal
  observeEvent(input$drag_selected_wells, {
    req(input$drag_selected_wells)
    wells <- input$drag_selected_wells
    if (length(wells) == 0) return()
    well_label <- if (length(wells) <= 8) paste(wells, collapse = ", ") else
                  paste0(paste(head(wells, 6), collapse = ", "), " ... +", length(wells) - 6, " more")
    showModal(modalDialog(
      title   = paste("Assign", length(wells), "Wells"),
      p(strong("Selected: "), well_label),
      selectInput("modal_bulk_type", "Assign as:",
                  choices = c("Standard", "Sample", "Blank", "NSB", "B0", "QC_Low", "QC_Mid", "QC_High")),
      footer  = tagList(
        modalButton("Cancel"),
        actionButton("modal_bulk_assign", "Assign All", class = "btn btn-primary")
      ),
      easyClose = TRUE
    ))
    # Store the selected wells for the observer below
    session$userData$bulk_wells <- wells
  })

  observeEvent(input$modal_bulk_assign, {
    wells <- session$userData$bulk_wells
    type  <- input$modal_bulk_type
    req(length(wells) > 0, !is.null(type))
    assigns <- well_assignments()
    for (w in wells) assigns[[w]] <- type
    well_assignments(assigns)
    showNotification(paste("Assigned", length(wells), "wells as", type), type = "message", duration = 3)
    removeModal()
  })

  # Plate table output (grid of OD values)
  output$plateTableOutput <- renderUI({
    req(raw_data())
    data <- raw_data()
    well_col <- input$well_col
    od_col   <- input$od_col
    excluded <- excluded_wells()

    if (is.null(well_col) || !well_col %in% colnames(data)) return(p(class="text-muted small", "No well column — plate table not available."))
    if (is.null(od_col)   || !od_col   %in% colnames(data)) return(p(class="text-muted small", "No OD column — plate table not available."))

    wells <- as.character(data[[well_col]])
    if (mean(is_valid_well_id(wells), na.rm = TRUE) < 0.5) {
      return(p(class="text-muted small", "Data does not contain plate well positions — table not available."))
    }

    ods   <- data[[od_col]]
    well_od_map <- setNames(ods, wells)

    # Determine plate dimensions from format selector or auto-detect
    fmt <- input$plateFormat %||% "auto"
    if (fmt == "96")  { plate_rows_n <- 8;  plate_cols_n <- 12 } else
    if (fmt == "384") { plate_rows_n <- 16; plate_cols_n <- 24 } else
    if (fmt == "48")  { plate_rows_n <- 6;  plate_cols_n <- 8  } else
    if (fmt == "24")  { plate_rows_n <- 4;  plate_cols_n <- 6  } else {
      vwells      <- wells[is_valid_well_id(wells)]
      row_letters <- toupper(gsub("[0-9]+$", "", vwells))
      col_nums    <- suppressWarnings(as.integer(gsub("^[A-Za-z]+", "", vwells)))
      max_row_idx <- max(match(row_letters, LETTERS), na.rm = TRUE)
      max_col     <- max(col_nums, na.rm = TRUE)
      if (!is.finite(max_row_idx)) max_row_idx <- 8
      if (!is.finite(max_col))     max_col     <- 12
      fits_96  <- max_row_idx <= 8  && max_col <= 12
      fits_384 <- max_row_idx <= 16 && max_col <= 24
      fits_48  <- max_row_idx <= 6  && max_col <= 8
      fits_24  <- max_row_idx <= 4  && max_col <= 6
      if (fits_24)  { plate_rows_n <- 4;  plate_cols_n <- 6  } else
      if (fits_48)  { plate_rows_n <- 6;  plate_cols_n <- 8  } else
      if (fits_96)  { plate_rows_n <- 8;  plate_cols_n <- 12 } else
      if (fits_384) { plate_rows_n <- 16; plate_cols_n <- 24 } else {
        plate_rows_n <- max_row_idx; plate_cols_n <- max_col
      }
    }
    row_lbl <- LETTERS[1:plate_rows_n]

    header <- tags$tr(tags$th(""), lapply(1:plate_cols_n, function(c)
      tags$th(as.character(c), style="text-align:center;font-size:10px;min-width:50px;")))
    body_rows <- lapply(row_lbl, function(r) {
      cells <- lapply(1:plate_cols_n, function(c) {
        wid <- paste0(r, c)
        val <- well_od_map[wid]
        is_ex <- wid %in% excluded
        style <- if (is_ex) "background:#f3f4f6;color:#9ca3af;text-decoration:line-through;" else ""
        tags$td(
          style = paste0("text-align:center;font-size:11px;padding:2px 4px;border:1px solid #e5e7eb;", style),
          if (!is.null(val) && !is.na(val)) sprintf("%.3f", as.numeric(val)) else ""
        )
      })
      tags$tr(tags$th(r, style="font-weight:bold;padding:2px 6px;font-size:11px;"), cells)
    })

    tags$div(style="overflow-x:auto;",
      tags$table(style="border-collapse:collapse;font-family:monospace;",
        tags$thead(header), tags$tbody(body_rows))
    )
  })

  # Exclude/Restore all wells observers
  observeEvent(input$resetExclusions, {
    excluded_wells(character(0))
    showNotification("All exclusions cleared.", type="message", duration=3)
  })
  observeEvent(input$restoreAllWells, {
    excluded_wells(character(0))
    showNotification("All exclusions cleared.", type="message", duration=3)
  })
  observeEvent(input$excludeSelectedWells, {
    showNotification("Click individual wells in the plate view to exclude them.", type="message", duration=4)
  })

  # Plate summary table
  output$plateSummaryTable <- renderTable({
    req(raw_data())
    data <- raw_data()
    type_col <- input$type_col

    if (is.null(type_col) || !type_col %in% colnames(data)) {
      return(data.frame(Category = "N/A", Count = 0L))
    }

    type_counts <- as.data.frame(table(data[[type_col]]), stringsAsFactors = FALSE)
    colnames(type_counts) <- c("Sample Type", "Count")
    type_counts <- type_counts[order(-type_counts$Count), ]
    type_counts
  })

  # Plate legend
  output$plateLegend <- renderUI({
    legend_items <- list(
      div(class = "plate-legend-item",
        tags$span(class = "plate-legend-swatch", style = "background-color: #dc3545;"), "Standard"),
      div(class = "plate-legend-item",
        tags$span(class = "plate-legend-swatch", style = "background-color: #0d6efd;"), "Sample"),
      div(class = "plate-legend-item",
        tags$span(class = "plate-legend-swatch", style = "background-color: #198754;"), "Control (NSB/B0/QC)"),
      div(class = "plate-legend-item",
        tags$span(class = "plate-legend-swatch", style = "background-color: #6c757d;"), "Blank"),
      div(class = "plate-legend-item",
        tags$span(class = "plate-legend-swatch", style = "background-color: #f8f9fa; border: 1px solid #ccc;"), "Empty")
    )
    div(
      h5("Legend"),
      legend_items,
      hr(),
      p(class = "text-muted small",
        "Hover over wells to see sample type. Wells are mapped from the Well ID column in your data.")
    )
  })

  # Reactive for filtered results based on toggle
  filtered_results <- reactive({
    # Check if results exist, if not return NULL
    if(is.null(results())) return(NULL)

    # Return the complete results object
    return(results())
  })
  
  # Dynamic plot height wrapper
  output$standardCurvePlotWrapper <- renderUI({
    height_px <- paste0(input$plotHeight %||% 600, "px")
    plotlyOutput("standardCurvePlot", height = height_px)
  })

  # ---- Helper: get unique sample type labels from current results ----
  get_sample_types <- function(res) {
    CTRL <- c("Standard", "NSB", "B0", "Blank", "QC_Low", "QC_Mid", "QC_High")
    if (is.null(res)) return(character(0))
    s <- res$samples
    if (is.null(s) || !("Type" %in% colnames(s))) return(character(0))
    unique(s$Type[!s$Type %in% CTRL & !is.na(s$Type)])
  }

  # ---- Block "Color by sample" when labels aren't unique: show modal + uncheck ----
  observeEvent(input$colorSamplesIndividually, {
    if (!isTRUE(input$colorSamplesIndividually)) return()
    res <- filtered_results()
    sample_types <- get_sample_types(res)
    if (length(sample_types) > 1) return()   # labels ARE unique — nothing to do

    updateCheckboxInput(session, "colorSamplesIndividually", value = FALSE)

    showModal(modalDialog(
      title = tagList(icon("triangle-exclamation"), " Sample labels required"),
      p("To color by sample, each sample group needs a unique name.",
        "Right now all samples share the same label."),
      p("Assign unique names first using one of:"),
      tags$ul(
        tags$li(tags$b("Auto-label:"), " go to Plate View and click Auto-label replicates"),
        tags$li(tags$b("Manual:"), " go to Plate View and click individual wells to assign names")
      ),
      p(class = "text-muted small",
        "After assigning names, hit Analyze again, then enable Color by sample."),
      footer = modalButton("Got it")
    ))
  })

  # ---- Auto-label button (shown in Plate View when sample labels aren't unique) ----
  output$autoLabelBtn <- renderUI({
    res <- filtered_results()
    sample_types <- get_sample_types(res)
    if (length(sample_types) > 1) return(NULL)   # already labeled — hide
    tags$div(style = "margin-top:4px;",
      actionButton("showAutoLabelModal", "Auto-label replicates",
                   icon = icon("wand-magic-sparkles"), class = "btn btn-info btn-sm"))
  })

  # ---- Auto-label modal ----
  observeEvent(input$showAutoLabelModal, {
    showModal(modalDialog(
      title = tagList(icon("wand-magic-sparkles"), " Auto-label replicates"),
      p("Groups adjacent sample wells and assigns sequential names."),
      fluidRow(
        column(6,
          radioButtons("autoLabel_direction", "Replicates run along:",
            choices = c("Rows (A1-A2-A3 …)" = "row",
                        "Columns (A1-B1-C1 …)" = "col"),
            selected = "row")
        ),
        column(6,
          numericInput("autoLabel_repCount", "Wells per sample (replicates):",
                       value = 2, min = 1, max = 12, step = 1)
        )
      ),
      textInput("autoLabel_prefix", "Sample name prefix:", value = "Sample_"),
      p(class = "text-muted small",
        "Example: prefix Sample_ with 2 replicates →",
        tags$em("Sample_1, Sample_1, Sample_2, Sample_2, …")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("applyAutoLabel", "Apply labels", class = "btn btn-primary")
      )
    ))
  })

  # ---- Apply auto-label ----
  observeEvent(input$applyAutoLabel, {
    removeModal()
    res <- filtered_results()
    if (is.null(res)) return()

    CTRL <- c("Standard", "NSB", "B0", "Blank", "QC_Low", "QC_Mid", "QC_High")
    samples <- res$samples
    if (is.null(samples) || !("Well" %in% colnames(samples))) {
      showNotification("No sample wells found in results.", type = "warning")
      return()
    }

    # Get sample wells
    sample_wells <- samples$Well[!samples$Type %in% CTRL & !is.na(samples$Well)]
    sample_wells <- unique(sample_wells)

    if (length(sample_wells) == 0) {
      showNotification("No sample wells found.", type = "warning")
      return()
    }

    # Parse well IDs into row letter + col number for sorting
    rows <- toupper(gsub("[0-9]+$", "", sample_wells))
    cols <- suppressWarnings(as.integer(gsub("^[A-Za-z]+", "", sample_wells)))
    row_idx <- match(rows, LETTERS)

    # Sort: row-wise = by row first, then col; col-wise = col first, then row
    direction <- input$autoLabel_direction %||% "row"
    if (direction == "row") {
      ord <- order(row_idx, cols)
    } else {
      ord <- order(cols, row_idx)
    }
    sorted_wells <- sample_wells[ord]

    # Group into replicates
    n_rep <- max(1L, as.integer(input$autoLabel_repCount %||% 2))
    prefix <- input$autoLabel_prefix %||% "Sample_"
    n_groups <- ceiling(length(sorted_wells) / n_rep)

    assigns <- well_assignments()
    for (i in seq_along(sorted_wells)) {
      group_num <- ceiling(i / n_rep)
      assigns[[sorted_wells[i]]] <- paste0(prefix, group_num)
    }
    well_assignments(assigns)

    showNotification(
      paste0("Labeled ", length(sorted_wells), " wells as ", n_groups,
             " samples (", n_rep, " replicates each). Click Analyze to update."),
      type = "message", duration = 6
    )
  })

  # sampleLabelWarning now unused (replaced by modal above) — keep as NULL
  output$sampleLabelWarning <- renderUI(NULL)

  # Standard curve plot output
  output$standardCurvePlot <- renderPlotly({
    req(filtered_results())

    # Get the currently selected model
    selected_model <- input$plotModelChoice

    # Set default if not found
    if(is.null(selected_model)) selected_model <- "combined"

    # Generate the plot based on filtered results
    create_standard_curve_plot(
      filtered_results(),
      selected_model,
      show_reliability_colors    = input$showReliabilityColors    %||% TRUE,
      flip_axes                  = input$flipAxes                  %||% FALSE,
      point_size                 = input$plotPointSize             %||% 3,
      line_size                  = input$plotLineSize              %||% 1.2,
      theme_name                 = input$plotTheme                 %||% "cowplot",
      font_family                = input$plotFont                  %||% "Arial",
      std_color                  = input$std_color                 %||% "#dc2626",
      sample_color               = input$sample_color              %||% "#2563eb",
      unreliable_color           = input$unreliable_color          %||% "#eab308",
      show_r2                    = input$showR2                    %||% TRUE,
      show_x_errbar              = input$showXErrbar               %||% FALSE,
      show_ci                    = input$showCI                    %||% FALSE,
      plot_title                 = input$plotTitle,
      x_title                    = input$plotXLabel,
      y_title                    = input$plotYLabel,
      color_samples_individually = input$colorSamplesIndividually  %||% FALSE
    )
  })
  
  # Table outputs
  output$standardsTable <- renderDT({
    req(filtered_results())

    # Format the standards data for display - use constants and check availability
    available_cols <- colnames(filtered_results()$standards)
    display_cols <- intersect(c(COL_WELL, COL_CONCENTRATION, COL_OD, COL_RESPONSE), available_cols)
    standards_data <- filtered_results()$standards %>%
      select(all_of(display_cols)) %>%
      arrange(!!sym(COL_CONCENTRATION))

    # Create the table
    datatable(standards_data,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'),
              rownames = FALSE)
  })

  output$samplesTable <- renderDT({
    req(filtered_results())
    model <- input$modelSelect

    # Get samples data
    samples_data <- filtered_results()$samples

    # Create the table with conditional formatting
    datatable(samples_data,
              options = list(pageLength = 20, scrollX = TRUE, dom = 'tp'),
              rownames = FALSE) %>%
      formatStyle(
        "Is_Unreliable",
        target = "row",
        backgroundColor = styleEqual(
          c(TRUE, FALSE),
          c("#FFDDDD", "white")
        )
      ) %>%
      formatStyle("Out_of_Range",
        backgroundColor = styleEqual(TRUE, "#fef9c3"))
  })

  output$sampleSummaryTable <- renderDT({
    req(filtered_results())

    # Get sample summary data
    sample_summary <- filtered_results()$sample_summary

    # Create the table
    datatable(sample_summary,
              options = list(pageLength = 20, scrollX = TRUE, dom = 'tp'),
              rownames = FALSE)
  })

  # ============ Quality Control Tab Outputs ============

  # Assay Limits Table (LOD/LOQ/ULOQ)
  output$assayLimitsTable <- renderTable({
    req(filtered_results())

    limits <- filtered_results()$assay_limits

    if (is.null(limits)) {
      return(data.frame(
        Metric = c("LOD", "LOQ", "ULOQ"),
        Value = c("N/A", "N/A", "N/A"),
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      Metric = c(
        if (identical(limits$lod$details$blank_source, "zero-standard")) "LOD (0 standard)" else "LOD",
        "LOQ", "ULOQ", "Dynamic Range"
      ),
      Value = c(
        if (!is.null(limits$lod$concentration) && !is.na(limits$lod$concentration))
          format(limits$lod$concentration, digits = 4, scientific = TRUE) else "N/A",
        if (!is.null(limits$loq$concentration) && !is.na(limits$loq$concentration))
          format(limits$loq$concentration, digits = 4, scientific = TRUE) else "N/A",
        if (!is.null(limits$uloq$concentration) && !is.na(limits$uloq$concentration))
          format(limits$uloq$concentration, digits = 4, scientific = TRUE) else "N/A",
        if (!is.null(limits$dynamic_range) && !is.na(limits$dynamic_range$fold_range))
          paste0(round(limits$dynamic_range$fold_range, 1), "-fold") else "N/A"
      ),
      stringsAsFactors = FALSE
    )
  })

  # QC Status Box
  output$qcStatusBox <- renderUI({
    req(filtered_results())

    qc <- filtered_results()$qc_summary

    if (is.null(qc)) {
      return(tags$div(
        class = "alert alert-info",
        tags$strong("QC Status: "), "Not calculated"
      ))
    }

    status_class <- switch(qc$overall_status,
      "PASS" = "alert-success",
      "WARNING" = "alert-warning",
      "FAIL" = "alert-danger",
      "alert-info"
    )

    tags$div(
      class = paste("alert", status_class),
      tags$h4(paste("Overall Status:", qc$overall_status)),
      if (!is.null(qc$issues) && length(qc$issues) > 0) {
        tags$ul(
          lapply(qc$issues, function(issue) tags$li(issue))
        )
      }
    )
  })

  # QC Summary Table
  output$qcSummaryTable <- renderTable({
    req(filtered_results())

    qc <- filtered_results()$qc_summary

    if (is.null(qc)) {
      return(data.frame(Metric = "N/A", Value = "N/A"))
    }

    data.frame(
      Metric = c("Best R²", "Standards Passing", "Analysis Time"),
      Value = c(
        if (!is.null(qc$best_r_squared)) round(qc$best_r_squared, 4) else "N/A",
        if (!is.null(qc$standards_passing)) paste0(qc$standards_passing, "/", qc$standards_total) else "N/A",
        format(qc$generated_at, "%Y-%m-%d %H:%M:%S")
      ),
      stringsAsFactors = FALSE
    )
  })

  # Model Performance Table
  output$modelPerformanceTable <- renderTable({
    req(filtered_results())

    comparison <- filtered_results()$model_comparison

    if (is.null(comparison) || nrow(comparison) == 0) {
      return(data.frame(Model = "N/A", `R²` = "N/A", AIC = "N/A"))
    }

    comparison %>%
      mutate(
        R_squared = round(R_squared, 4),
        RMSE = round(RMSE, 4),
        AIC = round(AIC, 2)
      ) %>%
      select(Model, R_squared, RMSE, AIC)
  })

  # Standards Accuracy Table
  output$standardsAccuracyTable <- renderDT({
    req(filtered_results())

    accuracy <- filtered_results()$standards_accuracy

    if (is.null(accuracy) || nrow(accuracy) == 0) {
      return(datatable(
        data.frame(Message = "Standards accuracy data not available"),
        options = list(dom = 't')
      ))
    }

    # Format for display
    display_data <- accuracy %>%
      mutate(
        Concentration = format(Concentration, digits = 4, scientific = TRUE),
        Mean_Response = round(Mean_Response, 4),
        CV_Pct = round(CV_Pct, 2),
        Mean_Back_Calc = format(Mean_Back_Calc, digits = 4, scientific = TRUE),
        Mean_Recovery_Pct = round(Mean_Recovery_Pct, 1),
        Mean_Percent_RE = round(Mean_Percent_RE, 1)
      ) %>%
      select(
        Concentration, N, Mean_Response, CV_Pct,
        Mean_Back_Calc, Mean_Recovery_Pct, Mean_Percent_RE, QC_Status
      )

    dt <- datatable(
      display_data,
      options = list(pageLength = 20, scrollX = TRUE, dom = 'tp'),
      rownames = FALSE
    ) %>%
      formatStyle(
        "QC_Status",
        backgroundColor = styleEqual(c("PASS", "FAIL"), c("#d4edda", "#f8d7da"))
      ) %>%
      formatStyle(
        "Mean_Recovery_Pct",
        backgroundColor = styleInterval(c(80, 120), c("#f8d7da", "#d4edda", "#f8d7da"))
      )
    # Highlight CV > 15%
    if ("CV_Pct" %in% colnames(display_data)) {
      dt <- dt %>%
        formatStyle("CV_Pct",
          backgroundColor = styleInterval(15, c("white", "#fef9c3")))
    }
    # Highlight %RE outside ±20%
    if ("Mean_Percent_RE" %in% colnames(display_data)) {
      dt <- dt %>%
        formatStyle("Mean_Percent_RE",
          backgroundColor = styleInterval(c(-20, 20), c("#fee2e2", "white", "#fee2e2")))
    }
    dt
  })

  # Data summary banner after upload
  output$dataSummaryInfo <- renderUI({
    req(raw_data())
    data <- raw_data()
    type_col <- input$type_col

    # Count sample types if the type column exists
    if (!is.null(type_col) && type_col %in% colnames(data)) {
      type_counts <- table(data[[type_col]])
      std_label <- input$std_label %||% "Standard"
      blank_label <- input$blank_label %||% "Blank"
      nsb_label <- input$nsb_label %||% "NSB"
      b0_label <- input$b0_label %||% "B0"

      n_standards <- sum(type_counts[names(type_counts) == std_label], na.rm = TRUE)
      n_blanks <- sum(type_counts[names(type_counts) == blank_label], na.rm = TRUE)
      n_controls <- sum(type_counts[names(type_counts) %in% c(nsb_label, b0_label)], na.rm = TRUE)
      n_samples <- nrow(data) - n_standards - n_blanks - n_controls

      div(class = "alert alert-info",
        icon("info-circle"),
        strong(paste(nrow(data), "rows loaded")), " — ",
        span(class = "label label-primary", paste(n_standards, "Standards")), " ",
        span(class = "label label-success", paste(n_samples, "Samples")), " ",
        if (n_controls > 0) span(class = "label label-info", paste(n_controls, "Controls")), " ",
        if (n_blanks > 0) span(class = "label label-default", paste(n_blanks, "Blanks"))
      )
    } else {
      div(class = "alert alert-info",
        icon("info-circle"),
        strong(paste(nrow(data), "rows,", ncol(data), "columns loaded"))
      )
    }
  })

  # Cleaned data preview
  output$cleanedPreview <- renderDT({
    req(raw_data())
    data <- raw_data()

    type_col <- input$type_col
    remove_unlabeled <- input$remove_unlabeled %||% TRUE

    if (remove_unlabeled && !is.null(type_col) && type_col %in% colnames(data)) {
      cleaned <- data %>% dplyr::filter(!is.na(.data[[type_col]]) & .data[[type_col]] != "")
      removed_count <- nrow(data) - nrow(cleaned)
    } else {
      cleaned <- data
      removed_count <- 0
    }

    DT::datatable(
      cleaned,
      options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'),
      rownames = FALSE,
      caption = if (removed_count > 0) {
        htmltools::tags$caption(
          style = "caption-side: top; color: #64748b; font-size: 0.85rem;",
          paste0(removed_count, " unlabeled row(s) removed. Showing ", nrow(cleaned), " of ", nrow(data), " rows.")
        )
      }
    )
  })

  # Analysis metadata banner in Results tab
  output$analysisMetadata <- renderUI({
    req(results())
    res <- results()

    assay_type <- res$assay_type %||% "Unknown"
    best_model <- res$best_model_name %||% "N/A"
    normalization <- if (isTRUE(res$skip_normalization)) "Skipped" else "B0/NSB"
    weighting <- res$weight_type %||% "none"

    div(class = "alert alert-success",
      icon("check-circle"),
      strong("Analysis Complete"), " — ",
      span(class = "label label-primary", paste("Assay:", assay_type)), " ",
      span(class = "label label-success", paste("Best Model:", best_model)), " ",
      span(class = "label label-info", paste("Normalization:", normalization)), " ",
      if (weighting != "none") span(class = "label label-warning", paste("Weight:", weighting))
    )
  })

  # New Analysis reset button
  observeEvent(input$resetAnalysis, {
    results(NULL)
    raw_data(NULL)
    parsed_plate(NULL)
    well_assignments(list())
    # Reset file input via JS (shinyjs not available)
    session$sendCustomMessage("resetFileInput", "file")
    updateSelectInput(session, "sheet", choices = character(0))
    updateSelectInput(session, "well_col", choices = character(0))
    updateSelectInput(session, "type_col", choices = character(0))
    updateSelectInput(session, "conc_col", choices = character(0))
    updateSelectInput(session, "od_col", choices = character(0))
    updateSelectInput(session, "od_corr_col", choices = character(0))
    updateNavbarPage(session, "mainNav", selected = "Upload & Analyze")
    showNotification("Analysis reset. Ready for new data.", type = "message", duration = 3)
  })

  # Sheet names are handled in the file upload observer

  # Model comparison table
  output$modelComparison <- renderTable({
    req(filtered_results())
    filtered_results()$model_comparison
  })

  # Model equations table
  output$modelEquations <- renderTable({
    req(filtered_results())

    # Create a data frame with equations
    equations <- filtered_results()$equations

    if(is.null(equations)) {
      return(data.frame(Model = character(0), Equation = character(0)))
    }

    data.frame(
      Model = names(equations),
      Equation = unlist(equations),
      stringsAsFactors = FALSE
    )
  })

  # Download handlers for individual files
  output$downloadSampleSummary <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_SampleSummary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(filtered_results())
      writexl::write_xlsx(filtered_results()$sample_summary, file)
    }
  )

  output$downloadSamples <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_Samples_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(filtered_results())
      writexl::write_xlsx(filtered_results()$samples, file)
    }
  )

  output$downloadStandards <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_Standards_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(filtered_results())
      writexl::write_xlsx(filtered_results()$standards, file)
    }
  )

  # Duplicate download handlers for Results tab quick download buttons
  output$downloadAllFiles2 <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix %||% "ELISA_Results", "_AllResults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(filtered_results())
      temp_dir <- tempdir()
      excel_file <- file.path(temp_dir, "ELISA_Results_AllData.xlsx")
      data_list <- list(
        "Standards" = filtered_results()$standards,
        "Samples" = filtered_results()$samples,
        "Sample Summary" = filtered_results()$sample_summary,
        "Model Comparison" = filtered_results()$model_comparison
      )
      if (!is.null(filtered_results()$standards_accuracy)) {
        data_list[["Standards Accuracy"]] <- filtered_results()$standards_accuracy
      }
      writexl::write_xlsx(data_list, excel_file)
      zip(file, files = excel_file, flags = "-j")
    },
    contentType = "application/zip"
  )

  output$downloadSampleSummary2 <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix %||% "ELISA_Results", "_SampleSummary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(filtered_results())
      writexl::write_xlsx(filtered_results()$sample_summary, file)
    }
  )

  output$downloadSamples2 <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix %||% "ELISA_Results", "_Samples_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(filtered_results())
      writexl::write_xlsx(filtered_results()$samples, file)
    }
  )

  output$downloadStandards2 <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix %||% "ELISA_Results", "_Standards_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(filtered_results())
      writexl::write_xlsx(filtered_results()$standards, file)
    }
  )

  # Helper function to create ggplot version for export
  create_export_plot <- function(results, model_choice, flip_axes = FALSE,
                               theme_name = "cowplot", font_family = "Arial",
                               std_color = "#dc2626", sample_color = "#2563eb",
                               unreliable_color = "#eab308", show_r2 = TRUE,
                               show_x_errbar = FALSE, show_ci = FALSE,
                               plot_title = NULL, x_title = NULL, y_title = NULL,
                               point_size = 3, line_size = 1.2,
                               color_samples_individually = FALSE) {
    create_standard_curve_plot(
      results, model_choice,
      flip_axes                  = flip_axes,
      point_size                 = point_size,
      line_size                  = line_size,
      theme_name                 = theme_name,
      font_family                = font_family,
      std_color                  = std_color,
      sample_color               = sample_color,
      unreliable_color           = unreliable_color,
      show_r2                    = show_r2,
      show_x_errbar              = show_x_errbar,
      show_ci                    = show_ci,
      plot_title                 = plot_title,
      x_title                    = x_title,
      y_title                    = y_title,
      color_samples_individually = color_samples_individually,
      return_ggplot              = TRUE
    )
  }

  # Handlers for plot downloads (using ggplot2 for reliable PNG export)
  output$downloadPlotsCombined <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_CombinedPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "combined", flip_axes = input$flipAxes %||% FALSE,
        theme_name = input$plotTheme %||% "cowplot", font_family = input$plotFont %||% "Arial",
        std_color = input$std_color %||% "#dc2626", sample_color = input$sample_color %||% "#2563eb",
        unreliable_color = input$unreliable_color %||% "#eab308",
        show_r2 = input$showR2 %||% TRUE, show_x_errbar = input$showXErrbar %||% FALSE,
        plot_title = input$plotTitle, x_title = input$plotXLabel, y_title = input$plotYLabel,
        point_size = input$plotPointSize %||% 3, line_size = input$plotLineSize %||% 1.2,
        color_samples_individually = input$colorSamplesIndividually %||% FALSE)
      ggsave(file, plot = p, width = input$plotDlWidth %||% 8, height = input$plotDlHeight %||% 6, units = "in", dpi = 300, bg = "white")
    }
  )

  output$downloadPlots4PL <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_4PLPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "4pl", flip_axes = input$flipAxes %||% FALSE,
        theme_name = input$plotTheme %||% "cowplot", font_family = input$plotFont %||% "Arial",
        std_color = input$std_color %||% "#dc2626", sample_color = input$sample_color %||% "#2563eb",
        unreliable_color = input$unreliable_color %||% "#eab308",
        show_r2 = input$showR2 %||% TRUE, show_x_errbar = input$showXErrbar %||% FALSE,
        plot_title = input$plotTitle, x_title = input$plotXLabel, y_title = input$plotYLabel,
        point_size = input$plotPointSize %||% 3, line_size = input$plotLineSize %||% 1.2,
        color_samples_individually = input$colorSamplesIndividually %||% FALSE)
      ggsave(file, plot = p, width = input$plotDlWidth %||% 8, height = input$plotDlHeight %||% 6, units = "in", dpi = 300, bg = "white")
    }
  )

  output$downloadPlots5PL <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_5PLPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "5pl", flip_axes = input$flipAxes %||% FALSE,
        theme_name = input$plotTheme %||% "cowplot", font_family = input$plotFont %||% "Arial",
        std_color = input$std_color %||% "#dc2626", sample_color = input$sample_color %||% "#2563eb",
        unreliable_color = input$unreliable_color %||% "#eab308",
        show_r2 = input$showR2 %||% TRUE, show_x_errbar = input$showXErrbar %||% FALSE,
        plot_title = input$plotTitle, x_title = input$plotXLabel, y_title = input$plotYLabel,
        point_size = input$plotPointSize %||% 3, line_size = input$plotLineSize %||% 1.2,
        color_samples_individually = input$colorSamplesIndividually %||% FALSE)
      ggsave(file, plot = p, width = input$plotDlWidth %||% 8, height = input$plotDlHeight %||% 6, units = "in", dpi = 300, bg = "white")
    }
  )

  output$downloadPlotsLinear <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_LinearPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "linear", flip_axes = input$flipAxes %||% FALSE,
        theme_name = input$plotTheme %||% "cowplot", font_family = input$plotFont %||% "Arial",
        std_color = input$std_color %||% "#dc2626", sample_color = input$sample_color %||% "#2563eb",
        unreliable_color = input$unreliable_color %||% "#eab308",
        show_r2 = input$showR2 %||% TRUE, show_x_errbar = input$showXErrbar %||% FALSE,
        plot_title = input$plotTitle, x_title = input$plotXLabel, y_title = input$plotYLabel,
        point_size = input$plotPointSize %||% 3, line_size = input$plotLineSize %||% 1.2,
        color_samples_individually = input$colorSamplesIndividually %||% FALSE)
      ggsave(file, plot = p, width = input$plotDlWidth %||% 8, height = input$plotDlHeight %||% 6, units = "in", dpi = 300, bg = "white")
    }
  )

  # Combined download handler (ZIP file)
  output$downloadAllFiles <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_AllResults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(filtered_results())

      # Create temp directory
      temp_dir <- tempdir()

      # Create files to include in the ZIP
      excel_file <- file.path(temp_dir, "ELISA_Results_AllData.xlsx")
      combined_plot <- file.path(temp_dir, "ELISA_CombinedPlot.png")
      fourpl_plot <- file.path(temp_dir, "ELISA_4PLPlot.png")
      fivepl_plot <- file.path(temp_dir, "ELISA_5PLPlot.png")
      linear_plot <- file.path(temp_dir, "ELISA_LinearPlot.png")
      qc_report <- file.path(temp_dir, "ELISA_QC_Report.xlsx")

      # Create Excel file with all data
      data_list <- list(
        "Standards" = filtered_results()$standards,
        "Samples" = filtered_results()$samples,
        "Sample Summary" = filtered_results()$sample_summary,
        "Model Comparison" = filtered_results()$model_comparison
      )

      # Add accuracy data if available
      if (!is.null(filtered_results()$standards_accuracy)) {
        data_list[["Standards Accuracy"]] <- filtered_results()$standards_accuracy
      }

      writexl::write_xlsx(data_list, excel_file)

      # Create QC report
      tryCatch({
        qc_data <- list()

        # Assay limits
        if (!is.null(filtered_results()$assay_limits)) {
          limits <- filtered_results()$assay_limits
          qc_data[["Assay Limits"]] <- data.frame(
            Metric = c("LOD", "LOQ", "ULOQ"),
            Concentration = c(
              limits$lod$concentration %||% NA,
              limits$loq$concentration %||% NA,
              limits$uloq$concentration %||% NA
            )
          )
        }

        # QC Summary
        if (!is.null(filtered_results()$qc_summary)) {
          qc <- filtered_results()$qc_summary
          qc_data[["QC Summary"]] <- data.frame(
            Metric = c("Overall Status", "Best R²", "Standards Passing"),
            Value = c(
              qc$overall_status %||% "N/A",
              as.character(qc$best_r_squared %||% "N/A"),
              paste0(qc$standards_passing %||% "N/A", "/", qc$standards_total %||% "N/A")
            )
          )
        }

        if (length(qc_data) > 0) {
          writexl::write_xlsx(qc_data, qc_report)
        }
      }, error = function(e) {
        message("Error creating QC report: ", e$message)
      })

      # Create plot files using ggplot2 for reliable export
      tryCatch({
        flip <- input$flipAxes %||% FALSE
        p_combined <- create_export_plot(filtered_results(), "combined", flip_axes = flip)
        ggsave(combined_plot, plot = p_combined, width = input$plotDlWidth %||% 8, height = input$plotDlHeight %||% 6, units = "in", dpi = 300, bg = "white")

        p_4pl <- create_export_plot(filtered_results(), "4pl", flip_axes = flip)
        ggsave(fourpl_plot, plot = p_4pl, width = input$plotDlWidth %||% 8, height = input$plotDlHeight %||% 6, units = "in", dpi = 300, bg = "white")

        p_5pl <- create_export_plot(filtered_results(), "5pl", flip_axes = flip)
        ggsave(fivepl_plot, plot = p_5pl, width = input$plotDlWidth %||% 8, height = input$plotDlHeight %||% 6, units = "in", dpi = 300, bg = "white")

        p_linear <- create_export_plot(filtered_results(), "linear", flip_axes = flip)
        ggsave(linear_plot, plot = p_linear, width = input$plotDlWidth %||% 8, height = input$plotDlHeight %||% 6, units = "in", dpi = 300, bg = "white")
      }, error = function(e) {
        message("Error generating plot: ", e$message)
      })

      # Create ZIP file with all files
      files_to_zip <- c(excel_file, combined_plot, fourpl_plot, fivepl_plot, linear_plot, qc_report)
      files_to_zip <- files_to_zip[file.exists(files_to_zip)]

      zip(file, files = files_to_zip, flags = "-j")
    },
    contentType = "application/zip"
  )

  # ============ Paste Data Observers ============

  observeEvent(input$process_paste, {
    req(input$paste_data_text)
    text <- input$paste_data_text
    if (trimws(text) == "") {
      showNotification("Nothing to parse — paste your data first.", type = "warning")
      return()
    }

    result <- tryCatch(parse_pasted_data(text), error = function(e) {
      list(format = "error", message = e$message)
    })

    if (result$format == "error") {
      showNotification(paste("Parse error:", result$message), type = "error", duration = 8)
      return()
    }

    raw_data(result$data)
    if (result$format == "plate") {
      parsed_plate(list(plate_corrected = result$plate_matrix))
    }

    # Update column dropdowns to match new data
    update_column_dropdowns(session, colnames(result$data))

    # Clear old well assignments
    well_assignments(list())

    showNotification(result$message, type = "message", duration = 5)
    updateNavbarPage(session, "mainNav", selected = "Plate View")
  })

  observeEvent(input$clear_paste, {
    updateTextAreaInput(session, "paste_data_text", value = "")
  })

  output$pasteParseStatus <- renderUI({
    if (is.null(raw_data())) return(NULL)
    tags$div(class = "alert alert-success", style = "margin-top:8px;",
      icon("check"), " Data loaded: ", nrow(raw_data()), " rows")
  })

  # ============ Well Click / Assignment Modal ============

  observeEvent(input$clicked_well, {
    req(input$clicked_well)
    well_id <- input$clicked_well
    assigns <- well_assignments()

    # Determine current type from data or manual assignment
    data <- raw_data()
    well_col_name <- input$well_col
    type_col_name <- input$type_col
    current_type <- assigns[[well_id]] %||% {
      if (!is.null(data) && !is.null(well_col_name) && well_col_name %in% colnames(data) &&
          !is.null(type_col_name) && type_col_name %in% colnames(data)) {
        row_idx <- which(as.character(data[[well_col_name]]) == well_id)
        if (length(row_idx) > 0) as.character(data[[type_col_name]][row_idx[1]]) else ""
      } else ""
    }

    # Get OD value for display
    od_col_name <- input$od_col
    od_val <- tryCatch({
      if (!is.null(data) && !is.null(od_col_name) && !is.null(well_col_name) &&
          od_col_name %in% colnames(data) && well_col_name %in% colnames(data)) {
        row_idx <- which(as.character(data[[well_col_name]]) == well_id)
        if (length(row_idx) > 0) data[[od_col_name]][row_idx[1]] else NA
      } else if (!is.null(parsed_plate())) {
        r_idx <- which(LETTERS[1:8] == substr(well_id, 1, 1))
        c_idx <- suppressWarnings(as.integer(substr(well_id, 2, nchar(well_id))))
        if (length(r_idx) > 0 && !is.na(c_idx) && c_idx >= 1 && c_idx <= 12)
          parsed_plate()$plate_corrected[r_idx, c_idx] else NA
      } else NA
    }, error = function(e) NA)

    od_display <- if (!is.null(od_val) && !is.na(od_val %||% NA)) sprintf("%.4f", as.numeric(od_val)) else "—"

    showModal(modalDialog(
      title = paste("Well", well_id),
      p(tags$b("OD value: "), od_display),
      selectizeInput("modal_well_type", "Assign Sample Type:",
                  choices = c("Standard", "Sample", "Blank", "NSB", "B0",
                              "QC_Low", "QC_Mid", "QC_High", "Empty"),
                  selected = if (!is.null(current_type) && current_type != "") current_type else "Sample",
                  options = list(create = TRUE, placeholder = "Type a custom name or choose...")),
      tags$small(class = "text-muted", "Tip: type a custom sample name (e.g. ", tags$em("Patient_01"), ") and press Enter to create it."),
      conditionalPanel(
        condition = "input.modal_well_type == 'Standard'",
        textInput("modal_well_conc", "Concentration:", placeholder = "e.g. 62.5")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modal_clear_well", "Clear Assignment", class = "btn btn-default"),
        actionButton("modal_exclude_well",
                     if (well_id %in% excluded_wells()) "Restore" else "Exclude",
                     class = if (well_id %in% excluded_wells()) "btn btn-success" else "btn btn-warning"),
        actionButton("modal_assign_well", "Assign", class = "btn btn-primary")
      )
    ))
  })

  observeEvent(input$modal_assign_well, {
    req(input$clicked_well, input$modal_well_type)
    well_id <- input$clicked_well
    assigns <- well_assignments()
    assigns[[well_id]] <- input$modal_well_type
    if (!is.null(input$modal_well_conc) && nchar(trimws(input$modal_well_conc %||% "")) > 0) {
      assigns[[paste0(well_id, "_conc")]] <- trimws(input$modal_well_conc)
    }
    well_assignments(assigns)
    removeModal()
    showNotification(paste0(well_id, " assigned as ", input$modal_well_type), type = "message", duration = 2)
  })

  observeEvent(input$modal_exclude_well, {
    req(input$clicked_well)
    well_id <- input$clicked_well
    ex <- excluded_wells()
    if (well_id %in% ex) {
      excluded_wells(setdiff(ex, well_id))
      showNotification(paste(well_id, "restored."), type = "message", duration = 2)
    } else {
      excluded_wells(c(ex, well_id))
      showNotification(paste(well_id, "excluded."), type = "message", duration = 2)
    }
    removeModal()
  })

  observeEvent(input$modal_clear_well, {
    req(input$clicked_well)
    assigns <- well_assignments()
    assigns[[input$clicked_well]] <- NULL
    assigns[[paste0(input$clicked_well, "_conc")]] <- NULL
    well_assignments(assigns)
    removeModal()
  })

  # ============ Format Conversion & Copy Plate ============

  # Observe raw_data changes to update the hidden clipboard textarea
  observe({
    if (is.null(raw_data())) return(NULL)
    data <- raw_data()
    well_col <- input$well_col
    type_col <- input$type_col
    od_col   <- input$od_col
    assigns  <- well_assignments()

    plate_mat <- matrix("", 8, 12)
    if (!is.null(well_col) && well_col %in% colnames(data)) {
      wells <- as.character(data[[well_col]])
      types <- if (!is.null(type_col) && type_col %in% colnames(data)) as.character(data[[type_col]]) else rep("", length(wells))
      ods   <- if (!is.null(od_col)   && od_col   %in% colnames(data)) as.character(data[[od_col]])   else rep("", length(wells))
      for (i in seq_along(wells)) {
        w     <- wells[i]
        r_idx <- which(LETTERS[1:8] == substr(w, 1, 1))
        c_idx <- suppressWarnings(as.integer(substr(w, 2, nchar(w))))
        if (length(r_idx) > 0 && !is.na(c_idx) && c_idx >= 1 && c_idx <= 12) {
          type_display <- if (!is.null(assigns[[w]])) assigns[[w]] else types[i]
          od_display   <- ods[i]
          plate_mat[r_idx, c_idx] <- if (!is.na(od_display) && od_display != "") od_display else type_display
        }
      }
    } else if (!is.null(parsed_plate())) {
      pm <- parsed_plate()$plate_corrected
      for (r in 1:8) for (c in 1:12) {
        v <- pm[r, c]
        plate_mat[r, c] <- if (!is.na(v %||% NA)) as.character(v) else ""
      }
    }

    header_row <- paste(c("", as.character(1:12)), collapse = "\t")
    data_rows  <- sapply(1:8, function(r) paste(c(LETTERS[r], plate_mat[r, ]), collapse = "\t"))
    tsv_text   <- paste(c(header_row, data_rows), collapse = "\n")
    session$sendCustomMessage("updatePlateText", tsv_text)
  })

  output$plateLongTableOutput <- renderUI({
    req(raw_data())
    tagList(
      hr(),
      h5("Long Format Table"),
      DT::dataTableOutput("plateLongTable")
    )
  })

  output$plateLongTable <- DT::renderDataTable({
    req(raw_data())
    data <- raw_data()
    # Apply well_assignments overrides to type column
    assigns  <- well_assignments()
    well_col <- input$well_col
    type_col <- input$type_col
    if (!is.null(well_col) && well_col %in% colnames(data) &&
        !is.null(type_col) && type_col %in% colnames(data)) {
      for (w in names(assigns)) {
        if (!grepl("_conc$", w)) {
          idx <- which(as.character(data[[well_col]]) == w)
          if (length(idx) > 0) data[[type_col]][idx] <- assigns[[w]]
        }
      }
    }
    DT::datatable(data, options = list(pageLength = 20, scrollX = TRUE, dom = "tp"), rownames = FALSE)
  })

  output$download_long_format <- downloadHandler(
    filename = function() paste0("ELISA_LongFormat_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      req(raw_data())
      writexl::write_xlsx(raw_data(), file)
    }
  )

  output$download_plate_format <- downloadHandler(
    filename = function() paste0("ELISA_PlateGrid_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      req(raw_data())
      data     <- raw_data()
      well_col <- input$well_col %||% "Well ID"
      od_col   <- input$od_col   %||% "OD"

      plate_df <- data.frame(matrix("", 8, 13))
      colnames(plate_df) <- c("Row", as.character(1:12))
      plate_df$Row <- LETTERS[1:8]

      if (well_col %in% colnames(data) && od_col %in% colnames(data)) {
        for (i in seq_len(nrow(data))) {
          w     <- as.character(data[[well_col]][i])
          r_idx <- which(LETTERS[1:8] == substr(w, 1, 1))
          c_idx <- suppressWarnings(as.integer(substr(w, 2, nchar(w))))
          if (length(r_idx) > 0 && !is.na(c_idx) && c_idx >= 1 && c_idx <= 12) {
            plate_df[r_idx, c_idx + 1] <- as.character(data[[od_col]][i])
          }
        }
      }
      writexl::write_xlsx(plate_df, file)
    }
  )

}

# Run the Shiny application
shinyApp(ui, server)