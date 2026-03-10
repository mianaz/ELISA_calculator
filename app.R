# Main Shiny application file for ELISA Standard Curve Calculator (Beta)
library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(readr)
library(writexl)
library(drc)
library(ggplot2)
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

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store analysis results
  results <- reactiveVal(NULL)
  raw_data <- reactiveVal(NULL)
  parsed_plate <- reactiveVal(NULL)  # Stores parsed plate grid data from plate reader files
  exclusion_audit_trail <- reactiveVal(list(entries = list(), created_at = Sys.time()))

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
          od_corr_col = if(input$od_corr_col == "None") input$od_col else input$od_corr_col,
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
          calculate_limits = input$calculate_limits %||% TRUE
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
        # Display error to user
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
          calculate_limits = input$calculate_limits %||% TRUE
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

  # Generate 96-well plate visualization from uploaded data
  output$plateViewOutput <- renderUI({
    req(raw_data())
    data <- raw_data()

    well_col <- input$well_col
    type_col <- input$type_col

    if (is.null(well_col) || !well_col %in% colnames(data)) {
      return(p(class = "text-muted", "Well ID column not found. Please check column mapping."))
    }

    # Parse well positions (e.g., A1 -> row=A, col=1)
    wells <- as.character(data[[well_col]])
    types <- if (!is.null(type_col) && type_col %in% colnames(data)) {
      as.character(data[[type_col]])
    } else {
      rep("Unknown", length(wells))
    }

    # Build a well->type lookup
    well_type_map <- setNames(types, wells)

    # Standard labels
    std_label <- input$std_label %||% "Standard"
    blank_label <- input$blank_label %||% "Blank"
    nsb_label <- input$nsb_label %||% "NSB"
    b0_label <- input$b0_label %||% "B0"

    # Classify well type for coloring
    classify_well <- function(type) {
      if (is.na(type) || type == "") return("empty")
      if (type == std_label) return("std")
      if (type == blank_label) return("blank")
      if (type %in% c(nsb_label, b0_label, "QC_Low", "QC_Mid", "QC_High")) return("control")
      return("sample")
    }

    row_labels <- LETTERS[1:8]
    col_labels <- 1:12

    # Build plate HTML
    plate_rows <- list()

    # Column headers
    header_items <- list(tags$span(class = "plate-row-label", ""))
    for (col in col_labels) {
      header_items <- c(header_items, list(
        tags$span(class = "plate-col-header", as.character(col))
      ))
    }
    plate_rows <- c(plate_rows, list(div(header_items)))

    # Well rows
    for (row in row_labels) {
      row_items <- list(tags$span(class = "plate-row-label", row))
      for (col in col_labels) {
        well_id <- paste0(row, col)
        type <- well_type_map[well_id]
        well_class <- if (!is.na(type)) classify_well(type) else "empty"
        tooltip <- if (!is.na(type) && type != "") {
          paste0(well_id, ": ", type)
        } else {
          paste0(well_id, ": Empty")
        }

        row_items <- c(row_items, list(
          tags$span(
            class = paste("plate-well", well_class),
            title = tooltip,
            substr(well_id, 2, nchar(well_id))
          )
        ))
      }
      plate_rows <- c(plate_rows, list(div(row_items)))
    }

    div(class = "plate-container", plate_rows)
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
      show_reliability_colors = input$showReliabilityColors %||% TRUE,
      flip_axes = input$flipAxes %||% FALSE
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
      )
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
      Metric = c("LOD", "LOQ", "ULOQ", "Dynamic Range"),
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

    datatable(
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
  create_export_plot <- function(results, model_choice, flip_axes = FALSE) {
    # Extract needed data elements
    std_summary <- results$std_summary
    samples_data <- results$samples
    log_transform <- results$log_transform
    std_range <- results$std_range
    y_axis_label <- if(results$skip_normalization) "OD (corrected)" else "Normalized OD (B/B0)"
    best_model <- results$best_model_name

    # Generate curve data
    min_conc <- min(std_summary$Concentration[std_summary$Concentration > 0], na.rm = TRUE) / 10
    max_conc <- max(std_summary$Concentration, na.rm = TRUE) * 10
    x_points <- exp(seq(log(min_conc), log(max_conc), length.out = 100))

    # Prepare standard points and curve data as x/y columns
    std_pts <- data.frame(
      x = std_summary$Concentration,
      y = std_summary$Mean_Response,
      y_lo = std_summary$Mean_Response - std_summary$SD_Response,
      y_hi = std_summary$Mean_Response + std_summary$SD_Response
    )

    # Build curve data frames
    curve_frames <- list()
    if (model_choice == "combined" || model_choice == "4pl") {
      if (!is.null(results$models[["4PL"]]) && results$models[["4PL"]]$is_valid()) {
        y_4pl <- tryCatch(results$models[["4PL"]]$predict_response(x_points), error = function(e) NULL)
        if (!is.null(y_4pl)) curve_frames$f4pl <- data.frame(x = x_points, y = y_4pl, model = "4PL")
      }
    }
    if (model_choice == "combined" || model_choice == "5pl") {
      if (!is.null(results$models[["5PL"]]) && results$models[["5PL"]]$is_valid()) {
        y_5pl <- tryCatch(results$models[["5PL"]]$predict_response(x_points), error = function(e) NULL)
        if (!is.null(y_5pl)) curve_frames$f5pl <- data.frame(x = x_points, y = y_5pl, model = "5PL")
      }
    }
    if (model_choice == "combined" || model_choice == "linear") {
      if (!is.null(results$models[["Linear"]]) && results$models[["Linear"]]$is_valid()) {
        y_lin <- tryCatch(results$models[["Linear"]]$predict_response(x_points), error = function(e) NULL)
        if (!is.null(y_lin)) curve_frames$flin <- data.frame(x = x_points, y = y_lin, model = "Linear")
      }
    }

    # Process sample data for plotting
    min_conc_plot <- min(std_summary$Concentration[std_summary$Concentration > 0], na.rm = TRUE)
    samples_summary <- samples_data %>%
      dplyr::filter(!is.na(Response)) %>%
      dplyr::group_by(Type) %>%
      dplyr::summarize(
        Mean_Response = mean(Response, na.rm = TRUE),
        SD_Response = sd(Response, na.rm = TRUE),
        Mean_Concentration = mean(Capped_Concentration, na.rm = TRUE),
        SD_Concentration = sd(Capped_Concentration, na.rm = TRUE),
        Has_Out_of_Range = any(Out_of_Range, na.rm = TRUE),
        Has_Uncalculatable = any(Model_Used == "Uncalculatable", na.rm = TRUE),
        Is_Unreliable = any(Is_Unreliable, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(!Type %in% c("NSB", "B0", "Standard", "Blank"))

    if (nrow(samples_summary) > 0) {
      samp_pts <- data.frame(
        x = samples_summary$Mean_Concentration,
        y = samples_summary$Mean_Response,
        x_lo = pmax(samples_summary$Mean_Concentration - samples_summary$SD_Concentration, min_conc_plot / 10),
        x_hi = samples_summary$Mean_Concentration + samples_summary$SD_Concentration,
        y_lo = samples_summary$Mean_Response - samples_summary$SD_Response,
        y_hi = samples_summary$Mean_Response + samples_summary$SD_Response,
        Is_Unreliable = samples_summary$Is_Unreliable,
        Type = samples_summary$Type
      )
    } else {
      samp_pts <- NULL
    }

    # Flip axes if requested
    if (flip_axes) {
      std_pts <- data.frame(x = std_pts$y, y = std_pts$x, y_lo = NA_real_, y_hi = NA_real_)
      for (nm in names(curve_frames)) {
        cf <- curve_frames[[nm]]
        curve_frames[[nm]] <- data.frame(x = cf$y, y = cf$x, model = cf$model)
      }
      if (!is.null(samp_pts)) {
        samp_pts <- data.frame(
          x = samp_pts$y, y = samp_pts$x,
          x_lo = samp_pts$y_lo, x_hi = samp_pts$y_hi,
          y_lo = samp_pts$x_lo, y_hi = samp_pts$x_hi,
          Is_Unreliable = samp_pts$Is_Unreliable,
          Type = samp_pts$Type
        )
      }
    }

    # Build warning captions
    warning_messages <- character(0)
    out_of_range_samples <- samples_data %>%
      dplyr::filter(Out_of_Range == TRUE) %>%
      dplyr::pull(Type) %>%
      unique()
    if (length(out_of_range_samples) > 0) {
      warning_messages <- c(warning_messages,
        paste("Note: Outside standard curve range:",
              paste(out_of_range_samples, collapse = ", ")))
    }
    uncalculatable_samples <- samples_data %>%
      dplyr::filter(Model_Used == "Uncalculatable") %>%
      dplyr::pull(Type) %>%
      unique()
    if (length(uncalculatable_samples) > 0) {
      warning_messages <- c(warning_messages,
        paste("Warning: Estimated values:",
              paste(uncalculatable_samples, collapse = ", ")))
    }
    caption <- if (length(warning_messages) > 0) paste(warning_messages, collapse = "\n") else NULL

    # Build the plot
    p <- ggplot() +
      geom_point(data = std_pts, aes(x = x, y = y), color = "red", size = 3, shape = 16) +
      geom_errorbar(data = std_pts, aes(x = x, ymin = y_lo, ymax = y_hi),
                    width = 0.1, color = "red", alpha = 0.5)

    # Add model curves
    curve_colors <- c("4PL" = "blue", "5PL" = "purple", "Linear" = "green")
    curve_linetypes <- c("4PL" = "solid", "5PL" = "dashed", "Linear" = "solid")
    for (cf in curve_frames) {
      p <- p + geom_line(data = cf, aes(x = x, y = y),
                         color = curve_colors[cf$model[1]],
                         linetype = curve_linetypes[cf$model[1]],
                         linewidth = 1)
    }

    # Add sample points if any exist
    if (!is.null(samp_pts) && nrow(samp_pts) > 0) {
      p <- p +
        geom_point(data = samp_pts,
                   aes(x = x, y = y, color = Is_Unreliable, shape = Type), size = 3) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                           name = "Unreliable Value") +
        geom_errorbarh(data = samp_pts,
                       aes(y = y, xmin = x_lo, xmax = x_hi),
                       height = 0.01, alpha = 0.5, color = "gray50") +
        geom_errorbar(data = samp_pts,
                      aes(x = x, ymin = y_lo, ymax = y_hi),
                      width = 0.1, alpha = 0.5, color = "gray50")
    }

    # Add formatting — axis labels and log scale depend on flip_axes
    if (flip_axes) {
      p <- p +
        scale_y_log10(labels = scales::label_number(), breaks = scales::breaks_log(n = 8)) +
        geom_hline(yintercept = min(std_summary$Concentration, na.rm = TRUE),
                   linetype = "dashed", alpha = 0.3) +
        geom_hline(yintercept = max(std_summary$Concentration, na.rm = TRUE),
                   linetype = "dashed", alpha = 0.3) +
        labs(x = y_axis_label, y = "Concentration (log scale)",
             title = paste0("ELISA Standard Curve - ", toupper(model_choice)),
             subtitle = paste("Best model:", best_model), caption = caption)
    } else {
      p <- p +
        scale_x_log10(labels = scales::label_number(), breaks = scales::breaks_log(n = 8)) +
        geom_vline(xintercept = min(std_summary$Concentration, na.rm = TRUE),
                   linetype = "dashed", alpha = 0.3) +
        geom_vline(xintercept = max(std_summary$Concentration, na.rm = TRUE),
                   linetype = "dashed", alpha = 0.3) +
        labs(x = "Concentration (log scale)", y = y_axis_label,
             title = paste0("ELISA Standard Curve - ", toupper(model_choice)),
             subtitle = paste("Best model:", best_model), caption = caption)
    }

    p <- p +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0, color = "red", size = 10),
        legend.position = "bottom"
      )

    return(p)
  }

  # Handlers for plot downloads (using ggplot2 for reliable PNG export)
  output$downloadPlotsCombined <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_CombinedPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "combined", flip_axes = input$flipAxes %||% FALSE)
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150, bg = "white")
    }
  )

  output$downloadPlots4PL <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_4PLPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "4pl", flip_axes = input$flipAxes %||% FALSE)
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150, bg = "white")
    }
  )

  output$downloadPlots5PL <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_5PLPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "5pl", flip_axes = input$flipAxes %||% FALSE)
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150, bg = "white")
    }
  )

  output$downloadPlotsLinear <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_LinearPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "linear", flip_axes = input$flipAxes %||% FALSE)
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150, bg = "white")
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
        ggsave(combined_plot, plot = p_combined, width = 12, height = 8, dpi = 150, bg = "white")

        p_4pl <- create_export_plot(filtered_results(), "4pl", flip_axes = flip)
        ggsave(fourpl_plot, plot = p_4pl, width = 12, height = 8, dpi = 150, bg = "white")

        p_5pl <- create_export_plot(filtered_results(), "5pl", flip_axes = flip)
        ggsave(fivepl_plot, plot = p_5pl, width = 12, height = 8, dpi = 150, bg = "white")

        p_linear <- create_export_plot(filtered_results(), "linear", flip_axes = flip)
        ggsave(linear_plot, plot = p_linear, width = 12, height = 8, dpi = 150, bg = "white")
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
}

# Run the Shiny application
shinyApp(ui, server)