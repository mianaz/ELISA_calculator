# Plate Reader Format Parser
# Converts plate-format (8x12 grid) data into columnar format for ELISA analysis

#' Parse a plate reader grid into columnar format
#'
#' @param plate_data Matrix or data frame with 8 rows x 12 columns of OD values
#' @param plate_layout Data frame mapping well positions to sample info
#'   Must have columns: Row (A-H), Col (1-12), Sample_Type, Concentration (optional)
#' @return Data frame in columnar format with Well ID, Sample Type, Concentration, OD
parse_plate_grid <- function(plate_data, plate_layout) {
  if (!is.matrix(plate_data)) {
    plate_data <- as.matrix(plate_data)
  }

  stopifnot(nrow(plate_data) == 8, ncol(plate_data) == 12)

  results <- data.frame(
    `Well ID` = character(),
    `Sample Type` = character(),
    Concentration = character(),
    OD = numeric(),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(plate_layout))) {
    r <- match(toupper(plate_layout$Row[i]), LETTERS[1:8])
    c <- as.integer(plate_layout$Col[i])

    if (is.na(r) || is.na(c) || r < 1 || r > 8 || c < 1 || c > 12) next

    well_id <- paste0(LETTERS[r], c)
    od_val <- plate_data[r, c]
    conc <- if ("Concentration" %in% colnames(plate_layout)) {
      as.character(plate_layout$Concentration[i])
    } else {
      ""
    }
    conc[is.na(conc)] <- ""

    results <- rbind(results, data.frame(
      `Well ID` = well_id,
      `Sample Type` = plate_layout$Sample_Type[i],
      Concentration = conc,
      OD = od_val,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }

  return(results)
}

#' Detect plate reader format from file content
#'
#' @param file_path Path to the file
#' @param file_name Original file name
#' @return List with format type and metadata
detect_plate_format <- function(file_path, file_name) {
  ext <- tolower(tools::file_ext(file_name))

  result <- list(
    format = "unknown",
    instrument = "unknown",
    wavelengths = NULL,
    plate_size = 96,
    metadata = list()
  )

  if (ext %in% c("xlsx", "xls")) {
    tryCatch({
      raw <- readxl::read_excel(file_path, sheet = 1, col_names = FALSE, .name_repair = "minimal")
      text_content <- paste(as.character(unlist(raw[1:min(20, nrow(raw)), ])), collapse = " ")

      # BioTek Synergy detection
      if (grepl("Synergy", text_content, ignore.case = TRUE) ||
          grepl("BioTek", text_content, ignore.case = TRUE)) {
        result$format <- "biotek"
        result$instrument <- "BioTek Synergy"
      }

      # BMG LABTECH detection
      if (grepl("BMG", text_content, ignore.case = TRUE) ||
          grepl("SPECTROstar", text_content, ignore.case = TRUE) ||
          grepl("FLUOstar", text_content, ignore.case = TRUE)) {
        result$format <- "bmg"
        result$instrument <- "BMG LABTECH"
      }

      # Tecan detection
      if (grepl("Tecan", text_content, ignore.case = TRUE) ||
          grepl("Infinite", text_content, ignore.case = TRUE)) {
        result$format <- "tecan"
        result$instrument <- "Tecan"
      }

      # Molecular Devices detection
      if (grepl("SpectraMax", text_content, ignore.case = TRUE) ||
          grepl("SoftMax", text_content, ignore.case = TRUE)) {
        result$format <- "molecular_devices"
        result$instrument <- "Molecular Devices"
      }

      # Generic plate grid detection - look for 8x12 numeric block
      if (result$format == "unknown") {
        result$format <- detect_generic_plate_grid(raw)
      }

      # Detect wavelengths
      wl_match <- regmatches(text_content, gregexpr("\\b(4[0-9]{2}|6[0-9]{2})\\s*(nm)?\\b", text_content))[[1]]
      if (length(wl_match) > 0) {
        result$wavelengths <- unique(as.numeric(gsub("[^0-9]", "", wl_match)))
      }

    }, error = function(e) {
      result$metadata$error <- e$message
    })
  }

  return(result)
}

#' Detect generic 8x12 plate grid in raw data
#'
#' @param raw Raw data frame from file
#' @return Format string ("generic_grid" or "unknown")
detect_generic_plate_grid <- function(raw) {
  # Look for row labels A-H in first or second column
  for (col_idx in 1:min(3, ncol(raw))) {
    col_vals <- toupper(as.character(raw[[col_idx]]))
    col_vals[is.na(col_vals)] <- ""
    row_labels <- col_vals %in% LETTERS[1:8]

    if (sum(row_labels) >= 6) {
      return("generic_grid")
    }
  }
  return("unknown")
}

#' Parse BioTek Synergy plate reader Excel file
#'
#' Handles the BioTek format where 450nm and 620nm readings alternate
#' in paired rows (odd = 450nm, even = 620nm).
#'
#' @param file_path Path to Excel file
#' @param sheet Sheet name or index
#' @param background_correct Whether to subtract 620nm from 450nm
#' @return List with plate_450, plate_620, plate_corrected matrices, and metadata
parse_biotek_synergy <- function(file_path, sheet = 1, background_correct = TRUE) {
  raw <- readxl::read_excel(file_path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")

  # Find the plate data block: look for row with column numbers 1-12
  plate_start <- NULL
  label_col <- NULL

  for (i in 1:min(30, nrow(raw))) {
    row_vals <- as.numeric(as.character(unlist(raw[i, ])))
    # Check if this row contains sequential numbers 1-12 (column headers)
    nums <- row_vals[!is.na(row_vals)]
    if (length(nums) >= 10 && all(1:12 %in% nums)) {
      plate_start <- i + 1  # Data starts on next row
      # Find which columns hold the plate data
      col_positions <- which(row_vals %in% 1:12)
      label_col <- min(col_positions) - 1  # Row label column is just before
      break
    }
  }

  if (is.null(plate_start)) {
    stop("Could not find plate data grid in BioTek file. Expected column numbers 1-12.")
  }

  # Extract data columns (the 12 columns containing plate data)
  data_cols <- which(as.numeric(as.character(unlist(raw[plate_start - 1, ]))) %in% 1:12)
  if (length(data_cols) < 12) {
    stop("Could not find all 12 plate columns")
  }

  # Check if we have dual wavelength (alternating rows: 450nm then 620nm)
  # Detect by checking if there are 16 data rows (8 * 2) vs 8
  n_data_rows <- 0
  for (i in plate_start:(plate_start + 20)) {
    if (i > nrow(raw)) break
    label <- toupper(as.character(raw[[i, label_col]]))
    if (!is.na(label) && label %in% LETTERS[1:8]) {
      n_data_rows <- n_data_rows + 1
    }
    # Also count unlabeled rows between labeled ones (620nm rows)
    if (is.na(label) || label == "") {
      # Check if the data in this row is numeric
      test_val <- suppressWarnings(as.numeric(as.character(raw[[i, data_cols[1]]])))
      if (!is.na(test_val) && test_val < 5) {
        n_data_rows <- n_data_rows + 1
      }
    }
  }

  dual_wavelength <- n_data_rows >= 16

  plate_450 <- matrix(NA, nrow = 8, ncol = 12)
  plate_620 <- matrix(NA, nrow = 8, ncol = 12)

  if (dual_wavelength) {
    # Alternating rows: labeled row = 450nm, next row = 620nm
    for (r in 1:8) {
      row_450 <- plate_start + (r - 1) * 2
      row_620 <- row_450 + 1

      for (c in 1:12) {
        plate_450[r, c] <- as.numeric(as.character(raw[[row_450, data_cols[c]]]))
        if (row_620 <= nrow(raw)) {
          plate_620[r, c] <- as.numeric(as.character(raw[[row_620, data_cols[c]]]))
        }
      }
    }
  } else {
    # Single wavelength - just 8 rows
    for (r in 1:8) {
      row_idx <- plate_start + (r - 1)
      for (c in 1:12) {
        plate_450[r, c] <- as.numeric(as.character(raw[[row_idx, data_cols[c]]]))
      }
    }
  }

  plate_corrected <- if (dual_wavelength && background_correct) {
    plate_450 - plate_620
  } else {
    plate_450
  }

  rownames(plate_450) <- rownames(plate_620) <- rownames(plate_corrected) <- LETTERS[1:8]
  colnames(plate_450) <- colnames(plate_620) <- colnames(plate_corrected) <- 1:12

  # Extract metadata
  metadata <- list()
  for (i in 1:min(plate_start - 1, nrow(raw))) {
    key <- as.character(raw[[i, 1]])
    val <- as.character(raw[[i, 2]])
    if (!is.na(key) && key != "" && !is.na(val)) {
      metadata[[key]] <- val
    }
  }

  return(list(
    plate_450 = plate_450,
    plate_620 = plate_620,
    plate_corrected = round(plate_corrected, 4),
    dual_wavelength = dual_wavelength,
    background_corrected = dual_wavelength && background_correct,
    metadata = metadata
  ))
}

#' Parse a generic plate grid from Excel or CSV
#'
#' Finds an 8x12 numeric block in the file and extracts it as a plate.
#'
#' @param file_path Path to file
#' @param file_name Original file name
#' @param sheet Sheet name or index (for Excel)
#' @return List with plate matrix and metadata
parse_generic_plate <- function(file_path, file_name, sheet = 1) {
  ext <- tolower(tools::file_ext(file_name))

  if (ext %in% c("xlsx", "xls")) {
    raw <- readxl::read_excel(file_path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  } else if (ext == "csv") {
    raw <- readr::read_csv(file_path, col_names = FALSE, show_col_types = FALSE)
  } else if (ext %in% c("tsv", "txt")) {
    raw <- readr::read_tsv(file_path, col_names = FALSE, show_col_types = FALSE)
  } else {
    stop("Unsupported file format: ", ext)
  }

  # Strategy: find rows where column 1 or 2 has A-H labels
  plate_rows <- list()
  label_col <- NULL

  for (col_idx in 1:min(3, ncol(raw))) {
    col_vals <- toupper(trimws(as.character(raw[[col_idx]])))
    col_vals[is.na(col_vals)] <- ""

    for (letter in LETTERS[1:8]) {
      matches <- which(col_vals == letter)
      if (length(matches) > 0) {
        plate_rows[[letter]] <- matches[1]
      }
    }

    if (length(plate_rows) >= 6) {
      label_col <- col_idx
      break
    }
    plate_rows <- list()
  }

  if (length(plate_rows) < 6) {
    stop("Could not find plate grid (A-H row labels) in file")
  }

  # Find data columns (12 numeric columns after the label column)
  first_data_row <- plate_rows[["A"]]
  data_start_col <- label_col + 1
  data_cols <- data_start_col:(data_start_col + 11)
  data_cols <- data_cols[data_cols <= ncol(raw)]

  if (length(data_cols) < 12) {
    stop("Could not find 12 data columns in plate grid")
  }

  plate <- matrix(NA, nrow = 8, ncol = 12)
  for (r in 1:8) {
    letter <- LETTERS[r]
    if (!letter %in% names(plate_rows)) next
    row_idx <- plate_rows[[letter]]
    for (c in 1:12) {
      plate[r, c] <- suppressWarnings(as.numeric(as.character(raw[[row_idx, data_cols[c]]])))
    }
  }

  rownames(plate) <- LETTERS[1:8]
  colnames(plate) <- 1:12

  return(list(
    plate = plate,
    format = "generic_grid",
    label_col = label_col,
    data_cols = data_cols
  ))
}

#' Create a default plate layout for standard configurations
#'
#' @param layout_type One of "standards_left", "standards_top", "custom"
#' @param std_concentrations Vector of standard concentrations
#' @param std_cols Columns for standards (default 1:2 for duplicates)
#' @param blank_wells Character vector of blank well positions (e.g., "H11", "H12")
#' @param nsb_wells Character vector of NSB well positions
#' @param b0_wells Character vector of B0 well positions
#' @return Data frame with Row, Col, Sample_Type, Concentration columns
create_plate_layout <- function(
    layout_type = "standards_left",
    std_concentrations = NULL,
    std_cols = 1:2,
    blank_wells = NULL,
    nsb_wells = NULL,
    b0_wells = NULL
) {
  layout <- expand.grid(
    Row = LETTERS[1:8],
    Col = 1:12,
    stringsAsFactors = FALSE
  )
  layout$Sample_Type <- "Sample"
  layout$Concentration <- NA_real_

  if (layout_type == "standards_left" && !is.null(std_concentrations)) {
    n_std <- length(std_concentrations)
    # Standards go in rows A through min(H, n_std), in std_cols
    for (i in seq_len(min(n_std, 8))) {
      for (col in std_cols) {
        idx <- layout$Row == LETTERS[i] & layout$Col == col
        layout$Sample_Type[idx] <- "Standard"
        layout$Concentration[idx] <- std_concentrations[i]
      }
    }
  }

  # Assign special wells
  assign_wells <- function(wells, type) {
    if (is.null(wells)) return()
    for (w in wells) {
      row <- substr(w, 1, 1)
      col <- as.integer(substr(w, 2, nchar(w)))
      idx <- layout$Row == row & layout$Col == col
      if (any(idx)) {
        layout$Sample_Type[idx] <<- type
        layout$Concentration[idx] <<- NA_real_
      }
    }
  }

  assign_wells(blank_wells, "Blank")
  assign_wells(nsb_wells, "NSB")
  assign_wells(b0_wells, "B0")

  return(layout)
}

#' Convert plate matrix + layout into columnar ELISA data
#'
#' Convenience wrapper combining parse and layout.
#'
#' @param plate_matrix 8x12 matrix of OD values
#' @param plate_layout Layout data frame from create_plate_layout()
#' @param include_empty Whether to include wells with NA OD values
#' @return Data frame ready for ELISA analysis
plate_to_columnar <- function(plate_matrix, plate_layout, include_empty = FALSE) {
  result <- parse_plate_grid(plate_matrix, plate_layout)

  if (!include_empty) {
    result <- result[!is.na(result$OD), ]
  }

  return(result)
}
