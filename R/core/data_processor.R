# ELISA Data Processing Module
# Functions for processing, normalizing, and transforming ELISA data

#' Map user-selected columns to standardized internal names
#'
#' @param data Raw data frame
#' @param well_col User-selected well column name
#' @param type_col User-selected type column name
#' @param conc_col User-selected concentration column name
#' @param od_col User-selected OD column name
#' @param od_corr_col User-selected corrected OD column name (optional)
#' @param use_corrected Logical indicating if corrected OD should be used
#' @return Data frame with standardized column names
map_column_names <- function(data, well_col, type_col, conc_col, od_col,
                            od_corr_col = NULL, use_corrected = FALSE) {
  # Rename columns to standard names
  data <- rename_column_safe(data, well_col, COL_WELL)
  data <- rename_column_safe(data, type_col, COL_TYPE)
  data <- rename_column_safe(data, conc_col, COL_CONCENTRATION)
  data <- rename_column_safe(data, od_col, COL_OD)

  # Handle corrected OD if specified
  if (use_corrected && !is.null(od_corr_col) && od_corr_col %in% colnames(data)) {
    data <- rename_column_safe(data, od_corr_col, COL_OD_CORRECTED)
  }

  return(data)
}

#' Auto-detect and map columns based on patterns
#'
#' @param data Raw data frame
#' @param config Configuration list with column patterns
#' @return List with detected column names
auto_detect_columns <- function(data, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  detected_cols <- list()

  # Get column patterns from config
  patterns <- get_config_value("column_patterns", config)
  defaults <- get_config_value("default_columns", config)

  # Detect each column
  detected_cols$well <- detect_column(data, patterns$well, defaults$well)
  detected_cols$type <- detect_column(data, patterns$type, defaults$type)
  detected_cols$concentration <- detect_column(data, patterns$concentration, defaults$concentration)
  detected_cols$od <- detect_column(data, patterns$od, defaults$od)
  detected_cols$od_corrected <- detect_column(data, patterns$od_corrected, NULL)

  return(detected_cols)
}

#' Clean data by removing unlabeled samples and invalid rows
#'
#' @param data Data frame with standardized column names
#' @param remove_unlabeled Logical indicating if unlabeled samples should be removed
#' @return Cleaned data frame
clean_data <- function(data, remove_unlabeled = TRUE) {
  # Remove rows with empty or NA Type if requested
  if (remove_unlabeled) {
    data <- data %>%
      filter(!is.na(!!sym(COL_TYPE)) & trimws(as.character(!!sym(COL_TYPE))) != "")
  }

  # Ensure numeric columns are properly typed
  if (COL_OD %in% colnames(data)) {
    data[[COL_OD]] <- as.numeric(data[[COL_OD]])
  }

  if (COL_CONCENTRATION %in% colnames(data)) {
    data[[COL_CONCENTRATION]] <- as.numeric(data[[COL_CONCENTRATION]])
  }

  return(data)
}

#' Extract control samples (NSB, B0, Blank) from data
#'
#' @param data Data frame with standardized column names
#' @param nsb_label Label for NSB samples
#' @param b0_label Label for B0 samples
#' @param blank_label Label for Blank samples
#' @param od_column Which OD column to use
#' @return List with NSB, B0, and Blank OD values
extract_controls <- function(data, nsb_label = "NSB", b0_label = "B0",
                            blank_label = "Blank", od_column = COL_OD) {
  controls <- list(
    nsb_od = NA,
    b0_od = NA,
    blank_od = NA
  )

  # Extract NSB
  nsb_data <- data %>% filter(!!sym(COL_TYPE) == nsb_label)
  if (nrow(nsb_data) > 0) {
    controls$nsb_od <- mean(nsb_data[[od_column]], na.rm = TRUE)
  } else {
    controls$nsb_od <- 0
  }

  # Extract B0
  b0_data <- data %>% filter(!!sym(COL_TYPE) == b0_label)
  if (nrow(b0_data) > 0) {
    controls$b0_od <- mean(b0_data[[od_column]], na.rm = TRUE)
  }

  # Extract Blank
  blank_data <- data %>% filter(!!sym(COL_TYPE) == blank_label)
  if (nrow(blank_data) > 0) {
    controls$blank_od <- mean(blank_data[[od_column]], na.rm = TRUE)
  } else {
    controls$blank_od <- 0
  }

  return(controls)
}

#' Extract standards from data
#'
#' @param data Data frame with standardized column names
#' @param std_label Label for standard samples
#' @return Standards data frame
extract_standards <- function(data, std_label = "Standard") {
  standards <- data %>%
    filter(!!sym(COL_TYPE) == std_label) %>%
    mutate(
      !!COL_CONCENTRATION := as.numeric(!!sym(COL_CONCENTRATION)),
      !!COL_OD := as.numeric(!!sym(COL_OD))
    )

  return(standards)
}

#' Extract samples (non-control, non-standard) from data
#'
#' @param data Data frame with standardized column names
#' @param control_labels Character vector of control/standard labels to exclude
#' @return Samples data frame
extract_samples <- function(data, control_labels = c("NSB", "B0", "Standard", "Blank")) {
  samples <- data %>%
    filter(!(!!sym(COL_TYPE) %in% control_labels)) %>%
    mutate(
      # Initialize prediction columns
      !!COL_PRED_CONC := NA_real_,
      !!COL_CAPPED_CONC := NA_real_,
      !!COL_MODEL_USED := NA_character_,
      !!COL_ERROR_FLAG := FALSE,
      !!COL_OUT_OF_RANGE := FALSE,
      !!COL_IS_CAPPED := FALSE
    )

  return(samples)
}

#' Apply blank correction to OD values
#'
#' @param data Data frame with OD values
#' @param blank_od Blank OD value to subtract
#' @param od_column Which OD column to use
#' @return Data frame with OD_cor column added
apply_blank_correction <- function(data, blank_od, od_column = COL_OD) {
  data[[COL_OD_COR]] <- data[[od_column]] - blank_od
  return(data)
}

#' Normalize OD values using B0/NSB normalization
#'
#' @param data Data frame with OD_cor column
#' @param b0_od B0 (maximum binding) OD value
#' @param nsb_od NSB (non-specific binding) OD value
#' @param skip_normalization Logical indicating if normalization should be skipped
#' @return Data frame with Response column added
normalize_response <- function(data, b0_od, nsb_od, skip_normalization = FALSE) {
  if (skip_normalization) {
    # Use blank-corrected OD directly as response
    data[[COL_RESPONSE]] <- data[[COL_OD_COR]]
  } else {
    # Calculate B/B0 normalized response
    b0_norm <- b0_od - nsb_od
    data[[COL_RESPONSE]] <- data[[COL_OD_COR]] / b0_norm
  }

  # Store raw response values before any modifications
  data$Raw_Response <- data[[COL_RESPONSE]]

  return(data)
}

#' Apply log transformation to concentrations
#'
#' @param data Data frame with Concentration column
#' @param log_transform Logical indicating if log transformation should be applied
#' @param offset Small positive value to add before log transformation
#' @return Data frame with log_Concentration column added (if log_transform = TRUE)
transform_concentrations <- function(data, log_transform = TRUE, offset = DEFAULT_LOG_OFFSET) {
  if (log_transform && COL_CONCENTRATION %in% colnames(data)) {
    data[[COL_LOG_CONC]] <- safe_log10(data[[COL_CONCENTRATION]], offset)
  }

  return(data)
}

#' Process standards data through full pipeline
#'
#' @param standards Standards data frame (after extraction)
#' @param controls List with control OD values
#' @param skip_normalization Logical indicating if normalization should be skipped
#' @param log_transform Logical indicating if log transformation should be applied
#' @param config Configuration list
#' @return Processed standards data frame
process_standards <- function(standards, controls, skip_normalization = FALSE,
                             log_transform = TRUE, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  offset <- get_config_value("log_offset", config)

  # Apply blank correction
  blank_od <- if (!is.na(controls$blank_od) && controls$blank_od > 0) {
    controls$blank_od
  } else {
    controls$nsb_od
  }

  standards <- apply_blank_correction(standards, blank_od)

  # Normalize response
  standards <- normalize_response(standards, controls$b0_od, controls$nsb_od, skip_normalization)

  # Log transform concentrations
  standards <- transform_concentrations(standards, log_transform, offset)

  # Add invalid flag for extreme values
  qc_thresholds <- get_config_value("qc_thresholds", config)
  standards$Invalid_Flag <-
    standards[[COL_RESPONSE]] < qc_thresholds$response_min |
    standards[[COL_RESPONSE]] > qc_thresholds$response_max |
    is.na(standards[[COL_RESPONSE]])

  return(standards)
}

#' Process samples data through full pipeline
#'
#' @param samples Samples data frame (after extraction)
#' @param controls List with control OD values
#' @param skip_normalization Logical indicating if normalization should be skipped
#' @param config Configuration list
#' @return Processed samples data frame
process_samples <- function(samples, controls, skip_normalization = FALSE, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  # Apply blank correction
  blank_od <- if (!is.na(controls$blank_od) && controls$blank_od > 0) {
    controls$blank_od
  } else {
    controls$nsb_od
  }

  # Check if OD_cor already exists (from earlier processing)
  if (!COL_OD_COR %in% colnames(samples)) {
    samples <- apply_blank_correction(samples, blank_od)
  }

  # Normalize response
  samples <- normalize_response(samples, controls$b0_od, controls$nsb_od, skip_normalization)

  # Add invalid flag for extreme values
  qc_thresholds <- get_config_value("qc_thresholds", config)
  samples$Invalid_Flag <-
    samples[[COL_RESPONSE]] < qc_thresholds$response_min |
    samples[[COL_RESPONSE]] > qc_thresholds$response_max |
    is.na(samples[[COL_RESPONSE]])

  return(samples)
}

#' Summarize standards by concentration (average replicates)
#'
#' @param standards Processed standards data frame
#' @return Standards summary data frame
summarize_standards <- function(standards) {
  std_summary <- standards %>%
    group_by(!!sym(COL_CONCENTRATION)) %>%
    summarize(
      Mean_OD = mean(!!sym(COL_OD), na.rm = TRUE),
      Mean_OD_cor = mean(!!sym(COL_OD_COR), na.rm = TRUE),
      Mean_Response = mean(!!sym(COL_RESPONSE), na.rm = TRUE),
      SD_Response = sd(!!sym(COL_RESPONSE), na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(!!sym(COL_CONCENTRATION))

  return(std_summary)
}

#' Calculate standard curve range
#'
#' @param standards Standards data frame
#' @return Named numeric vector with min and max concentrations
calculate_std_range <- function(standards) {
  # Get standards with positive concentration
  positive_conc <- standards[[COL_CONCENTRATION]][standards[[COL_CONCENTRATION]] > 0]

  if (length(positive_conc) == 0) {
    # Fallback if no positive concentrations
    c(
      min = min(standards[[COL_CONCENTRATION]], na.rm = TRUE),
      max = max(standards[[COL_CONCENTRATION]], na.rm = TRUE)
    )
  } else {
    c(
      min = min(positive_conc, na.rm = TRUE),
      max = max(standards[[COL_CONCENTRATION]], na.rm = TRUE)
    )
  }
}

#' Create complete processed data object
#'
#' @param raw_data Raw data frame
#' @param standards Processed standards
#' @param samples Processed samples
#' @param controls Control values
#' @param std_summary Standards summary
#' @param std_range Standard curve range
#' @param config Configuration used
#' @return List with all processed data
create_processed_data <- function(raw_data, standards, samples, controls,
                                  std_summary, std_range, config) {
  list(
    raw_data = raw_data,
    standards = standards,
    samples = samples,
    controls = controls,
    std_summary = std_summary,
    std_range = std_range,
    config = config
  )
}
