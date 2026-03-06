# ELISA Data Validation Module
# Functions for validating ELISA data quality and integrity

#' Validate uploaded ELISA data
#'
#' @param data Data frame to validate
#' @param config Configuration list
#' @return List with validation results (valid, messages, warnings)
validate_elisa_data <- function(data, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  validation_result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )

  # Check if data exists and has rows
  if (is.null(data) || nrow(data) == 0) {
    validation_result$valid <- FALSE
    validation_result$errors <- c(validation_result$errors, "Data is empty or NULL")
    return(validation_result)
  }

  # Check if data has columns
  if (ncol(data) == 0) {
    validation_result$valid <- FALSE
    validation_result$errors <- c(validation_result$errors, "Data has no columns")
    return(validation_result)
  }

  return(validation_result)
}

#' Validate that required columns are present after mapping
#'
#' @param data Data frame with mapped columns
#' @param skip_normalization Logical indicating if normalization will be skipped
#' @return List with validation results
validate_required_columns <- function(data, skip_normalization = FALSE) {
  validation_result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )

  # Define required columns
  required_cols <- c(COL_WELL, COL_TYPE, COL_OD)

  # Add concentration if normalization is not skipped
  if (!skip_normalization) {
    required_cols <- c(required_cols, COL_CONCENTRATION)
  }

  # Check for missing columns
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    validation_result$valid <- FALSE
    validation_result$errors <- c(
      validation_result$errors,
      paste("Missing required columns:", paste(missing_cols, collapse = ", "))
    )
  }

  return(validation_result)
}

#' Validate standards data
#'
#' @param standards Standards data frame
#' @param config Configuration list
#' @return List with validation results
validate_standards <- function(standards, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  validation_result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0)
  )

  # Check if standards exist
  if (is.null(standards) || nrow(standards) == 0) {
    validation_result$valid <- FALSE
    validation_result$errors <- c(validation_result$errors, "No standards found in data")
    return(validation_result)
  }

  # Check minimum number of standards
  min_standards <- get_config_value("std_curve.min_standards", config)
  n_unique_standards <- length(unique(standards[[COL_CONCENTRATION]]))

  if (n_unique_standards < min_standards) {
    validation_result$valid <- FALSE
    validation_result$errors <- c(
      validation_result$errors,
      paste("Insufficient standards:", n_unique_standards, "found, minimum", min_standards, "required")
    )
  }

  # Check for NA values in critical columns
  if (any(is.na(standards[[COL_CONCENTRATION]]))) {
    validation_result$warnings <- c(
      validation_result$warnings,
      "Some standards have NA concentration values"
    )
  }

  if (any(is.na(standards[[COL_OD]]))) {
    validation_result$warnings <- c(
      validation_result$warnings,
      "Some standards have NA OD values"
    )
  }

  # Check for duplicate concentrations
  conc_counts <- table(standards[[COL_CONCENTRATION]])
  if (any(conc_counts > 10)) {
    validation_result$warnings <- c(
      validation_result$warnings,
      "Some standard concentrations have unusually many replicates (>10)"
    )
  }

  # Check for negative concentrations
  if (any(standards[[COL_CONCENTRATION]] < 0, na.rm = TRUE)) {
    validation_result$warnings <- c(
      validation_result$warnings,
      "Some standards have negative concentration values"
    )
  }

  return(validation_result)
}

#' Validate normalized response values
#'
#' @param data Data frame with Response column
#' @param config Configuration list
#' @return List with validation results and invalid flags
validate_response_values <- function(data, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  validation_result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0),
    invalid_flags = rep(FALSE, nrow(data))
  )

  # Get QC thresholds
  response_min <- get_config_value("qc_thresholds.response_min", config)
  response_max <- get_config_value("qc_thresholds.response_max", config)

  # Check for NA values
  na_count <- sum(is.na(data[[COL_RESPONSE]]))
  if (na_count > 0) {
    validation_result$warnings <- c(
      validation_result$warnings,
      paste(na_count, "rows have NA response values")
    )
  }

  # Flag invalid responses (too low or too high)
  validation_result$invalid_flags <-
    data[[COL_RESPONSE]] < response_min |
    data[[COL_RESPONSE]] > response_max |
    is.na(data[[COL_RESPONSE]])

  invalid_count <- sum(validation_result$invalid_flags, na.rm = TRUE)
  if (invalid_count > 0) {
    validation_result$warnings <- c(
      validation_result$warnings,
      paste(invalid_count, "rows have response values outside acceptable range [",
            response_min, ",", response_max, "]")
    )
  }

  return(validation_result)
}

#' Validate B0 and NSB values for normalization
#'
#' @param b0_od B0 (maximum binding) OD value
#' @param nsb_od NSB (non-specific binding) OD value
#' @param config Configuration list
#' @return List with validation results and recommendation
validate_b0_nsb <- function(b0_od, nsb_od, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  validation_result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0),
    skip_normalization = FALSE
  )

  # Check if values are NA
  if (is.na(b0_od)) {
    validation_result$warnings <- c(validation_result$warnings, "B0 value is NA")
    validation_result$skip_normalization <- TRUE
  }

  if (is.na(nsb_od)) {
    validation_result$warnings <- c(validation_result$warnings, "NSB value is NA")
    validation_result$skip_normalization <- TRUE
  }

  # Check if B0 and NSB are too close
  if (!is.na(b0_od) && !is.na(nsb_od)) {
    min_diff <- get_config_value("qc_thresholds.b0_nsb_min_diff", config)
    diff <- abs(b0_od - nsb_od)

    if (diff < min_diff) {
      validation_result$warnings <- c(
        validation_result$warnings,
        paste("B0 and NSB values are too close (diff =", round(diff, 4),
              "), normalization may be unreliable")
      )
      validation_result$skip_normalization <- TRUE
    }
  }

  return(validation_result)
}

#' Validate sample replicates for high CV
#'
#' @param samples Samples data frame
#' @param config Configuration list
#' @return List with validation results and CV flags
validate_sample_cv <- function(samples, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  validation_result <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0),
    high_cv_samples = character(0)
  )

  # Get CV threshold
  cv_threshold <- get_config_value("qc_thresholds.cv_threshold", config)

  # Calculate CV for each sample type
  sample_cv <- samples %>%
    group_by(!!sym(COL_TYPE)) %>%
    summarize(
      n = n(),
      cv = calculate_cv(!!sym(COL_RESPONSE)),
      .groups = "drop"
    ) %>%
    filter(n > 1)  # Only check CV for replicates

  # Identify high CV samples
  high_cv <- sample_cv %>%
    filter(cv > cv_threshold, !is.na(cv))

  if (nrow(high_cv) > 0) {
    validation_result$high_cv_samples <- high_cv[[COL_TYPE]]
    validation_result$warnings <- c(
      validation_result$warnings,
      paste("High CV detected in samples:", paste(high_cv[[COL_TYPE]], collapse = ", "))
    )
  }

  return(validation_result)
}

#' Perform comprehensive data validation
#'
#' @param data Raw data frame
#' @param standards Standards data frame (after extraction)
#' @param samples Samples data frame (after extraction)
#' @param b0_od B0 OD value
#' @param nsb_od NSB OD value
#' @param config Configuration list
#' @return Comprehensive validation results
perform_full_validation <- function(data, standards = NULL, samples = NULL,
                                   b0_od = NA, nsb_od = NA, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  validation_results <- list(
    overall_valid = TRUE,
    data_validation = NULL,
    column_validation = NULL,
    standards_validation = NULL,
    b0_nsb_validation = NULL,
    sample_cv_validation = NULL,
    all_errors = character(0),
    all_warnings = character(0)
  )

  # Validate raw data
  validation_results$data_validation <- validate_elisa_data(data, config)
  if (!validation_results$data_validation$valid) {
    validation_results$overall_valid <- FALSE
  }

  # Validate standards if provided
  if (!is.null(standards)) {
    validation_results$standards_validation <- validate_standards(standards, config)
    if (!validation_results$standards_validation$valid) {
      validation_results$overall_valid <- FALSE
    }
  }

  # Validate B0 and NSB if provided
  if (!is.na(b0_od) || !is.na(nsb_od)) {
    validation_results$b0_nsb_validation <- validate_b0_nsb(b0_od, nsb_od, config)
  }

  # Validate sample CV if provided
  if (!is.null(samples) && nrow(samples) > 0) {
    if (COL_RESPONSE %in% colnames(samples)) {
      validation_results$sample_cv_validation <- validate_sample_cv(samples, config)
    }
  }

  # Collect all errors and warnings
  for (result_name in names(validation_results)) {
    result <- validation_results[[result_name]]
    if (is.list(result)) {
      if ("errors" %in% names(result)) {
        validation_results$all_errors <- c(validation_results$all_errors, result$errors)
      }
      if ("warnings" %in% names(result)) {
        validation_results$all_warnings <- c(validation_results$all_warnings, result$warnings)
      }
    }
  }

  # Print summary
  if (length(validation_results$all_errors) > 0) {
    message("Validation Errors:")
    for (error in validation_results$all_errors) {
      message("  - ", error)
    }
  }

  if (length(validation_results$all_warnings) > 0) {
    message("Validation Warnings:")
    for (warning in validation_results$all_warnings) {
      message("  - ", warning)
    }
  }

  return(validation_results)
}
