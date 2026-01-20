# ELISA Analyzer Configuration Management
# This file manages configuration settings for the generalized ELISA analyzer

#' Default configuration settings for ELISA analysis
#'
#' @return List of default configuration values
get_default_config <- function() {
  list(
    # Application settings
    app_name = "ELISA Analyzer",
    app_version = "2.0.0",

    # Assay type settings
    assay_types = c("competitive", "direct", "sandwich", "auto"),
    default_assay_type = "auto",

    # Sample type labels
    sample_types = list(
      nsb = "NSB",
      b0 = "B0",
      standard = "Standard",
      blank = "Blank",
      sample = "Sample"
    ),

    # Model settings
    available_models = c("4PL", "Linear"),
    default_model_selection = "auto",  # auto, 4PL, or Linear

    # Data processing settings
    log_transform = TRUE,
    log_offset = 1e-5,
    skip_normalization = FALSE,
    use_corrected_od = TRUE,
    remove_unlabeled = TRUE,

    # Quality control thresholds
    qc_thresholds = list(
      cv_threshold = 0.15,  # 15% CV for replicates
      response_min = -0.5,  # Minimum acceptable normalized response
      response_max = 10,    # Maximum acceptable normalized response
      b0_nsb_min_diff = 0.05  # Minimum difference between B0 and NSB
    ),

    # Standard curve settings
    std_curve = list(
      min_standards = 3,  # Minimum number of standard points
      extrapolation_factor = 10,  # Extend curve for visualization
      curve_points = 100  # Number of points for smooth curve plotting
    ),

    # Column name defaults
    default_columns = list(
      well = "Well ID",
      type = "Sample Type",
      concentration = "Concentration",
      od = "OD",
      od_corrected = "OD (corrected)"
    ),

    # Column name patterns for auto-detection
    column_patterns = list(
      well = c("well", "id", "position"),
      type = c("type", "sample", "category", "group"),
      concentration = c("conc", "concentration", "std", "standard"),
      od = c("od", "absorbance", "optical", "density"),
      od_corrected = c("correct", "adjusted", "normalized")
    ),

    # Plotting settings
    plot_settings = list(
      default_theme = "minimal",
      point_size = 3,
      line_size = 1,
      error_bar_width = 0.1,
      alpha_error = 0.5,
      show_std_range_lines = TRUE,
      default_plot_type = "combined"  # combined, 4pl, or linear
    ),

    # Export settings
    export_settings = list(
      default_prefix = "ELISA_Results",
      timestamp_format = "%Y%m%d_%H%M%S",
      plot_width = 1200,
      plot_height = 800,
      plot_resolution = 150
    ),

    # Validation settings
    validation = list(
      require_standards = TRUE,
      require_samples = FALSE,  # Allow standards-only analysis
      min_replicates = 1
    )
  )
}

#' Get a specific configuration value
#'
#' @param key Configuration key (supports nested keys with dot notation)
#' @param config Optional custom configuration list
#' @return Configuration value
get_config_value <- function(key, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  # Support nested keys with dot notation (e.g., "qc_thresholds.cv_threshold")
  keys <- strsplit(key, "\\.")[[1]]
  value <- config

  for (k in keys) {
    if (k %in% names(value)) {
      value <- value[[k]]
    } else {
      warning(paste("Configuration key not found:", key))
      return(NULL)
    }
  }

  return(value)
}

#' Update configuration with custom values
#'
#' @param custom_config List of custom configuration values
#' @param base_config Base configuration (defaults to default config)
#' @return Merged configuration list
update_config <- function(custom_config, base_config = NULL) {
  if (is.null(base_config)) {
    base_config <- get_default_config()
  }

  # Recursively merge lists
  merge_lists <- function(base, custom) {
    if (!is.list(custom)) {
      return(custom)
    }

    for (name in names(custom)) {
      if (name %in% names(base) && is.list(base[[name]]) && is.list(custom[[name]])) {
        base[[name]] <- merge_lists(base[[name]], custom[[name]])
      } else {
        base[[name]] <- custom[[name]]
      }
    }

    return(base)
  }

  merge_lists(base_config, custom_config)
}

#' Validate configuration settings
#'
#' @param config Configuration list to validate
#' @return Logical indicating if configuration is valid
validate_config <- function(config) {
  valid <- TRUE
  messages <- character(0)

  # Check required fields
  required_fields <- c("app_name", "assay_types", "available_models", "qc_thresholds")

  for (field in required_fields) {
    if (!field %in% names(config)) {
      valid <- FALSE
      messages <- c(messages, paste("Missing required configuration field:", field))
    }
  }

  # Validate assay types
  if ("assay_types" %in% names(config)) {
    valid_assay_types <- c("competitive", "direct", "sandwich", "auto")
    invalid_types <- setdiff(config$assay_types, valid_assay_types)

    if (length(invalid_types) > 0) {
      valid <- FALSE
      messages <- c(messages, paste("Invalid assay types:", paste(invalid_types, collapse = ", ")))
    }
  }

  # Validate QC thresholds
  if ("qc_thresholds" %in% names(config)) {
    qc <- config$qc_thresholds

    if ("cv_threshold" %in% names(qc)) {
      if (qc$cv_threshold <= 0 || qc$cv_threshold > 1) {
        valid <- FALSE
        messages <- c(messages, "CV threshold must be between 0 and 1")
      }
    }

    if ("response_min" %in% names(qc) && "response_max" %in% names(qc)) {
      if (qc$response_min >= qc$response_max) {
        valid <- FALSE
        messages <- c(messages, "Response min must be less than response max")
      }
    }
  }

  # Print validation messages
  if (length(messages) > 0) {
    for (msg in messages) {
      message(paste("Configuration validation:", msg))
    }
  }

  return(valid)
}

#' Save configuration to file
#'
#' @param config Configuration list
#' @param file_path Path to save configuration file
#' @return Logical indicating success
save_config <- function(config, file_path) {
  tryCatch({
    saveRDS(config, file_path)
    message(paste("Configuration saved to:", file_path))
    return(TRUE)
  }, error = function(e) {
    warning(paste("Failed to save configuration:", e$message))
    return(FALSE)
  })
}

#' Load configuration from file
#'
#' @param file_path Path to configuration file
#' @return Configuration list or NULL if loading fails
load_config <- function(file_path) {
  if (!file.exists(file_path)) {
    warning(paste("Configuration file not found:", file_path))
    return(NULL)
  }

  tryCatch({
    config <- readRDS(file_path)
    message(paste("Configuration loaded from:", file_path))

    # Validate loaded configuration
    if (validate_config(config)) {
      return(config)
    } else {
      warning("Loaded configuration failed validation, using defaults")
      return(get_default_config())
    }
  }, error = function(e) {
    warning(paste("Failed to load configuration:", e$message))
    return(NULL)
  })
}
