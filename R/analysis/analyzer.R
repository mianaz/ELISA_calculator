# Generalized ELISA Analyzer
# Main analysis function using modular components

# Source all dependencies
source("R/utils/config.R")
source("R/utils/constants.R")
source("R/utils/helpers.R")
source("R/core/data_validator.R")
source("R/core/data_processor.R")
source("R/core/assay_detector.R")
source("R/models/model_selector.R")
source("R/quality/qc_statistics.R")  # New QC module
source("R/prediction.R")  # Keep original prediction functions for now
source("R/plotting.R")     # Keep original plotting functions for now

#' Analyze ELISA Data (Generalized for all assay types)
#'
#' @param file_path Path to data file
#' @param sheet_name Sheet name or index (for Excel files, ignored for CSV/TSV)
#' @param file_name Original file name (used to detect file type)
#' @param well_col Well ID column name
#' @param type_col Sample type column name
#' @param conc_col Concentration column name
#' @param od_col OD column name
#' @param od_corr_col Corrected OD column name (optional)
#' @param use_corrected Whether to use corrected OD
#' @param assay_type Assay type ("auto", "competitive", "direct", or "sandwich")
#' @param nsb_label NSB sample label
#' @param b0_label B0 sample label
#' @param std_label Standard sample label
#' @param blank_label Blank sample label
#' @param skip_normalization Skip B0/NSB normalization
#' @param log_transform Use log transformation for concentrations
#' @param offset Offset for log transformation
#' @param remove_unlabeled Remove samples without labels
#' @param dilution_factors Named vector or data frame mapping sample types to dilution factors
#' @param weight_type Weighting for regression: "none", "1/Y", "1/Y^2", "1/X^2"
#' @param models_to_fit Models to fit: c("4PL", "5PL", "Linear")
#' @param calculate_limits Whether to calculate LOD/LOQ/ULOQ
#' @param config Optional custom configuration
#' @return List with analysis results
analyze_elisa <- function(
    file_path,
    sheet_name = 1,
    file_name = NULL,
    well_col = "Well ID",
    type_col = "Sample Type",
    conc_col = "Concentration",
    od_col = "OD",
    od_corr_col = "OD (corrected)",
    use_corrected = TRUE,
    assay_type = "auto",
    nsb_label = "NSB",
    b0_label = "B0",
    std_label = "Standard",
    blank_label = "Blank",
    skip_normalization = FALSE,
    log_transform = TRUE,
    offset = DEFAULT_LOG_OFFSET,
    remove_unlabeled = TRUE,
    dilution_factors = NULL,
    weight_type = "none",
    models_to_fit = c(MODEL_4PL, MODEL_5PL, MODEL_LINEAR),
    calculate_limits = TRUE,
    config = NULL
) {

  # Get configuration
  if (is.null(config)) {
    config <- get_default_config()
  }

  message("=== Starting ELISA Analysis ===")

  # STEP 1: Read data based on file type
  message("\n[1/8] Reading data file...")

  # Determine file type from file_name or file_path
  if (is.null(file_name)) {
    file_name <- basename(file_path)
  }
  ext <- tolower(tools::file_ext(file_name))

  # Read data based on file extension
  if (ext %in% c("xlsx", "xls")) {
    # Excel file
    raw_data <- readxl::read_excel(file_path, sheet = sheet_name)
    message("  Read Excel file: ", nrow(raw_data), " rows, ", ncol(raw_data), " columns")
  } else if (ext == "csv") {
    # CSV file
    raw_data <- readr::read_csv(file_path, show_col_types = FALSE)
    message("  Read CSV file: ", nrow(raw_data), " rows, ", ncol(raw_data), " columns")
  } else if (ext == "tsv") {
    # TSV file
    raw_data <- readr::read_tsv(file_path, show_col_types = FALSE)
    message("  Read TSV file: ", nrow(raw_data), " rows, ", ncol(raw_data), " columns")
  } else {
    # Default to Excel for backward compatibility
    raw_data <- readxl::read_excel(file_path, sheet = sheet_name)
    message("  Read ", nrow(raw_data), " rows, ", ncol(raw_data), " columns")
  }

  # STEP 2: Auto-detect columns if needed
  message("\n[2/8] Mapping columns...")
  detected_cols <- auto_detect_columns(raw_data, config)

  # Use detected columns as defaults if user didn't specify
  well_col <- well_col %||% detected_cols$well
  type_col <- type_col %||% detected_cols$type
  conc_col <- conc_col %||% detected_cols$concentration
  od_col <- od_col %||% detected_cols$od

  # Map to standardized column names
  data <- map_column_names(
    raw_data, well_col, type_col, conc_col, od_col,
    od_corr_col, use_corrected
  )
  message("  Columns mapped: Well, Type, OD, Concentration")

  # STEP 3: Clean data
  message("\n[3/8] Cleaning data...")
  data <- clean_data(data, remove_unlabeled)
  message("  ", nrow(data), " rows after cleaning")

  # STEP 4: Extract controls, standards, and samples
  message("\n[4/8] Extracting data groups...")

  # Determine which OD column to use
  od_column <- if (use_corrected && COL_OD_CORRECTED %in% colnames(data)) {
    COL_OD_CORRECTED
  } else {
    COL_OD
  }

  controls <- extract_controls(data, nsb_label, b0_label, blank_label, od_column)
  message("  Controls: NSB=", round(controls$nsb_od, 4), ", B0=", round(controls$b0_od, 4),
          ", Blank=", round(controls$blank_od, 4))

  standards <- extract_standards(data, std_label)
  message("  Standards: ", nrow(standards), " wells, ",
          length(unique(standards[[COL_CONCENTRATION]])), " concentrations")

  samples <- extract_samples(data, c(nsb_label, b0_label, std_label, blank_label))
  message("  Samples: ", nrow(samples), " wells")

  # STEP 5: Validate B0/NSB and adjust normalization if needed
  message("\n[5/8] Validating controls...")
  b0_nsb_validation <- validate_b0_nsb(controls$b0_od, controls$nsb_od, config)

  if (b0_nsb_validation$skip_normalization && !skip_normalization) {
    message("  WARNING: B0/NSB validation suggests skipping normalization")
    skip_normalization <- TRUE
  }

  # STEP 6: Process standards and samples
  message("\n[6/8] Processing data...")
  standards <- process_standards(standards, controls, skip_normalization, log_transform, config)
  samples <- process_samples(samples, controls, skip_normalization, config)

  # Summarize standards
  std_summary <- summarize_standards(standards)
  std_range <- calculate_std_range(standards)
  message("  Standard curve range: ", format_concentration_range(std_range[1], std_range[2]))

  # STEP 7: Detect assay type
  message("\n[7/8] Detecting assay type...")
  assay_result <- determine_assay_type(assay_type, standards, config)
  final_assay_type <- assay_result$assay_type
  message("  ", assay_result$detection_result$message)

  # STEP 8: Fit models
  message("\n[8/10] Fitting dose-response models...")

  # Apply weighting if specified
  if (weight_type != "none" && weight_type != WEIGHT_NONE) {
    message("  Applying ", weight_type, " weighting...")
    standards <- calculate_weights(standards, weight_type)
  }

  # Fit all models
  fitted_models <- fit_all_models(standards, log_transform, models_to_fit)

  # Select best model
  model_selection <- select_best_model(fitted_models, criterion = "aic")
  best_model_name <- model_selection$best_model_name

  # Get model comparison and equations
  model_comparison <- model_selection$comparison
  equations <- get_model_equations(fitted_models)

  message("\n=== Model Fitting Complete ===")
  message("Best model: ", best_model_name)

  # STEP 9: Calculate LOD/LOQ/ULOQ
  assay_limits <- NULL
  standards_accuracy <- NULL

  if (calculate_limits && !is.null(model_selection$best_model)) {
    message("\n[9/10] Calculating assay limits (LOD/LOQ/ULOQ)...")

    # Get blank values for LOD/LOQ
    blank_data <- data %>%
      filter(Type == blank_label)

    blank_response <- if (nrow(blank_data) > 0 && COL_RESPONSE %in% colnames(blank_data)) {
      blank_data[[COL_RESPONSE]]
    } else {
      NULL
    }

    # Calculate assay limits
    tryCatch({
      assay_limits <- calculate_assay_limits(
        blank_values = blank_response,
        standards_data = standards,
        model = model_selection$best_model,
        config = config
      )

      if (!is.null(assay_limits$lod$concentration)) {
        message("  LOD: ", format_number(assay_limits$lod$concentration))
      }
      if (!is.null(assay_limits$loq$concentration)) {
        message("  LOQ: ", format_number(assay_limits$loq$concentration))
      }
      if (!is.null(assay_limits$uloq$concentration)) {
        message("  ULOQ: ", format_number(assay_limits$uloq$concentration))
      }
    }, error = function(e) {
      message("  Warning: Could not calculate assay limits - ", e$message)
    })

    # Calculate back-calculation and accuracy for standards
    message("  Calculating standard curve accuracy...")
    tryCatch({
      standards_accuracy <- summarize_standards_with_accuracy(standards, model_selection$best_model)

      if (!is.null(standards_accuracy)) {
        passing <- sum(standards_accuracy$QC_Status == "PASS", na.rm = TRUE)
        total <- nrow(standards_accuracy)
        message("  Standards passing accuracy criteria: ", passing, "/", total)
      }
    }, error = function(e) {
      message("  Warning: Could not calculate accuracy - ", e$message)
    })
  } else {
    message("\n[9/10] Skipping assay limits calculation")
  }

  # STEP 10: Generate predictions using model selector with fallback
  message("\n[10/10] Predicting Sample Concentrations...")

  # Use the new model-based prediction with fallback
  sample_predictions <- predict_with_fallback(
    samples[[COL_RESPONSE]],
    fitted_models,
    best_model_name,
    std_range
  )

  # Merge predictions back into samples
  samples[[COL_PRED_CONC]] <- sample_predictions$Predicted_Concentration
  samples[[COL_MODEL_USED]] <- sample_predictions$Model_Used
  samples[[COL_OUT_OF_RANGE]] <- sample_predictions$Out_of_Range
  samples[[COL_ERROR_FLAG]] <- !is.na(sample_predictions$Error_Message)

  # Apply concentration capping for out-of-range samples
  samples <- apply_concentration_capping(samples, std_range, standards)

  # Apply dilution factors if provided
  if (!is.null(dilution_factors)) {
    message("  Applying dilution factors...")
    samples <- apply_dilution_factors(samples, dilution_factors)
  } else {
    # Create Final_Concentration as copy of Capped_Concentration if no dilution
    samples[[COL_FINAL_CONC]] <- samples[[COL_CAPPED_CONC]]
  }

  # Calculate confidence intervals for predictions
  message("  Calculating prediction confidence intervals...")
  tryCatch({
    ci_results <- calculate_prediction_ci(
      response = samples[[COL_RESPONSE]],
      model = model_selection$best_model,
      std_data = standards,
      level = DEFAULT_CI_LEVEL
    )

    if (!is.null(ci_results) && nrow(ci_results) == nrow(samples)) {
      samples[[COL_CI_LOWER]] <- ci_results$CI_Lower
      samples[[COL_CI_UPPER]] <- ci_results$CI_Upper
    }
  }, error = function(e) {
    message("  Warning: Could not calculate confidence intervals - ", e$message)
  })

  # Calculate sample summaries
  samples_summary <- summarize_samples(samples, std_range)

  # Generate curve data for plotting
  message("\n=== Generating Visualizations ===")
  curve_data_list <- generate_all_curves(fitted_models, std_range)

  # Generate plots (using original plotting functions for now)
  y_axis_label <- if (skip_normalization) "OD (corrected)" else "Normalized OD (B/B0)"

  plots_actual <- create_plots(
    std_summary, samples_summary$actual, curve_data_list,
    equations, std_range, y_axis_label, use_capped_data = FALSE
  )

  plots_capped <- create_plots(
    std_summary, samples_summary$capped, curve_data_list,
    equations, std_range, y_axis_label, use_capped_data = TRUE
  )

  message("\n=== Analysis Complete ===\n")

  # Generate QC summary
  qc_summary <- tryCatch({
    generate_qc_summary(
      standards_summary = standards_accuracy,
      assay_limits = assay_limits,
      model_metrics = model_comparison
    )
  }, error = function(e) {
    list(overall_status = "UNKNOWN", message = e$message)
  })

  # Return comprehensive results
  return(list(
    # Models
    models = fitted_models,
    best_model = model_selection$best_model,
    best_model_name = best_model_name,
    model_comparison = model_comparison,
    equations = equations,

    # Data
    standards = standards,
    samples = samples,
    std_summary = std_summary,
    sample_summary = samples_summary$combined,

    # Quality Control (NEW)
    assay_limits = assay_limits,
    standards_accuracy = standards_accuracy,
    qc_summary = qc_summary,

    # Plots
    plots_actual = plots_actual,
    plots_capped = plots_capped,

    # Metadata
    assay_type = final_assay_type,
    assay_detection = assay_result$detection_result,
    std_range = std_range,
    controls = controls,
    skip_normalization = skip_normalization,
    use_corrected = use_corrected,
    log_transform = log_transform,
    weight_type = weight_type,
    dilution_factors = dilution_factors,
    config = config
  ))
}

#' Apply dilution factors to sample concentrations
#'
#' @param samples Samples data frame with Capped_Concentration
#' @param dilution_factors Named vector or data frame with dilution factors
#' @return Samples with Final_Concentration column
apply_dilution_factors <- function(samples, dilution_factors) {
  if (is.null(dilution_factors)) {
    samples[[COL_FINAL_CONC]] <- samples[[COL_CAPPED_CONC]]
    return(samples)
  }

  # Initialize Final_Concentration
  samples[[COL_DILUTION_FACTOR]] <- 1
  samples[[COL_FINAL_CONC]] <- samples[[COL_CAPPED_CONC]]

  # If dilution_factors is a named vector
  if (is.vector(dilution_factors) && !is.null(names(dilution_factors))) {
    for (sample_type in names(dilution_factors)) {
      idx <- samples[[COL_TYPE]] == sample_type
      if (any(idx)) {
        samples[[COL_DILUTION_FACTOR]][idx] <- dilution_factors[sample_type]
        samples[[COL_FINAL_CONC]][idx] <- samples[[COL_CAPPED_CONC]][idx] * dilution_factors[sample_type]
      }
    }
  }

  # If dilution_factors is a data frame with Type and Dilution columns
  if (is.data.frame(dilution_factors)) {
    if (all(c("Type", "Dilution") %in% colnames(dilution_factors))) {
      for (i in seq_len(nrow(dilution_factors))) {
        sample_type <- dilution_factors$Type[i]
        factor <- dilution_factors$Dilution[i]

        idx <- samples[[COL_TYPE]] == sample_type
        if (any(idx)) {
          samples[[COL_DILUTION_FACTOR]][idx] <- factor
          samples[[COL_FINAL_CONC]][idx] <- samples[[COL_CAPPED_CONC]][idx] * factor
        }
      }
    }
  }

  return(samples)
}

#' Apply concentration capping for out-of-range and uncalculatable samples
#'
#' @param samples Samples data frame with predictions
#' @param std_range Standard curve range
#' @param standards Standards data frame
#' @return Samples with capped concentrations
apply_concentration_capping <- function(samples, std_range, standards) {
  # Get unique standard concentrations for capping
  std_concentrations <- sort(unique(standards[[COL_CONCENTRATION]]))

  # Initialize capped concentration column
  samples[[COL_CAPPED_CONC]] <- samples[[COL_PRED_CONC]]

  # Get nearest standard concentration for NA values
  na_idx <- which(is.na(samples[[COL_PRED_CONC]]))
  if (length(na_idx) > 0) {
    # Use median standard concentration for uncalculatable samples
    samples[[COL_CAPPED_CONC]][na_idx] <- median(std_concentrations, na.rm = TRUE)
  }

  # Cap values below range
  below_idx <- which(!is.na(samples[[COL_PRED_CONC]]) & samples[[COL_PRED_CONC]] < std_range[1])
  if (length(below_idx) > 0) {
    samples[[COL_CAPPED_CONC]][below_idx] <- std_concentrations[1]
  }

  # Cap values above range
  above_idx <- which(!is.na(samples[[COL_PRED_CONC]]) & samples[[COL_PRED_CONC]] > std_range[2])
  if (length(above_idx) > 0) {
    samples[[COL_CAPPED_CONC]][above_idx] <- std_concentrations[length(std_concentrations)]
  }

  # Add flags
  samples[[COL_IS_CAPPED]] <- samples[[COL_CAPPED_CONC]] != samples[[COL_PRED_CONC]] | is.na(samples[[COL_PRED_CONC]])
  samples[[COL_IS_UNRELIABLE]] <- samples[[COL_IS_CAPPED]] | samples[[COL_OUT_OF_RANGE]]

  return(samples)
}
