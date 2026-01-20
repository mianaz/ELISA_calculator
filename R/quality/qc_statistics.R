# ELISA Quality Control Statistics Module
# Functions for LOD/LOQ/ULOQ, recovery, back-calculation, outlier detection, and confidence intervals

# Source dependencies
source("R/utils/helpers.R")
source("R/utils/constants.R")

#' Calculate Limit of Detection (LOD)
#'
#' @description
#' LOD is typically calculated as:
#' - Method 1: Mean_Blank + 3 * SD_Blank (signal-based)
#' - Method 2: From standard curve at low end (curve-based)
#'
#' @param blank_values Vector of blank OD values
#' @param model Fitted model object for curve-based calculation
#' @param method Calculation method: "signal" or "curve"
#' @param multiplier SD multiplier (default: 3 for LOD)
#' @return LOD value (concentration units if curve-based, OD if signal-based)
#' @export
calculate_lod <- function(blank_values = NULL, model = NULL, method = "signal", multiplier = 3) {
  if (method == "signal" && !is.null(blank_values)) {
    # Signal-based LOD: Mean + 3*SD
    blank_values <- blank_values[!is.na(blank_values)]

    if (length(blank_values) < 2) {
      warning("Insufficient blank values for LOD calculation")
      return(NA)
    }

    mean_blank <- mean(blank_values, na.rm = TRUE)
    sd_blank <- sd(blank_values, na.rm = TRUE)

    lod_signal <- mean_blank + multiplier * sd_blank

    # If model provided, convert to concentration
    if (!is.null(model) && model$is_valid()) {
      tryCatch({
        lod_conc <- model$predict_concentration(lod_signal)
        return(list(
          lod_signal = lod_signal,
          lod_concentration = lod_conc,
          method = "signal-based",
          mean_blank = mean_blank,
          sd_blank = sd_blank,
          multiplier = multiplier
        ))
      }, error = function(e) {
        return(list(
          lod_signal = lod_signal,
          lod_concentration = NA,
          method = "signal-based",
          mean_blank = mean_blank,
          sd_blank = sd_blank,
          multiplier = multiplier,
          error = e$message
        ))
      })
    }

    return(list(
      lod_signal = lod_signal,
      lod_concentration = NA,
      method = "signal-based",
      mean_blank = mean_blank,
      sd_blank = sd_blank,
      multiplier = multiplier
    ))

  } else if (method == "curve" && !is.null(model)) {
    # Curve-based LOD using model parameters
    # This requires the model to extrapolate to low concentrations
    warning("Curve-based LOD calculation not yet implemented")
    return(NA)
  }

  warning("Invalid parameters for LOD calculation")
  return(NA)
}

#' Calculate Limit of Quantitation (LOQ)
#'
#' @description
#' LOQ is typically calculated as:
#' Mean_Blank + 10 * SD_Blank (signal-based)
#' Or the lowest standard with CV < 20% and accuracy within 80-120%
#'
#' @param blank_values Vector of blank OD values
#' @param standards_data Standards data frame with replicates
#' @param model Fitted model object
#' @param method Calculation method: "signal" or "accuracy"
#' @param multiplier SD multiplier (default: 10 for LOQ)
#' @param cv_threshold Maximum acceptable CV (default: 0.20)
#' @param accuracy_range Acceptable accuracy range (default: c(80, 120))
#' @return LOQ value
#' @export
calculate_loq <- function(blank_values = NULL, standards_data = NULL, model = NULL,
                          method = "signal", multiplier = 10,
                          cv_threshold = 0.20, accuracy_range = c(80, 120)) {

  if (method == "signal" && !is.null(blank_values)) {
    # Signal-based LOQ
    blank_values <- blank_values[!is.na(blank_values)]

    if (length(blank_values) < 2) {
      warning("Insufficient blank values for LOQ calculation")
      return(NA)
    }

    mean_blank <- mean(blank_values, na.rm = TRUE)
    sd_blank <- sd(blank_values, na.rm = TRUE)

    loq_signal <- mean_blank + multiplier * sd_blank

    # Convert to concentration if model provided
    if (!is.null(model) && model$is_valid()) {
      tryCatch({
        loq_conc <- model$predict_concentration(loq_signal)
        return(list(
          loq_signal = loq_signal,
          loq_concentration = loq_conc,
          method = "signal-based",
          mean_blank = mean_blank,
          sd_blank = sd_blank,
          multiplier = multiplier
        ))
      }, error = function(e) {
        return(list(
          loq_signal = loq_signal,
          loq_concentration = NA,
          method = "signal-based",
          error = e$message
        ))
      })
    }

    return(list(
      loq_signal = loq_signal,
      loq_concentration = NA,
      method = "signal-based"
    ))

  } else if (method == "accuracy" && !is.null(standards_data) && !is.null(model)) {
    # Accuracy-based LOQ: lowest standard with acceptable precision and accuracy
    tryCatch({
      # Calculate back-calculated concentrations and CV for each standard level
      std_stats <- standards_data %>%
        group_by(Concentration) %>%
        summarize(
          n = n(),
          mean_response = mean(Response, na.rm = TRUE),
          cv = calculate_cv(Response),
          .groups = "drop"
        ) %>%
        arrange(Concentration)

      # Back-calculate concentrations
      std_stats$back_calc <- sapply(std_stats$mean_response, function(r) {
        model$predict_concentration(r)
      })

      # Calculate %accuracy
      std_stats$accuracy <- (std_stats$back_calc / std_stats$Concentration) * 100

      # Find lowest standard meeting criteria
      valid_stds <- std_stats %>%
        filter(
          cv <= cv_threshold,
          accuracy >= accuracy_range[1],
          accuracy <= accuracy_range[2]
        )

      if (nrow(valid_stds) > 0) {
        loq_conc <- min(valid_stds$Concentration)
        return(list(
          loq_concentration = loq_conc,
          method = "accuracy-based",
          cv_threshold = cv_threshold,
          accuracy_range = accuracy_range,
          std_stats = std_stats
        ))
      } else {
        warning("No standards meet LOQ criteria")
        return(list(
          loq_concentration = NA,
          method = "accuracy-based",
          message = "No standards meet precision/accuracy criteria"
        ))
      }

    }, error = function(e) {
      warning(paste("Error in accuracy-based LOQ calculation:", e$message))
      return(NA)
    })
  }

  warning("Invalid parameters for LOQ calculation")
  return(NA)
}

#' Calculate Upper Limit of Quantitation (ULOQ)
#'
#' @description
#' ULOQ is the highest standard concentration with acceptable precision and accuracy
#'
#' @param standards_data Standards data frame
#' @param model Fitted model object
#' @param cv_threshold Maximum acceptable CV
#' @param accuracy_range Acceptable accuracy range
#' @return ULOQ value
#' @export
calculate_uloq <- function(standards_data, model = NULL,
                           cv_threshold = 0.20, accuracy_range = c(80, 120)) {

  if (is.null(standards_data) || nrow(standards_data) == 0) {
    warning("No standards data for ULOQ calculation")
    return(NA)
  }

  tryCatch({
    # Calculate statistics for each standard level
    std_stats <- standards_data %>%
      group_by(Concentration) %>%
      summarize(
        n = n(),
        mean_response = mean(Response, na.rm = TRUE),
        cv = calculate_cv(Response),
        .groups = "drop"
      ) %>%
      arrange(desc(Concentration))

    # Back-calculate if model provided
    if (!is.null(model) && model$is_valid()) {
      std_stats$back_calc <- sapply(std_stats$mean_response, function(r) {
        model$predict_concentration(r)
      })
      std_stats$accuracy <- (std_stats$back_calc / std_stats$Concentration) * 100

      # Find highest standard meeting criteria
      valid_stds <- std_stats %>%
        filter(
          cv <= cv_threshold,
          accuracy >= accuracy_range[1],
          accuracy <= accuracy_range[2]
        )

      if (nrow(valid_stds) > 0) {
        uloq_conc <- max(valid_stds$Concentration)
        return(list(
          uloq_concentration = uloq_conc,
          method = "accuracy-based",
          std_stats = std_stats
        ))
      }
    }

    # Fallback: use highest standard with acceptable CV
    valid_stds <- std_stats %>%
      filter(cv <= cv_threshold)

    if (nrow(valid_stds) > 0) {
      uloq_conc <- max(valid_stds$Concentration)
      return(list(
        uloq_concentration = uloq_conc,
        method = "cv-based"
      ))
    }

    # Last resort: just use highest standard
    return(list(
      uloq_concentration = max(standards_data$Concentration, na.rm = TRUE),
      method = "default-highest",
      warning = "No standards meet precision criteria, using highest standard"
    ))

  }, error = function(e) {
    warning(paste("Error calculating ULOQ:", e$message))
    return(NA)
  })
}

#' Calculate comprehensive assay limits (LOD, LOQ, ULOQ)
#'
#' @param blank_values Vector of blank response values
#' @param standards_data Standards data frame
#' @param model Fitted model object
#' @param config Configuration list
#' @return List with LOD, LOQ, ULOQ and metadata
#' @export
calculate_assay_limits <- function(blank_values, standards_data, model, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  # Calculate LOD
  lod_result <- calculate_lod(
    blank_values = blank_values,
    model = model,
    method = "signal",
    multiplier = DEFAULT_LOD_MULTIPLIER
  )

  # Calculate LOQ using both methods
  loq_signal <- calculate_loq(
    blank_values = blank_values,
    model = model,
    method = "signal",
    multiplier = DEFAULT_LOQ_MULTIPLIER
  )

  loq_accuracy <- calculate_loq(
    standards_data = standards_data,
    model = model,
    method = "accuracy"
  )

  # Calculate ULOQ
  uloq_result <- calculate_uloq(
    standards_data = standards_data,
    model = model
  )

  # Use the more conservative (higher) LOQ
  loq_conc <- NA
  if (is.list(loq_signal) && !is.na(loq_signal$loq_concentration)) {
    loq_conc <- loq_signal$loq_concentration
  }
  if (is.list(loq_accuracy) && !is.na(loq_accuracy$loq_concentration)) {
    if (is.na(loq_conc) || loq_accuracy$loq_concentration > loq_conc) {
      loq_conc <- loq_accuracy$loq_concentration
    }
  }

  # Extract concentrations
  lod_conc <- if (is.list(lod_result)) lod_result$lod_concentration else NA
  uloq_conc <- if (is.list(uloq_result)) uloq_result$uloq_concentration else NA

  return(list(
    lod = list(
      concentration = lod_conc,
      details = lod_result
    ),
    loq = list(
      concentration = loq_conc,
      signal_method = loq_signal,
      accuracy_method = loq_accuracy
    ),
    uloq = list(
      concentration = uloq_conc,
      details = uloq_result
    ),
    dynamic_range = if (!is.na(loq_conc) && !is.na(uloq_conc)) {
      list(
        lower = loq_conc,
        upper = uloq_conc,
        fold_range = uloq_conc / loq_conc
      )
    } else NULL
  ))
}

#' Calculate back-calculated concentrations and %RE for standards
#'
#' @param standards_data Standards data frame with Response column
#' @param model Fitted model object
#' @return Data frame with back-calculated values and accuracy metrics
#' @export
calculate_back_calculation <- function(standards_data, model) {
  if (is.null(standards_data) || nrow(standards_data) == 0) {
    warning("No standards data provided")
    return(NULL)
  }

  if (is.null(model) || !model$is_valid()) {
    warning("Invalid model for back-calculation")
    return(NULL)
  }

  result <- standards_data %>%
    mutate(
      Back_Calculated_Conc = sapply(Response, function(r) {
        tryCatch({
          model$predict_concentration(r)
        }, error = function(e) NA)
      }),
      Percent_RE = ((Back_Calculated_Conc - Concentration) / Concentration) * 100,
      Recovery_Pct = (Back_Calculated_Conc / Concentration) * 100,
      Accuracy_Pass = abs(Percent_RE) <= DEFAULT_ACCURACY_THRESHOLD
    )

  return(result)
}

#' Calculate standard curve summary with back-calculation
#'
#' @param standards_data Standards data with replicates
#' @param model Fitted model
#' @return Summary data frame
#' @export
summarize_standards_with_accuracy <- function(standards_data, model) {
  if (is.null(standards_data)) return(NULL)

  # First, add back-calculation to all standards
  std_with_back_calc <- calculate_back_calculation(standards_data, model)

  if (is.null(std_with_back_calc)) return(NULL)

  # Summarize by concentration level
  summary <- std_with_back_calc %>%
    group_by(Concentration) %>%
    summarize(
      N = n(),
      Mean_Response = mean(Response, na.rm = TRUE),
      SD_Response = sd(Response, na.rm = TRUE),
      CV_Pct = calculate_cv(Response) * 100,
      Mean_Back_Calc = mean(Back_Calculated_Conc, na.rm = TRUE),
      SD_Back_Calc = sd(Back_Calculated_Conc, na.rm = TRUE),
      Mean_Percent_RE = mean(Percent_RE, na.rm = TRUE),
      Mean_Recovery_Pct = mean(Recovery_Pct, na.rm = TRUE),
      Accuracy_Pass = all(Accuracy_Pass, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(Concentration)

  # Add overall pass/fail
  summary$QC_Status <- ifelse(
    summary$CV_Pct <= (DEFAULT_CV_THRESHOLD * 100) &
      summary$Mean_Recovery_Pct >= DEFAULT_RECOVERY_MIN &
      summary$Mean_Recovery_Pct <= DEFAULT_RECOVERY_MAX,
    "PASS", "FAIL"
  )

  return(summary)
}

#' Perform Grubb's test for outlier detection
#'
#' @param x Numeric vector
#' @param alpha Significance level
#' @return List with outlier indices and test statistics
#' @export
grubbs_test <- function(x, alpha = 0.05) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 3) {
    return(list(
      has_outlier = FALSE,
      message = "Insufficient data for Grubb's test (n < 3)"
    ))
  }

  # Calculate Grubb's statistic
  mean_x <- mean(x)
  sd_x <- sd(x)

  if (sd_x == 0) {
    return(list(
      has_outlier = FALSE,
      message = "Zero variance - no outliers"
    ))
  }

  # Find maximum deviation
  deviations <- abs(x - mean_x) / sd_x
  max_idx <- which.max(deviations)
  G <- deviations[max_idx]

  # Critical value (approximation using t-distribution)
  t_crit <- qt(1 - alpha / (2 * n), n - 2)
  G_crit <- ((n - 1) / sqrt(n)) * sqrt(t_crit^2 / (n - 2 + t_crit^2))

  has_outlier <- G > G_crit

  return(list(
    has_outlier = has_outlier,
    outlier_index = if (has_outlier) max_idx else NA,
    outlier_value = if (has_outlier) x[max_idx] else NA,
    G_statistic = G,
    G_critical = G_crit,
    alpha = alpha,
    n = n
  ))
}

#' Perform Dixon's Q test for outlier detection
#'
#' @param x Numeric vector (3-25 observations)
#' @param alpha Significance level
#' @return List with outlier information
#' @export
dixon_q_test <- function(x, alpha = 0.05) {
  x <- sort(x[!is.na(x)])
  n <- length(x)

  if (n < 3 || n > 25) {
    return(list(
      has_outlier = FALSE,
      message = "Dixon's Q test requires 3-25 observations"
    ))
  }

  # Dixon's Q critical values (alpha = 0.05)
  q_critical_table <- c(
    "3" = 0.970, "4" = 0.829, "5" = 0.710, "6" = 0.628, "7" = 0.569,
    "8" = 0.608, "9" = 0.564, "10" = 0.530, "11" = 0.502, "12" = 0.479,
    "13" = 0.611, "14" = 0.586, "15" = 0.565, "16" = 0.546, "17" = 0.529,
    "18" = 0.514, "19" = 0.501, "20" = 0.489, "21" = 0.478, "22" = 0.468,
    "23" = 0.459, "24" = 0.451, "25" = 0.443
  )

  q_crit <- q_critical_table[as.character(n)]

  # Calculate Q for lowest and highest values
  range_x <- x[n] - x[1]

  if (range_x == 0) {
    return(list(
      has_outlier = FALSE,
      message = "Zero range - no outliers"
    ))
  }

  # Q ratio for lowest value
  Q_low <- (x[2] - x[1]) / range_x

  # Q ratio for highest value
  Q_high <- (x[n] - x[n-1]) / range_x

  # Determine which end has potential outlier
  if (Q_low > Q_high) {
    Q <- Q_low
    outlier_value <- x[1]
    outlier_end <- "low"
  } else {
    Q <- Q_high
    outlier_value <- x[n]
    outlier_end <- "high"
  }

  has_outlier <- Q > q_crit

  return(list(
    has_outlier = has_outlier,
    outlier_value = if (has_outlier) outlier_value else NA,
    outlier_end = if (has_outlier) outlier_end else NA,
    Q_statistic = Q,
    Q_critical = q_crit,
    alpha = alpha,
    n = n
  ))
}

#' Detect outliers in replicate measurements
#'
#' @param values Numeric vector of replicate values
#' @param method Detection method: "grubbs", "dixon", or "both"
#' @param alpha Significance level
#' @return List with outlier detection results
#' @export
detect_outliers <- function(values, method = "both", alpha = 0.05) {
  values <- values[!is.na(values)]
  n <- length(values)

  results <- list(
    n = n,
    method = method,
    alpha = alpha
  )

  if (n < 3) {
    results$message <- "Insufficient data for outlier detection"
    results$has_outlier <- FALSE
    return(results)
  }

  if (method %in% c("grubbs", "both")) {
    results$grubbs <- grubbs_test(values, alpha)
  }

  if (method %in% c("dixon", "both") && n <= 25) {
    results$dixon <- dixon_q_test(values, alpha)
  }

  # Combine results
  if (method == "both") {
    results$has_outlier <-
      (is.list(results$grubbs) && results$grubbs$has_outlier) ||
      (is.list(results$dixon) && results$dixon$has_outlier)
  } else if (method == "grubbs") {
    results$has_outlier <- is.list(results$grubbs) && results$grubbs$has_outlier
  } else {
    results$has_outlier <- is.list(results$dixon) && results$dixon$has_outlier
  }

  return(results)
}

#' Calculate confidence intervals for predicted concentrations
#'
#' @param response Response value(s)
#' @param model Fitted model
#' @param std_data Standards data used for fitting
#' @param level Confidence level (default: 0.95)
#' @param method CI method: "delta" or "bootstrap"
#' @return Data frame with predictions and confidence intervals
#' @export
calculate_prediction_ci <- function(response, model, std_data, level = 0.95, method = "delta") {
  if (is.null(model) || !model$is_valid()) {
    warning("Invalid model for CI calculation")
    return(data.frame(
      Response = response,
      Predicted_Conc = NA,
      CI_Lower = NA,
      CI_Upper = NA
    ))
  }

  # Calculate point predictions
  predictions <- sapply(response, function(r) {
    tryCatch({
      model$predict_concentration(r)
    }, error = function(e) NA)
  })

  if (method == "delta") {
    # Delta method approximation for confidence intervals
    # Using standard error propagation from the model

    # Get model residual standard error
    tryCatch({
      fitted_model <- model$fitted_model

      if (!is.null(fitted_model)) {
        # Get residual variance
        residuals <- residuals(fitted_model)
        sigma <- sqrt(sum(residuals^2) / (length(residuals) - length(coef(fitted_model))))

        # Approximate SE of inverse prediction using finite differences
        delta <- 0.001 * abs(response) + 0.001

        se_conc <- sapply(seq_along(response), function(i) {
          r <- response[i]
          pred <- predictions[i]

          if (is.na(pred)) return(NA)

          # Numerical derivative of inverse function
          pred_plus <- tryCatch(model$predict_concentration(r + delta), error = function(e) NA)
          pred_minus <- tryCatch(model$predict_concentration(r - delta), error = function(e) NA)

          if (is.na(pred_plus) || is.na(pred_minus)) return(NA)

          deriv <- (pred_plus - pred_minus) / (2 * delta)
          abs(deriv) * sigma
        })

        # Calculate CI
        z <- qnorm(1 - (1 - level) / 2)
        ci_lower <- predictions - z * se_conc
        ci_upper <- predictions + z * se_conc

        # Ensure lower bound is positive
        ci_lower <- pmax(ci_lower, 0)

        return(data.frame(
          Response = response,
          Predicted_Conc = predictions,
          SE = se_conc,
          CI_Lower = ci_lower,
          CI_Upper = ci_upper,
          CI_Level = level,
          Method = "delta"
        ))
      }
    }, error = function(e) {
      warning(paste("Error calculating delta CI:", e$message))
    })
  }

  # Fallback: return predictions without CI
  return(data.frame(
    Response = response,
    Predicted_Conc = predictions,
    CI_Lower = NA,
    CI_Upper = NA,
    CI_Level = level,
    Method = "none"
  ))
}

#' Apply weighting to data for weighted regression
#'
#' @param data Data frame with Response and Concentration columns
#' @param weight_type Weighting type: "none", "1/Y", "1/Y^2", "1/X^2"
#' @return Data frame with Weights column added
#' @export
calculate_weights <- function(data, weight_type = "none") {
  if (weight_type == "none" || weight_type == WEIGHT_NONE) {
    data$Weights <- 1
  } else if (weight_type == "1/Y" || weight_type == WEIGHT_1_Y) {
    data$Weights <- 1 / abs(data$Response + 0.001)
  } else if (weight_type == "1/Y^2" || weight_type == WEIGHT_1_Y2) {
    data$Weights <- 1 / (data$Response + 0.001)^2
  } else if (weight_type == "1/X^2" || weight_type == WEIGHT_1_X2) {
    data$Weights <- 1 / (data$Concentration + 0.001)^2
  } else {
    warning(paste("Unknown weight type:", weight_type, "- using no weighting"))
    data$Weights <- 1
  }

  # Normalize weights
  data$Weights <- data$Weights / sum(data$Weights, na.rm = TRUE) * nrow(data)

  return(data)
}

#' Create exclusion audit trail entry
#'
#' @param sample_id Sample identifier
#' @param reason Reason for exclusion
#' @param excluded_by Who/what excluded (user, algorithm, etc.)
#' @param additional_info Additional information
#' @return Audit trail entry list
#' @export
create_exclusion_entry <- function(sample_id, reason, excluded_by = "user", additional_info = NULL) {
  entry <- list(
    sample_id = sample_id,
    reason = reason,
    excluded_by = excluded_by,
    timestamp = Sys.time(),
    additional_info = additional_info
  )

  return(entry)
}

#' Manage exclusion audit trail
#'
#' @param audit_trail Existing audit trail (list)
#' @param action Action: "add", "remove", "clear"
#' @param entry Entry to add/remove
#' @return Updated audit trail
#' @export
manage_audit_trail <- function(audit_trail = NULL, action = "add", entry = NULL) {
  if (is.null(audit_trail)) {
    audit_trail <- list(
      entries = list(),
      created_at = Sys.time(),
      last_modified = Sys.time()
    )
  }

  if (action == "add" && !is.null(entry)) {
    audit_trail$entries <- c(audit_trail$entries, list(entry))
    audit_trail$last_modified <- Sys.time()
  } else if (action == "remove" && !is.null(entry$sample_id)) {
    # Remove entries for specific sample
    audit_trail$entries <- Filter(
      function(e) e$sample_id != entry$sample_id,
      audit_trail$entries
    )
    audit_trail$last_modified <- Sys.time()
  } else if (action == "clear") {
    audit_trail$entries <- list()
    audit_trail$last_modified <- Sys.time()
  }

  return(audit_trail)
}

#' Generate QC summary report
#'
#' @param standards_summary Standards summary with accuracy data
#' @param assay_limits LOD/LOQ/ULOQ results
#' @param model_metrics Model comparison metrics
#' @return QC summary list
#' @export
generate_qc_summary <- function(standards_summary = NULL, assay_limits = NULL, model_metrics = NULL) {
  summary <- list(
    generated_at = Sys.time(),
    overall_status = "PASS"
  )

  issues <- character(0)

  # Check standards accuracy
  if (!is.null(standards_summary)) {
    failed_stds <- standards_summary %>% filter(QC_Status == "FAIL")
    if (nrow(failed_stds) > 0) {
      issues <- c(issues, paste(
        nrow(failed_stds), "standard levels failed accuracy/precision criteria"
      ))
    }
    summary$standards_passing <- nrow(standards_summary) - nrow(failed_stds)
    summary$standards_total <- nrow(standards_summary)
  }

  # Check model fit
  if (!is.null(model_metrics)) {
    best_r2 <- max(model_metrics$R_squared, na.rm = TRUE)
    if (best_r2 < 0.99) {
      issues <- c(issues, paste("Best model R² =", round(best_r2, 4), "< 0.99"))
    }
    summary$best_r_squared <- best_r2
  }

  # Check assay limits
  if (!is.null(assay_limits)) {
    summary$lod <- assay_limits$lod$concentration
    summary$loq <- assay_limits$loq$concentration
    summary$uloq <- assay_limits$uloq$concentration

    if (!is.null(assay_limits$dynamic_range)) {
      summary$dynamic_range <- assay_limits$dynamic_range$fold_range
    }
  }

  # Set overall status
  if (length(issues) > 0) {
    summary$overall_status <- "WARNING"
    summary$issues <- issues
  }

  return(summary)
}
