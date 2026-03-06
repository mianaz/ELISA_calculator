# ELISA Model Selection Module
# Functions for comparing and selecting the best model

#' Fit all available models to data
#'
#' @param data Data frame with standards data
#' @param log_transform Whether to use log transformation
#' @param models_to_fit Character vector of model types to fit (default: c("4PL", "5PL", "Linear"))
#' @return List of fitted model objects
fit_all_models <- function(data, log_transform = TRUE,
                           models_to_fit = c(MODEL_4PL, MODEL_5PL, MODEL_LINEAR)) {
  fitted_models <- list()

  # Fit 4PL model if requested
  if (MODEL_4PL %in% models_to_fit) {
    message("Fitting 4PL model...")
    model_4pl <- Elisa4PLModel$new(log_transform = log_transform)
    model_4pl$fit(data)

    if (model_4pl$is_valid()) {
      fitted_models[[MODEL_4PL]] <- model_4pl
      message("  4PL model fitted successfully")
    } else {
      message("  4PL model fitting failed: ", model_4pl$fit_error)
    }
  }

  # Fit 5PL model if requested
  if (MODEL_5PL %in% models_to_fit) {
    message("Fitting 5PL model...")
    model_5pl <- Elisa5PLModel$new(log_transform = log_transform)
    model_5pl$fit(data)

    if (model_5pl$is_valid()) {
      fitted_models[[MODEL_5PL]] <- model_5pl
      message("  5PL model fitted successfully")

      # Check if 5PL is essentially 4PL (g ≈ 1)
      if (model_5pl$is_symmetric(tolerance = 0.15)) {
        message("  Note: 5PL asymmetry parameter (g) is close to 1 - curve is nearly symmetric")
      }
    } else {
      message("  5PL model fitting failed: ", model_5pl$fit_error)
    }
  }

  # Fit Linear model if requested
  if (MODEL_LINEAR %in% models_to_fit) {
    message("Fitting Linear model...")
    model_linear <- ElisaLinearModel$new(log_transform = log_transform)
    model_linear$fit(data)

    if (model_linear$is_valid()) {
      fitted_models[[MODEL_LINEAR]] <- model_linear
      message("  Linear model fitted successfully")
    } else {
      message("  Linear model fitting failed: ", model_linear$fit_error)
    }
  }

  return(fitted_models)
}

#' Compare fitted models and create comparison table
#'
#' @param fitted_models List of fitted model objects
#' @return Data frame with model comparison metrics
compare_models <- function(fitted_models) {
  if (length(fitted_models) == 0) {
    warning("No fitted models to compare")
    return(data.frame(
      Model = character(0),
      R_squared = numeric(0),
      RMSE = numeric(0),
      AIC = numeric(0),
      LogLik = numeric(0),
      N_obs = integer(0)
    ))
  }

  comparison_rows <- list()

  for (model_name in names(fitted_models)) {
    model <- fitted_models[[model_name]]

    if (!model$is_valid()) {
      next
    }

    # Get quality metrics
    metrics <- model$get_quality_metrics()

    if (!is.null(metrics)) {
      comparison_rows[[model_name]] <- data.frame(
        Model = model_name,
        R_squared = metrics$r_squared,
        RMSE = metrics$rmse,
        AIC = metrics$aic,
        LogLik = metrics$loglik,
        N_obs = metrics$n_obs,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(comparison_rows) == 0) {
    warning("No valid model metrics available")
    return(data.frame(
      Model = character(0),
      R_squared = numeric(0),
      RMSE = numeric(0),
      AIC = numeric(0),
      LogLik = numeric(0),
      N_obs = integer(0)
    ))
  }

  # Combine all rows
  comparison_table <- do.call(rbind, comparison_rows)
  rownames(comparison_table) <- NULL

  # Sort by AIC (lower is better)
  comparison_table <- comparison_table %>%
    arrange(AIC)

  return(comparison_table)
}

#' Select the best model based on quality metrics
#'
#' @param fitted_models List of fitted model objects
#' @param criterion Selection criterion ("aic", "r2", or "auto")
#' @return List with best model object and selection details
select_best_model <- function(fitted_models, criterion = "aic") {
  if (length(fitted_models) == 0) {
    return(list(
      best_model = NULL,
      best_model_name = NULL,
      criterion = criterion,
      message = "No fitted models available"
    ))
  }

  # Get comparison table
  comparison <- compare_models(fitted_models)

  if (nrow(comparison) == 0) {
    return(list(
      best_model = NULL,
      best_model_name = NULL,
      criterion = criterion,
      message = "No valid models to compare"
    ))
  }

  # Select best model based on criterion
  best_model_name <- NULL

  if (criterion == "aic" || criterion == "auto") {
    # Use AIC (lower is better)
    best_model_name <- comparison$Model[which.min(comparison$AIC)]
    selection_message <- paste0(
      "Selected ", best_model_name, " based on AIC (AIC = ",
      round(comparison$AIC[comparison$Model == best_model_name], 2), ")"
    )
  } else if (criterion == "r2") {
    # Use R² (higher is better)
    best_model_name <- comparison$Model[which.max(comparison$R_squared)]
    selection_message <- paste0(
      "Selected ", best_model_name, " based on R² (R² = ",
      round(comparison$R_squared[comparison$Model == best_model_name], 4), ")"
    )
  } else {
    warning("Unknown criterion: ", criterion, ". Using AIC instead.")
    best_model_name <- comparison$Model[which.min(comparison$AIC)]
    selection_message <- paste0(
      "Selected ", best_model_name, " based on AIC (default)"
    )
  }

  message(selection_message)

  return(list(
    best_model = fitted_models[[best_model_name]],
    best_model_name = best_model_name,
    criterion = criterion,
    comparison = comparison,
    message = selection_message
  ))
}

#' Get model equations for all fitted models
#'
#' @param fitted_models List of fitted model objects
#' @return Named list of equation strings
get_model_equations <- function(fitted_models) {
  equations <- list()

  for (model_name in names(fitted_models)) {
    model <- fitted_models[[model_name]]

    if (model$is_valid()) {
      equations[[model_name]] <- model$get_equation()
    }
  }

  return(equations)
}

#' Predict concentrations for samples using best model with fallback
#'
#' @param sample_responses Numeric vector of sample response values
#' @param fitted_models List of fitted model objects
#' @param best_model_name Name of best model to try first
#' @param std_range Standard curve range (min, max concentrations)
#' @return Data frame with predictions for each sample
predict_with_fallback <- function(sample_responses, fitted_models, best_model_name, std_range) {
  # Handle empty input gracefully
  if (length(sample_responses) == 0) {
    return(data.frame(
      Response = numeric(0),
      Predicted_Concentration = numeric(0),
      Model_Used = character(0),
      Out_of_Range = logical(0),
      Error_Message = character(0),
      stringsAsFactors = FALSE
    ))
  }

  predictions <- data.frame(
    Response = sample_responses,
    Predicted_Concentration = NA_real_,
    Model_Used = NA_character_,
    Out_of_Range = FALSE,
    Error_Message = NA_character_,
    stringsAsFactors = FALSE
  )

  # Try best model first (guard against NULL/empty name)
  best_model <- NULL
  if (!is.null(best_model_name) && nchar(best_model_name) > 0 && best_model_name %in% names(fitted_models)) {
    best_model <- fitted_models[[best_model_name]]
  }

  for (i in seq_along(sample_responses)) {
    response <- sample_responses[i]

    if (is.na(response)) {
      predictions$Error_Message[i] <- "NA response value"
      next
    }

    # Try best model
    concentration <- NULL
    model_used <- NULL

    if (!is.null(best_model) && best_model$is_valid()) {
      tryCatch({
        concentration <- best_model$predict_concentration(response)
        model_used <- best_model_name
      }, error = function(e) {
        # Best model failed, will try fallback
      })
    }

    # If best model failed, try other models
    if (is.null(concentration) || is.na(concentration)) {
      for (fallback_name in names(fitted_models)) {
        if (fallback_name == best_model_name) next  # Already tried

        fallback_model <- fitted_models[[fallback_name]]

        if (!is.null(fallback_model) && fallback_model$is_valid()) {
          tryCatch({
            concentration <- fallback_model$predict_concentration(response)
            model_used <- paste0(fallback_name, " (fallback)")
            break  # Successfully predicted, stop trying
          }, error = function(e) {
            # This fallback also failed, try next
          })
        }
      }
    }

    # Store results
    if (!is.null(concentration) && !is.na(concentration)) {
      predictions$Predicted_Concentration[i] <- concentration
      predictions$Model_Used[i] <- model_used

      # Check if out of range
      if (concentration < std_range[1] || concentration > std_range[2]) {
        predictions$Out_of_Range[i] <- TRUE
      }
    } else {
      predictions$Model_Used[i] <- "Uncalculatable"
      predictions$Error_Message[i] <- "All models failed to predict concentration"
    }
  }

  return(predictions)
}

#' Generate curve data for all fitted models
#'
#' @param fitted_models List of fitted model objects
#' @param std_range Standard curve range
#' @param n_points Number of points for curve
#' @return Data frame with curve data for all models
generate_all_curves <- function(fitted_models, std_range, n_points = 100) {
  curve_data_list <- list()

  # Extend range for visualization
  extended_range <- c(std_range[1] / 10, std_range[2] * 10)

  for (model_name in names(fitted_models)) {
    model <- fitted_models[[model_name]]

    if (model$is_valid()) {
      tryCatch({
        curve_data <- model$generate_curve(extended_range, n_points)
        curve_data_list[[model_name]] <- curve_data
      }, error = function(e) {
        warning(paste("Error generating curve for", model_name, ":", e$message))
      })
    }
  }

  # Return the list of curve data (not combined)
  # This allows create_plots to handle the combination
  return(curve_data_list)
}
