# ELISA Model Base Class
# Base R6 class for all dose-response curve models

library(R6)

#' Base class for ELISA dose-response models
#'
#' @description
#' Abstract base class defining the interface for all curve-fitting models.
#' Specific models (4PL, Linear, etc.) should inherit from this class.
#'
#' @export
ElisaModel <- R6Class("ElisaModel",
  public = list(
    #' @field model_type Type of model (e.g., "4PL", "Linear")
    model_type = NULL,

    #' @field fitted_model The fitted model object
    fitted_model = NULL,

    #' @field model_data Data used for fitting
    model_data = NULL,

    #' @field is_fitted Logical indicating if model has been fitted
    is_fitted = FALSE,

    #' @field log_transform Logical indicating if log transformation was used
    log_transform = TRUE,

    #' @field fit_error Error message if fitting failed
    fit_error = NULL,

    #' @field fit_warnings Warning messages from fitting
    fit_warnings = character(0),

    #' Initialize the model
    #'
    #' @param model_type Model type identifier
    #' @param log_transform Whether to use log transformation
    initialize = function(model_type = "Base", log_transform = TRUE) {
      self$model_type <- model_type
      self$log_transform <- log_transform
      self$is_fitted <- FALSE
    },

    #' Fit the model to data
    #'
    #' @param data Data frame with concentration and response columns
    #' @return Self (for method chaining)
    fit = function(data) {
      stop("fit() method must be implemented by subclass")
    },

    #' Predict response for given concentrations
    #'
    #' @param concentrations Numeric vector of concentration values
    #' @return Numeric vector of predicted responses
    predict_response = function(concentrations) {
      stop("predict_response() method must be implemented by subclass")
    },

    #' Predict concentration for given response (inverse prediction)
    #'
    #' @param response Numeric value of response
    #' @return Predicted concentration
    predict_concentration = function(response) {
      stop("predict_concentration() method must be implemented by subclass")
    },

    #' Get model equation as a string
    #'
    #' @return Character string with model equation
    get_equation = function() {
      stop("get_equation() method must be implemented by subclass")
    },

    #' Get model parameters
    #'
    #' @return Named list of model parameters
    get_parameters = function() {
      if (!self$is_fitted) {
        warning("Model has not been fitted yet")
        return(NULL)
      }
      return(list())
    },

    #' Get model quality metrics
    #'
    #' @return List with R², AIC, logLik, etc.
    get_quality_metrics = function() {
      if (!self$is_fitted) {
        warning("Model has not been fitted yet")
        return(NULL)
      }

      # Calculate R-squared if we have fitted model
      if (!is.null(self$fitted_model) && !is.null(self$model_data)) {
        tryCatch({
          fitted_values <- fitted(self$fitted_model)
          observed_values <- self$model_data[[COL_RESPONSE]]

          r_squared <- calculate_r_squared(observed_values, fitted_values)
          rmse <- calculate_rmse(observed_values, fitted_values)

          # Get AIC and logLik
          aic_val <- tryCatch(AIC(self$fitted_model), error = function(e) NA)
          loglik_val <- tryCatch(logLik(self$fitted_model)[1], error = function(e) NA)

          return(list(
            model_type = self$model_type,
            r_squared = r_squared,
            rmse = rmse,
            aic = aic_val,
            loglik = loglik_val,
            n_obs = nrow(self$model_data)
          ))
        }, error = function(e) {
          warning(paste("Error calculating quality metrics:", e$message))
          return(NULL)
        })
      }

      return(NULL)
    },

    #' Generate curve data for plotting
    #'
    #' @param conc_range Range of concentrations (min, max)
    #' @param n_points Number of points to generate
    #' @return Data frame with Concentration and Response columns
    generate_curve = function(conc_range, n_points = 100) {
      if (!self$is_fitted) {
        warning("Model has not been fitted yet")
        return(data.frame(Concentration = numeric(0), Response = numeric(0)))
      }

      # Generate sequence of concentrations
      conc_seq <- log_seq(conc_range[1], conc_range[2], length.out = n_points)

      # Predict responses
      response_seq <- self$predict_response(conc_seq)

      return(data.frame(
        Concentration = conc_seq,
        Response = response_seq,
        Model = self$model_type
      ))
    },

    #' Check if model is valid and fitted
    #'
    #' @return Logical indicating if model is ready for use
    is_valid = function() {
      self$is_fitted && is.null(self$fit_error)
    },

    #' Get summary of model fit
    #'
    #' @return Character string with summary
    get_summary = function() {
      if (!self$is_fitted) {
        return(paste(self$model_type, "model: Not fitted"))
      }

      if (!is.null(self$fit_error)) {
        return(paste(self$model_type, "model: Fit failed -", self$fit_error))
      }

      metrics <- self$get_quality_metrics()
      if (!is.null(metrics)) {
        return(paste0(
          self$model_type, " model: ",
          "R² = ", round(metrics$r_squared, 4), ", ",
          "AIC = ", round(metrics$aic, 2)
        ))
      }

      return(paste(self$model_type, "model: Fitted"))
    },

    #' Print method
    #'
    #' @return Self (invisibly)
    print = function() {
      cat(self$get_summary(), "\n")
      if (length(self$fit_warnings) > 0) {
        cat("Warnings:\n")
        for (w in self$fit_warnings) {
          cat("  -", w, "\n")
        }
      }
      invisible(self)
    }
  )
)
