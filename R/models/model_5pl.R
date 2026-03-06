# ELISA 5-Parameter Logistic (5PL) Model
# R6 class for fitting 5PL log-logistic dose-response curves with asymmetry parameter

library(drc)
library(R6)

#' Five-Parameter Log-Logistic Model for ELISA
#'
#' @description
#' 5PL log-logistic model using drc::LL.5()
#' Formula: f(x) = c + (d-c)/(1+(x/e)^b)^f
#' where:
#'   b = slope (Hill slope)
#'   c = lower asymptote (minimum response)
#'   d = upper asymptote (maximum response)
#'   e = ED50 (inflection point)
#'   f = asymmetry factor (f=1 reduces to 4PL)
#'
#' The 5PL model is preferred when dose-response curves are asymmetric,
#' which is common in immunoassays with hook effects or matrix interference.
#'
#' Note: LL.5() handles log-transformation internally, so we always
#' fit using Concentration directly (not pre-log-transformed).
#'
#' @export
Elisa5PLModel <- R6Class("Elisa5PLModel",
  inherit = ElisaModel,

  public = list(
    #' @field parameters Named list of 5PL parameters (b, c, d, e, f)
    #' Using drc naming: b=slope, c=lower, d=upper, e=ED50, f=asymmetry
    parameters = NULL,

    #' Initialize 5PL model
    #'
    #' @param log_transform Whether to use log-logistic (TRUE) or plain logistic (FALSE)
    initialize = function(log_transform = TRUE) {
      super$initialize(model_type = MODEL_5PL, log_transform = log_transform)
      self$parameters <- list(b = NA, c = NA, d = NA, e = NA, f = NA)
    },

    #' Fit 5PL model to data
    #'
    #' @param data Data frame with Concentration and Response columns
    #' @return Self (for method chaining)
    fit = function(data) {
      # Prepare data - remove rows with NA values
      # Always use Concentration directly - LL.5() handles log internally
      clean_data <- data %>%
        filter(
          !is.na(!!sym(COL_CONCENTRATION)),
          !is.na(!!sym(COL_RESPONSE)),
          is.finite(!!sym(COL_CONCENTRATION)),
          is.finite(!!sym(COL_RESPONSE)),
          !!sym(COL_CONCENTRATION) > 0  # Need positive concentrations for log-logistic
        )

      # Store the cleaned data used for fitting (for accurate R² calculation)
      self$model_data <- clean_data

      if (nrow(clean_data) < 5) {
        self$fit_error <- "Insufficient data points for 5PL fitting (need at least 5)"
        return(self)
      }

      # Fit model using LL.5 (log-logistic) or L.5 (plain logistic)
      tryCatch({
        # Use weights if available in data
        wts <- if ("Weights" %in% colnames(clean_data)) clean_data$Weights else NULL

        if (self$log_transform) {
          # Use LL.5() - log-logistic 5-parameter model (standard for dose-response)
          self$fitted_model <- drm(
            as.formula(paste(COL_RESPONSE, "~", COL_CONCENTRATION)),
            data = clean_data,
            fct = LL.5(),
            weights = wts,
            control = drmc(errorm = FALSE, maxIt = 500)
          )
        } else {
          # Use L.5() - plain 5-parameter logistic (rarely needed)
          self$fitted_model <- drm(
            as.formula(paste(COL_RESPONSE, "~", COL_CONCENTRATION)),
            data = clean_data,
            fct = L.5(),
            weights = wts,
            control = drmc(errorm = FALSE, maxIt = 500)
          )
        }

        # Extract parameters
        self$extract_parameters()
        self$is_fitted <- TRUE

      }, error = function(e) {
        self$fit_error <- paste("5PL fitting failed:", e$message)
        self$is_fitted <- FALSE
      })

      return(self)
    },

    #' Extract 5PL parameters from fitted model
    #'
    #' @return NULL (updates self$parameters)
    extract_parameters = function() {
      if (is.null(self$fitted_model)) {
        return(NULL)
      }

      tryCatch({
        params <- coef(self$fitted_model)

        # drc package LL.5() parameter naming: b, c, d, e, f
        # where: b=slope, c=lower, d=upper, e=ED50, f=asymmetry
        if ("b:(Intercept)" %in% names(params)) {
          self$parameters$b <- unname(params["b:(Intercept)"])  # slope
          self$parameters$c <- unname(params["c:(Intercept)"])  # lower asymptote
          self$parameters$d <- unname(params["d:(Intercept)"])  # upper asymptote
          self$parameters$e <- unname(params["e:(Intercept)"])  # ED50
          self$parameters$f <- unname(params["f:(Intercept)"])  # asymmetry
        } else {
          # Generic fallback - assume order [b, c, d, e, f]
          self$parameters$b <- unname(params[1])  # slope
          self$parameters$c <- unname(params[2])  # lower
          self$parameters$d <- unname(params[3])  # upper
          self$parameters$e <- unname(params[4])  # ED50
          self$parameters$f <- unname(params[5])  # asymmetry
          self$fit_warnings <- c(self$fit_warnings, "Could not identify parameter names, used positional assignment")
        }

      }, error = function(e) {
        self$fit_warnings <- c(self$fit_warnings, paste("Error extracting parameters:", e$message))
      })
    },

    #' Predict response for given concentrations
    #'
    #' @param concentrations Numeric vector of concentration values
    #' @return Numeric vector of predicted responses
    predict_response = function(concentrations) {
      if (!self$is_fitted) {
        warning("Model has not been fitted yet")
        return(rep(NA, length(concentrations)))
      }

      tryCatch({
        # Always use Concentration directly - model handles transformation
        newdata <- data.frame(Concentration = concentrations)
        names(newdata) <- COL_CONCENTRATION
        predict(self$fitted_model, newdata = newdata)
      }, error = function(e) {
        warning(paste("Error predicting response:", e$message))
        rep(NA, length(concentrations))
      })
    },

    #' Predict concentration for given response (inverse prediction)
    #'
    #' @param response Numeric value of response
    #' @return Predicted concentration
    predict_concentration = function(response) {
      if (!self$is_fitted) {
        warning("Model has not been fitted yet")
        return(NA)
      }

      # For LL.5/L.5: f(x) = c + (d-c)/(1+(x/e)^b)^f
      # Inverse: x = e * (((d-c)/(y-c))^(1/f) - 1)^(1/b)
      tryCatch({
        b <- self$parameters$b
        c <- self$parameters$c  # lower asymptote
        d <- self$parameters$d  # upper asymptote
        e <- self$parameters$e  # ED50
        f <- self$parameters$f  # asymmetry

        # Check for valid parameters
        if (any(is.na(c(b, c, d, e, f)))) {
          return(NA)
        }

        # Check response is within asymptotes
        if (d > c) {
          # Increasing curve
          if (response <= c || response >= d) {
            warning("Response is outside model asymptotes")
            return(NA)
          }
        } else {
          # Decreasing curve
          if (response >= c || response <= d) {
            warning("Response is outside model asymptotes")
            return(NA)
          }
        }

        # Calculate the ratio term: (d-c)/(y-c)
        ratio <- (d - c) / (response - c)

        # Check for valid ratio
        if (ratio <= 0) {
          warning("Cannot calculate concentration: invalid ratio term")
          return(NA)
        }

        # Apply asymmetry correction: ratio^(1/f) - 1
        inner <- ratio^(1/f) - 1

        if (inner <= 0) {
          warning("Cannot calculate concentration: invalid inner term after asymmetry correction")
          return(NA)
        }

        # Calculate concentration: x = e * inner^(1/b)
        concentration <- e * (inner)^(1/b)

        # Sanity check
        if (!is.finite(concentration) || concentration <= 0) {
          return(NA)
        }

        return(concentration)

      }, error = function(e) {
        warning(paste("Error predicting concentration:", e$message))
        return(NA)
      })
    },

    #' Get model equation as a string
    #'
    #' @return Character string with model equation
    get_equation = function() {
      if (!self$is_fitted) {
        return("5PL model not fitted")
      }

      b <- self$parameters$b
      c <- self$parameters$c
      d <- self$parameters$d
      e <- self$parameters$e
      f <- self$parameters$f

      if (any(is.na(c(b, c, d, e, f)))) {
        return("5PL model parameters not available")
      }

      # Get quality metrics
      metrics <- self$get_quality_metrics()
      r_squared_text <- if (!is.null(metrics)) {
        paste0(", R² = ", round(metrics$r_squared, 4))
      } else {
        ""
      }

      paste0(
        "y = ", round(c, 4), " + (",
        round(d, 4), " - ", round(c, 4),
        ")/(1 + (x/", round(e, 2), ")^", round(b, 3), ")^", round(f, 3),
        r_squared_text
      )
    },

    #' Get model parameters
    #'
    #' @return Named list of model parameters
    get_parameters = function() {
      if (!self$is_fitted) {
        warning("Model has not been fitted yet")
        return(NULL)
      }
      return(self$parameters)
    },

    #' Check if model is essentially 4PL (f ≈ 1)
    #'
    #' @param tolerance Tolerance for comparing f to 1
    #' @return Logical indicating if model is essentially symmetric
    is_symmetric = function(tolerance = 0.1) {
      if (!self$is_fitted || is.na(self$parameters$f)) {
        return(NA)
      }
      return(abs(self$parameters$f - 1) < tolerance)
    },

    #' Get asymmetry assessment
    #'
    #' @return Character description of curve asymmetry
    get_asymmetry_description = function() {
      if (!self$is_fitted || is.na(self$parameters$f)) {
        return("Cannot assess asymmetry")
      }

      f <- self$parameters$f

      if (abs(f - 1) < 0.1) {
        return("Symmetric (5PL equivalent to 4PL)")
      } else if (f > 1) {
        return(paste0("Right-skewed (f = ", round(f, 3), ")"))
      } else {
        return(paste0("Left-skewed (f = ", round(f, 3), ")"))
      }
    }
  )
)
