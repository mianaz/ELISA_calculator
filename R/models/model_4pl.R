# ELISA 4-Parameter Logistic (4PL) Model
# R6 class for fitting 4PL log-logistic dose-response curves

library(drc)

#' Four-Parameter Log-Logistic Model for ELISA
#'
#' @description
#' 4PL log-logistic model using drc::LL.4()
#' Formula: f(x) = c + (d-c)/(1+(x/e)^b)
#' where:
#'   b = slope (Hill slope)
#'   c = lower asymptote (minimum response)
#'   d = upper asymptote (maximum response)
#'   e = ED50 (inflection point, concentration at 50% effect)
#'
#' Note: LL.4() handles log-transformation internally, so we always
#' fit using Concentration directly (not pre-log-transformed).
#'
#' @export
Elisa4PLModel <- R6Class("Elisa4PLModel",
  inherit = ElisaModel,

  public = list(
    #' @field parameters Named list of 4PL parameters (b, c, d, e)
    #' Using drc naming convention: b=slope, c=lower, d=upper, e=ED50
    parameters = NULL,

    #' Initialize 4PL model
    #'
    #' @param log_transform Whether to use log-logistic (TRUE) or plain logistic (FALSE)
    initialize = function(log_transform = TRUE) {
      super$initialize(model_type = MODEL_4PL, log_transform = log_transform)
      self$parameters <- list(b = NA, c = NA, d = NA, e = NA)
    },

    #' Fit 4PL model to data
    #'
    #' @param data Data frame with Concentration and Response columns
    #' @return Self (for method chaining)
    fit = function(data) {
      # Prepare data - remove rows with NA values
      # Always use Concentration directly - LL.4() handles log internally
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

      if (nrow(clean_data) < 4) {
        self$fit_error <- "Insufficient data points for 4PL fitting (need at least 4)"
        return(self)
      }

      # Fit model using LL.4 (log-logistic) or L.4 (plain logistic)
      tryCatch({
        # Use weights if available in data
        wts <- if ("Weights" %in% colnames(clean_data)) clean_data$Weights else NULL

        if (self$log_transform) {
          # Use LL.4() - log-logistic 4-parameter model (standard for dose-response)
          self$fitted_model <- drm(
            as.formula(paste(COL_RESPONSE, "~", COL_CONCENTRATION)),
            data = clean_data,
            fct = LL.4(),
            weights = wts,
            control = drmc(errorm = FALSE)
          )
        } else {
          # Use L.4() - plain 4-parameter logistic (rarely needed)
          self$fitted_model <- drm(
            as.formula(paste(COL_RESPONSE, "~", COL_CONCENTRATION)),
            data = clean_data,
            fct = L.4(),
            weights = wts,
            control = drmc(errorm = FALSE)
          )
        }

        # Extract parameters
        self$extract_parameters()
        self$is_fitted <- TRUE

      }, error = function(e) {
        self$fit_error <- e$message
        self$is_fitted <- FALSE
      })

      return(self)
    },

    #' Extract 4PL parameters from fitted model
    #'
    #' @return NULL (updates self$parameters)
    extract_parameters = function() {
      if (is.null(self$fitted_model)) {
        return(NULL)
      }

      tryCatch({
        params <- coef(self$fitted_model)

        # drc package naming: b:(Intercept), c:(Intercept), d:(Intercept), e:(Intercept)
        # For LL.4(): b=slope, c=lower, d=upper, e=ED50
        if ("b:(Intercept)" %in% names(params)) {
          self$parameters$b <- unname(params["b:(Intercept)"])  # slope
          self$parameters$c <- unname(params["c:(Intercept)"])  # lower asymptote
          self$parameters$d <- unname(params["d:(Intercept)"])  # upper asymptote
          self$parameters$e <- unname(params["e:(Intercept)"])  # ED50
        } else {
          # Generic fallback - assume order [b, c, d, e]
          self$parameters$b <- unname(params[1])
          self$parameters$c <- unname(params[2])
          self$parameters$d <- unname(params[3])
          self$parameters$e <- unname(params[4])
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

      # For LL.4/L.4: f(x) = c + (d-c)/(1+(x/e)^b)
      # Inverse: x = e * ((d-c)/(y-c) - 1)^(1/b)
      tryCatch({
        b <- self$parameters$b
        c <- self$parameters$c  # lower asymptote
        d <- self$parameters$d  # upper asymptote
        e <- self$parameters$e  # ED50

        # Check for valid parameters
        if (any(is.na(c(b, c, d, e)))) {
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

        # Calculate the ratio term: (d-c)/(y-c) - 1
        ratio <- (d - c) / (response - c) - 1

        # Check for valid ratio
        if (ratio <= 0) {
          warning("Cannot calculate concentration: invalid ratio term")
          return(NA)
        }

        # Calculate concentration: x = e * ratio^(1/b)
        concentration <- e * (ratio)^(1/b)

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
        return("4PL model not fitted")
      }

      b <- self$parameters$b
      c <- self$parameters$c
      d <- self$parameters$d
      e <- self$parameters$e

      if (any(is.na(c(b, c, d, e)))) {
        return("4PL model parameters not available")
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
        ")/(1 + (x/", round(e, 2), ")^", round(b, 3), ")",
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
    }
  )
)
