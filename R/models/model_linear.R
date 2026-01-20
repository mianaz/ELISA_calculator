# ELISA Linear Model
# R6 class for fitting linear dose-response curves

# Source dependencies
source("R/models/model_base.R")
source("R/utils/helpers.R")
source("R/utils/constants.R")

#' Linear Model for ELISA
#'
#' @description
#' Linear model: y = b0 + b1 * x
#' where:
#'   b0 = intercept
#'   b1 = slope
#'
#' For log-transformed data: y = b0 + b1 * log10(x)
#'
#' @export
ElisaLinearModel <- R6Class("ElisaLinearModel",
  inherit = ElisaModel,

  public = list(
    #' @field coefficients Named list of linear model coefficients
    coefficients = NULL,

    #' Initialize linear model
    #'
    #' @param log_transform Whether to use log transformation
    initialize = function(log_transform = TRUE) {
      super$initialize(model_type = MODEL_LINEAR, log_transform = log_transform)
      self$coefficients <- list(intercept = NA, slope = NA)
    },

    #' Fit linear model to data
    #'
    #' @param data Data frame with Concentration and Response columns
    #' @return Self (for method chaining)
    fit = function(data) {
      # Store model data
      self$model_data <- data

      # Prepare data - remove rows with NA values
      if (self$log_transform) {
        clean_data <- data %>%
          filter(
            !is.na(!!sym(COL_LOG_CONC)),
            !is.na(!!sym(COL_RESPONSE)),
            is.finite(!!sym(COL_LOG_CONC)),
            is.finite(!!sym(COL_RESPONSE))
          )

        if (nrow(clean_data) < 3) {
          self$fit_error <- "Insufficient data points for linear fitting (need at least 3)"
          return(self)
        }

        # Fit linear model using log-transformed concentrations
        tryCatch({
          formula <- as.formula(paste(COL_RESPONSE, "~", COL_LOG_CONC))
          self$fitted_model <- lm(formula, data = clean_data)

          # Extract coefficients
          self$extract_coefficients()
          self$is_fitted <- TRUE

        }, error = function(e) {
          self$fit_error <- e$message
          self$is_fitted <- FALSE
        })

      } else {
        clean_data <- data %>%
          filter(
            !is.na(!!sym(COL_CONCENTRATION)),
            !is.na(!!sym(COL_RESPONSE)),
            is.finite(!!sym(COL_CONCENTRATION)),
            is.finite(!!sym(COL_RESPONSE))
          )

        if (nrow(clean_data) < 3) {
          self$fit_error <- "Insufficient data points for linear fitting (need at least 3)"
          return(self)
        }

        # Fit linear model using regular concentrations
        tryCatch({
          formula <- as.formula(paste(COL_RESPONSE, "~", COL_CONCENTRATION))
          self$fitted_model <- lm(formula, data = clean_data)

          # Extract coefficients
          self$extract_coefficients()
          self$is_fitted <- TRUE

        }, error = function(e) {
          self$fit_error <- e$message
          self$is_fitted <- FALSE
        })
      }

      return(self)
    },

    #' Extract coefficients from fitted model
    #'
    #' @return NULL (updates self$coefficients)
    extract_coefficients = function() {
      if (is.null(self$fitted_model)) {
        return(NULL)
      }

      tryCatch({
        coefs <- coef(self$fitted_model)
        self$coefficients$intercept <- unname(coefs[1])
        self$coefficients$slope <- unname(coefs[2])

      }, error = function(e) {
        self$fit_warnings <- c(self$fit_warnings, paste("Error extracting coefficients:", e$message))
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
        if (self$log_transform) {
          log_conc <- safe_log10(concentrations)
          newdata <- data.frame(log_Concentration = log_conc)
          names(newdata) <- COL_LOG_CONC
          predict(self$fitted_model, newdata = newdata)
        } else {
          newdata <- data.frame(Concentration = concentrations)
          names(newdata) <- COL_CONCENTRATION
          predict(self$fitted_model, newdata = newdata)
        }
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

      # Linear inverse: x = (y - b0) / b1
      tryCatch({
        b0 <- self$coefficients$intercept
        b1 <- self$coefficients$slope

        # Check for valid coefficients
        if (any(is.na(c(b0, b1)))) {
          return(NA)
        }

        # Check for zero slope
        if (abs(b1) < .Machine$double.eps) {
          warning("Slope is too close to zero, cannot invert")
          return(NA)
        }

        # Calculate concentration
        if (self$log_transform) {
          # For log-transformed model: log10(x) = (y - b0) / b1
          log_conc <- (response - b0) / b1
          concentration <- 10^log_conc
        } else {
          # For regular model: x = (y - b0) / b1
          concentration <- (response - b0) / b1
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
        return("Linear model not fitted")
      }

      b0 <- self$coefficients$intercept
      b1 <- self$coefficients$slope

      if (any(is.na(c(b0, b1)))) {
        return("Linear model coefficients not available")
      }

      # Get quality metrics
      metrics <- self$get_quality_metrics()
      r_squared_text <- if (!is.null(metrics)) {
        paste0(", R² = ", round(metrics$r_squared, 4))
      } else {
        ""
      }

      if (self$log_transform) {
        paste0(
          "y = ", round(b0, 3), " + ",
          round(b1, 3), " * log10(x)",
          r_squared_text
        )
      } else {
        paste0(
          "y = ", round(b0, 3), " + ",
          round(b1, 3), " * x",
          r_squared_text
        )
      }
    },

    #' Get model parameters (coefficients)
    #'
    #' @return Named list of coefficients
    get_parameters = function() {
      if (!self$is_fitted) {
        warning("Model has not been fitted yet")
        return(NULL)
      }
      return(self$coefficients)
    }
  )
)
