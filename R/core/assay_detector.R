# ELISA Assay Type Detection Module
# Functions for automatically detecting ELISA assay type based on data patterns

#' Detect ELISA assay type based on standard curve relationship
#'
#' @param standards Standards data frame with Concentration and Response columns
#' @param config Configuration list
#' @return List with detected assay type, correlation, and confidence
detect_assay_type <- function(standards, config = NULL) {
  if (is.null(config)) {
    config <- get_default_config()
  }

  # Initialize result
  detection_result <- list(
    assay_type = ASSAY_TYPE_AUTO,
    correlation = NA,
    confidence = "unknown",
    message = "Unable to detect assay type"
  )

  # Check if we have enough data
  if (nrow(standards) < 3) {
    detection_result$message <- "Insufficient standards for assay type detection (need at least 3)"
    return(detection_result)
  }

  # Remove any rows with NA in critical columns
  clean_standards <- standards %>%
    filter(
      !is.na(!!sym(COL_CONCENTRATION)),
      !is.na(!!sym(COL_RESPONSE)),
      is.finite(!!sym(COL_CONCENTRATION)),
      is.finite(!!sym(COL_RESPONSE))
    )

  if (nrow(clean_standards) < 3) {
    detection_result$message <- "Insufficient valid standards after removing NAs"
    return(detection_result)
  }

  # Calculate Pearson correlation between concentration and response
  tryCatch({
    correlation <- cor(
      clean_standards[[COL_CONCENTRATION]],
      clean_standards[[COL_RESPONSE]],
      use = "complete.obs"
    )

    detection_result$correlation <- correlation

    # Determine assay type based on correlation
    # Competitive ELISA: negative correlation (higher conc → lower signal)
    # Direct/Sandwich ELISA: positive correlation (higher conc → higher signal)

    if (is.na(correlation)) {
      detection_result$message <- "Could not calculate correlation"
      detection_result$confidence <- "none"
    } else if (correlation < CORRELATION_THRESHOLD_COMPETITIVE) {
      # Strong negative correlation suggests competitive assay
      detection_result$assay_type <- ASSAY_TYPE_COMPETITIVE
      detection_result$confidence <- if (correlation < -0.8) "high" else if (correlation < -0.6) "medium" else "low"
      detection_result$message <- paste0(
        "Detected competitive ELISA (negative correlation: ",
        round(correlation, 3), ")"
      )
    } else if (correlation > CORRELATION_THRESHOLD_DIRECT) {
      # Strong positive correlation suggests direct/sandwich assay
      detection_result$assay_type <- ASSAY_TYPE_DIRECT
      detection_result$confidence <- if (correlation > 0.8) "high" else if (correlation > 0.6) "medium" else "low"
      detection_result$message <- paste0(
        "Detected direct/sandwich ELISA (positive correlation: ",
        round(correlation, 3), ")"
      )
    } else {
      # Weak correlation - unclear assay type
      detection_result$assay_type <- ASSAY_TYPE_AUTO
      detection_result$confidence <- "low"
      detection_result$message <- paste0(
        "Unclear assay type (weak correlation: ",
        round(correlation, 3), "). Assuming competitive."
      )
      # Default to competitive for backward compatibility
      detection_result$assay_type <- ASSAY_TYPE_COMPETITIVE
    }

  }, error = function(e) {
    detection_result$message <- paste("Error detecting assay type:", e$message)
  })

  return(detection_result)
}

#' Validate user-selected assay type against detected type
#'
#' @param user_assay_type User-selected assay type
#' @param detected_result Detection result from detect_assay_type()
#' @return List with validation result and recommendations
validate_assay_type_selection <- function(user_assay_type, detected_result) {
  validation <- list(
    valid = TRUE,
    warnings = character(0),
    recommendations = character(0)
  )

  # If user selected "auto", use detection
  if (user_assay_type == ASSAY_TYPE_AUTO) {
    validation$recommendations <- c(
      validation$recommendations,
      paste("Using auto-detected assay type:", detected_result$assay_type)
    )
    return(validation)
  }

  # If detection was successful, check for mismatch
  if (!is.na(detected_result$correlation) &&
      detected_result$confidence %in% c("high", "medium")) {

    if (user_assay_type != detected_result$assay_type) {
      validation$warnings <- c(
        validation$warnings,
        paste0(
          "User-selected assay type (", user_assay_type,
          ") differs from detected type (", detected_result$assay_type,
          "). Correlation: ", round(detected_result$correlation, 3)
        )
      )
      validation$recommendations <- c(
        validation$recommendations,
        paste("Consider using", detected_result$assay_type, "based on data pattern")
      )
    }
  }

  return(validation)
}

#' Determine final assay type to use for analysis
#'
#' @param user_assay_type User-selected assay type (can be "auto")
#' @param standards Standards data frame
#' @param config Configuration list
#' @return List with final assay type and detection details
determine_assay_type <- function(user_assay_type = ASSAY_TYPE_AUTO, standards, config = NULL) {
  # Detect assay type from data
  detection <- detect_assay_type(standards, config)

  # If user selected auto, use detected type
  if (user_assay_type == ASSAY_TYPE_AUTO) {
    final_type <- detection$assay_type
  } else {
    # Use user-selected type
    final_type <- user_assay_type

    # Validate user selection
    validation <- validate_assay_type_selection(user_assay_type, detection)

    # Print warnings if any
    if (length(validation$warnings) > 0) {
      for (warning in validation$warnings) {
        message("Warning: ", warning)
      }
    }
  }

  return(list(
    assay_type = final_type,
    detection_result = detection,
    user_selected = user_assay_type != ASSAY_TYPE_AUTO
  ))
}

#' Get assay type description for UI display
#'
#' @param assay_type Assay type constant
#' @return Description string
get_assay_type_description <- function(assay_type) {
  descriptions <- list()
  descriptions[[ASSAY_TYPE_COMPETITIVE]] <- "Competitive ELISA (inverse relationship: higher concentration = lower signal)"
  descriptions[[ASSAY_TYPE_DIRECT]] <- "Direct ELISA (direct relationship: higher concentration = higher signal)"
  descriptions[[ASSAY_TYPE_INDIRECT]] <- "Indirect ELISA (direct relationship: higher concentration = higher signal)"
  descriptions[[ASSAY_TYPE_SANDWICH]] <- "Sandwich ELISA (direct relationship: higher concentration = higher signal)"
  descriptions[[ASSAY_TYPE_AUTO]] <- "Auto-detect assay type based on data pattern"

  descriptions[[assay_type]] %||% "Unknown assay type"
}

#' Adjust model fitting direction based on assay type
#'
#' @param assay_type Assay type constant
#' @return List with fitting parameters
get_fitting_parameters <- function(assay_type) {
  params <- list(
    reverse_response = FALSE,  # Whether to reverse response for fitting
    description = ""
  )

  if (assay_type == ASSAY_TYPE_COMPETITIVE) {
    # Competitive assays have inverse relationship (current behavior)
    params$reverse_response <- FALSE
    params$description <- "Using inverse relationship for competitive ELISA"
  } else if (assay_type %in% c(ASSAY_TYPE_DIRECT, ASSAY_TYPE_INDIRECT, ASSAY_TYPE_SANDWICH)) {
    # Direct/indirect/sandwich assays have positive relationship
    params$reverse_response <- FALSE
    params$description <- paste0("Using direct relationship for ", assay_type, " ELISA")
  }

  return(params)
}

#' Create visualization data showing assay type detection
#'
#' @param standards Standards data frame
#' @param detection_result Detection result
#' @return Data frame for plotting detection visualization
create_detection_visualization_data <- function(standards, detection_result) {
  viz_data <- standards %>%
    select(!!sym(COL_CONCENTRATION), !!sym(COL_RESPONSE)) %>%
    mutate(
      Assay_Type = detection_result$assay_type,
      Correlation = detection_result$correlation,
      Confidence = detection_result$confidence
    )

  return(viz_data)
}
