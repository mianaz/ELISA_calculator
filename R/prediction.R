# ELISA Prediction Functions
# Functions for predicting sample concentrations from standard curves

#' Create prediction function that handles fallbacks properly
#'
#' @param sample_row Sample data row with Response value
#' @param models List of fitted models
#' @param standards Standards data
#' @param log_transform Whether to use log transformation
#' @param offset Offset added to concentration values
#' @return List with prediction results
predict_sample_concentration <- function(sample_row, models, standards, log_transform = TRUE, offset = 1e-5) {
  response_val <- sample_row$Response
  if(is.na(response_val)) return(list(Concentration = NA, Model_Used = NA, Error = "NA Response"))

  result <- list(
    Concentration = NA,
    Model_Used = NA,
    Error = NA,
    Out_Of_Range = FALSE
  )

  # Get standard curve range from the standards parameter
  std_range <- range(standards$Concentration, na.rm = TRUE)
  
  # Try 4PL model if available
  if(!is.null(models$fourpl)) {
    tryCatch({
      # Get model parameters
      params <- coef(models$fourpl)
      
      # Identify parameter naming convention
      if("d:(Intercept)" %in% names(params)) {
        a <- params["d:(Intercept)"]  # upper asymptote
        d <- params["c:(Intercept)"]  # lower asymptote
        c <- params["e:(Intercept)"]  # ED50
        b <- params["b:(Intercept)"]  # slope
      } else if("Upper" %in% names(params)) {
        a <- params["Upper"]
        d <- params["Lower"]
        c <- params["ED50"]
        b <- params["Slope"]
      } else {
        stop("Cannot identify parameter names in 4PL model")
      }
      
      # For 4PL model: y = d + (a - d) / (1 + (x / e)^b)
      # Solve for x: x = e * ((a - d) / (y - d) - 1)^(1/b)
      # Where: a = upper asymptote, d = lower asymptote, e = ED50, b = slope
      
      # Calculate concentration directly from 4PL parameters
      if(log_transform) {
        # For log-transformed model, solve for log-concentration
        # then back-transform to concentration
        numerator <- (a - d) / (response_val - d) - 1
        # Handle negative values or zeros that would cause errors when taking roots
        if(numerator <= 0 && b < 0) {
          # If numerator is negative and b is negative, we can't take the root
          stop("Cannot calculate concentration: negative base with fractional exponent")
        }
        
        # For competitive ELISA with log-transform, need to handle carefully
        log_conc <- c + (1/b) * log10(max(numerator, .Machine$double.eps))
        result$Concentration <- 10^log_conc
      } else {
        # For non-log transform, calculate directly
        numerator <- (a - d) / (response_val - d) - 1
        # Handle negative values or zeros that would cause errors when taking roots
        if(numerator <= 0 && b < 0) {
          stop("Cannot calculate concentration: negative base with fractional exponent")
        }
        
        result$Concentration <- c * (numerator)^(1/b)
      }
      
      result$Model_Used <- "4PL"
      
      # Check if within range
      if(result$Concentration < std_range[1] || result$Concentration > std_range[2]) {
        result$Out_Of_Range <- TRUE
        result$Error <- "Value outside standard curve range"
      }
    }, error = function(e) {
      # If 4PL fails, set concentration to NA and store error
      result$Concentration <- NA
      result$Error <- paste("4PL error:", e$message)
      result$Model_Used <- NA
    })
  }
  
  # If 4PL failed or out of range, try linear model as fallback
  if((is.na(result$Concentration) || result$Out_Of_Range) && !is.null(models$linear)) {
    tryCatch({
      linear_coefs <- coef(models$linear)
      
      if(log_transform) {
        # For log-transformed model: y = b0 + b1 * log10(x)
        # Solve for x: log10(x) = (y - b0) / b1
        # Therefore: x = 10^((y - b0) / b1)
        log_conc <- (response_val - linear_coefs[1]) / linear_coefs[2]
        linear_result <- 10^log_conc
      } else {
        # For non-log model: y = b0 + b1 * x
        # Solve for x: x = (y - b0) / b1
        linear_result <- (response_val - linear_coefs[1]) / linear_coefs[2]
      }
      
      # Only replace 4PL result if it was missing or out of range
      if(is.na(result$Concentration) || result$Out_Of_Range) {
        result$Concentration <- linear_result
        result$Model_Used <- "Linear"
        
        # Check if this result is within range
        if(result$Concentration < std_range[1] || result$Concentration > std_range[2]) {
          result$Out_Of_Range <- TRUE
        } else {
          result$Out_Of_Range <- FALSE
        }
      }
    }, error = function(e) {
      if(is.na(result$Concentration)) {
        result$Error <- paste("Linear error:", e$message)
      }
      # Keep existing 4PL result if there was one
    })
  }
  
  # If both models failed, ensure we have a proper error message
  if(is.na(result$Concentration) && is.na(result$Error)) {
    result$Error <- "No suitable model could calculate concentration"
  }
  
  return(result)
}

#' Predict sample concentrations using optimal model choice
#'
#' @param samples Samples data frame
#' @param models List of fitted models
#' @param standards Standards data frame
#' @param std_summary Summarized standards data
#' @param log_transform Whether to log transform concentrations
#' @param offset Small positive value added to concentrations
#' @param std_range Range of standard concentrations
#' @return Samples data frame with predictions
predict_sample_concentrations <- function(samples, models, standards, std_summary, 
                                         log_transform = TRUE, offset = 1e-5,
                                         std_range = NULL) {
  # Define standard curve range if not provided
  if (is.null(std_range)) {
    std_range <- range(standards$Concentration, na.rm = TRUE)
  }
  
  # Track samples that couldn't be calculated
  uncalculatable_samples <- character(0)
  
  # Get all unique sample types
  sample_types <- unique(samples$Type)
  
  # Process each sample type to ensure all replicates use same model
  for(sample_type in sample_types) {
    # Get indices of all samples of this type
    type_indices <- which(samples$Type == sample_type)
    
    # Calculate average response for this type
    type_response <- mean(samples$Response[type_indices], na.rm = TRUE)
    
    # Skip if no valid response (all NA)
    if(is.na(type_response)) {
      # Record that this sample couldn't be calculated
      uncalculatable_samples <- c(uncalculatable_samples, sample_type)
      
      # Set the values to NA, will be handled later for capped values
      samples$Predicted_Concentration[type_indices] <- NA
      samples$Model_Used[type_indices] <- "Uncalculatable"
      samples$Error_Flag[type_indices] <- TRUE
      samples$Out_of_Range[type_indices] <- TRUE  # Mark as out of range so they'll be capped
      next
    }
    
    # Predict using representative value to decide which model to use
    sample_row <- data.frame(Response = type_response)
    type_prediction <- predict_sample_concentration(
      sample_row, 
      models, 
      standards, 
      log_transform, 
      offset
    )
    
    # Always try 4PL first, fallback to Linear if 4PL fails
    chosen_model <- "4PL"
    
    # Calculate individual predictions using the same model for all samples of this type
    for(i in type_indices) {
      response_val <- samples$Response[i]
      if(is.na(response_val)) {
        # Record uncalculatable sample
        if(!(sample_type %in% uncalculatable_samples)) {
          uncalculatable_samples <- c(uncalculatable_samples, sample_type)
        }
        
        samples$Predicted_Concentration[i] <- NA
        samples$Model_Used[i] <- "Uncalculatable"
        samples$Error_Flag[i] <- TRUE
        samples$Out_of_Range[i] <- TRUE  # Mark as out of range so they'll be capped
        next
      }
      
      # Use chosen model for prediction
      individual_result <- list(
        Concentration = NA,
        Model_Used = NA,
        Error = NA,
        Out_Of_Range = FALSE
      )
      
      if(chosen_model == "4PL" && !is.null(models$fourpl)) {
        tryCatch({
          # Get model parameters for 4PL
          params <- coef(models$fourpl)
          
          # Identify parameter naming convention
          if("d:(Intercept)" %in% names(params)) {
            a <- params["d:(Intercept)"]  # upper asymptote
            d <- params["c:(Intercept)"]  # lower asymptote
            c <- params["e:(Intercept)"]  # ED50
            b <- params["b:(Intercept)"]  # slope
          } else if("Upper" %in% names(params)) {
            a <- params["Upper"]
            d <- params["Lower"]
            c <- params["ED50"]
            b <- params["Slope"]
          } else {
            stop("Cannot identify parameter names in 4PL model")
          }
          
          # Calculate concentration directly from 4PL parameters
          if(log_transform) {
            # For log-transformed model, solve for log-concentration
            # then back-transform to concentration
            numerator <- (a - d) / (response_val - d) - 1
            # Handle negative values or zeros that would cause errors when taking roots
            if(numerator <= 0 && b < 0) {
              # If numerator is negative and b is negative, we can't take the root
              stop("Cannot calculate concentration: negative base with fractional exponent")
            }
            
            # For competitive ELISA with log-transform, need to handle carefully
            log_conc <- c + (1/b) * log10(max(numerator, .Machine$double.eps))
            individual_result$Concentration <- 10^log_conc
          } else {
            # For non-log transform, calculate directly
            numerator <- (a - d) / (response_val - d) - 1
            # Handle negative values or zeros that would cause errors when taking roots
            if(numerator <= 0 && b < 0) {
              stop("Cannot calculate concentration: negative base with fractional exponent")
            }
            
            individual_result$Concentration <- c * (numerator)^(1/b)
          }
          
          # Simply use "4PL" without appending model source to ensure consistency
          individual_result$Model_Used <- "4PL"

          # Check if result is within standard curve range
          if(individual_result$Concentration < std_range[1] || individual_result$Concentration > std_range[2]) {
            individual_result$Out_Of_Range <- TRUE
            individual_result$Error <- "Value outside standard curve range"
            # Keep the calculated value but mark it as out of range -
            # We'll only filter it out in the 4PL-specific plots
          }
        }, error = function(e) {
          # Store original error
          original_error <- paste("4PL error:", e$message)
          
          # Record that this sample couldn't be calculated with 4PL
          if(!(sample_type %in% uncalculatable_samples)) {
            uncalculatable_samples <- c(uncalculatable_samples, sample_type)
          }

          # Try linear model as fallback if 4PL fails
          if(!is.null(models$linear)) {
            tryCatch({
              linear_coefs <- coef(models$linear)
              if(log_transform) {
                log_conc <- (response_val - linear_coefs[1]) / linear_coefs[2]
                individual_result$Concentration <- 10^log_conc
              } else {
                individual_result$Concentration <- (response_val - linear_coefs[1]) / linear_coefs[2]
              }
              individual_result$Model_Used <- "Linear (4PL failed)"
              individual_result$Error <- paste(original_error, "- Using linear model as fallback")

              # Check if linear result is within range
              if(individual_result$Concentration < std_range[1] || individual_result$Concentration > std_range[2]) {
                individual_result$Out_Of_Range <- TRUE
                individual_result$Error <- paste(individual_result$Error, "- Value outside standard curve range")
              }
            }, error = function(e2) {
              # If both models fail, keep the original error
              individual_result$Error <- original_error
              individual_result$Concentration <- NA
              individual_result$Model_Used <- "Uncalculatable"
              individual_result$Out_Of_Range <- TRUE  # Mark as out of range to trigger capping
            })
          } else {
            individual_result$Error <- original_error
            individual_result$Concentration <- NA
            individual_result$Model_Used <- "Uncalculatable"
            individual_result$Out_Of_Range <- TRUE  # Mark as out of range to trigger capping
          }
        })
      } else if(!is.null(models$linear)) {
        tryCatch({
          # Extract coefficients from the linear model
          linear_coefs <- coef(models$linear)
          
          # For linear model: y = b0 + b1 * log10(x) [if log_transform=TRUE]
          # Or y = b0 + b1 * x [if log_transform=FALSE]
          # Solving for concentration:
          if(log_transform) {
            # First calculate log10(concentration)
            log_conc <- (response_val - linear_coefs[1]) / linear_coefs[2]
            # Convert from log scale to actual concentration
            individual_result$Concentration <- 10^log_conc
          } else {
            # Direct linear model calculation for non-log transform
            individual_result$Concentration <- (response_val - linear_coefs[1]) / linear_coefs[2]
          }
          
          individual_result$Model_Used <- "Linear"

          # Check if result is within standard curve range
          if(individual_result$Concentration < std_range[1] || individual_result$Concentration > std_range[2]) {
            individual_result$Out_Of_Range <- TRUE
            individual_result$Error <- "Value outside standard curve range"
          }
        }, error = function(e) {
          individual_result$Error <- paste("Linear error:", e$message)
          individual_result$Concentration <- NA
          individual_result$Model_Used <- "Uncalculatable"
          individual_result$Out_Of_Range <- TRUE  # Mark as out of range to trigger capping
          
          # Record that this sample couldn't be calculated
          if(!(sample_type %in% uncalculatable_samples)) {
            uncalculatable_samples <- c(uncalculatable_samples, sample_type)
          }
        })
      }
      
      # Store results
      samples$Predicted_Concentration[i] <- individual_result$Concentration
      samples$Model_Used[i] <- individual_result$Model_Used
      samples$Error_Flag[i] <- !is.null(individual_result$Error) && !is.na(individual_result$Error)
      samples$Out_of_Range[i] <- isTRUE(individual_result$Out_Of_Range)
    }
  }
  
  # Add a warning for uncalculatable samples if any were found
  if(length(uncalculatable_samples) > 0) {
    message("Warning: The following samples could not be calculated and will use capped values: ", 
            paste(uncalculatable_samples, collapse=", "))
  }
  
  # For capped concentrations, use appropriate capping strategy
  # Find the unique standard concentrations for nearest-value capping
  std_concentrations <- sort(unique(standards$Concentration))
  
  # First, create a function to handle the NA cases outside of case_when
  get_nearest_std <- function(response_val, std_summary, std_concentrations) {
    if(is.na(response_val)) {
      # If response is also NA, use middle of standard range
      return(median(std_concentrations, na.rm = TRUE))
    } else {
      # Find the nearest standard based on response
      # For competitive ELISA, find standard with closest response value
      std_diffs <- abs(std_summary$Mean_Response - response_val)
      closest_std_idx <- which.min(std_diffs)
      return(std_summary$Concentration[closest_std_idx])
    }
  }

  # Create a capped concentration column
  samples$Capped_Concentration <- samples$Predicted_Concentration

  # Handle NA predictions
  na_idx <- which(is.na(samples$Predicted_Concentration))
  if(length(na_idx) > 0) {
    for(i in na_idx) {
      samples$Capped_Concentration[i] <- get_nearest_std(
        samples$Response[i],
        std_summary,
        std_concentrations
      )
    }
  }

  # Handle values below range
  below_idx <- which(!is.na(samples$Predicted_Concentration) & samples$Predicted_Concentration < std_range[1])
  if(length(below_idx) > 0) {
    samples$Capped_Concentration[below_idx] <- std_concentrations[1]
  }

  # Handle values above range
  above_idx <- which(!is.na(samples$Predicted_Concentration) & samples$Predicted_Concentration > std_range[2])
  if(length(above_idx) > 0) {
    samples$Capped_Concentration[above_idx] <- std_concentrations[length(std_concentrations)]
  }

  # Add flags
  samples <- samples %>%
    mutate(
      # Flag for values that were capped or extrapolated
      Is_Capped = Capped_Concentration != Predicted_Concentration | is.na(Predicted_Concentration),
      # Flag for unreliable results (capped, extrapolated or uncalculatable)
      Is_Unreliable = Is_Capped | Out_of_Range
    )
    
  return(samples)
}

#' Summarize sample data for plotting and tables
#'
#' @param samples Samples data frame with predictions
#' @param std_range Range of standard concentrations
#' @return List with actual, capped, and combined summaries
summarize_samples <- function(samples, std_range) {
  # Summarize samples by type (average replicates)
  # Create two versions of samples summary: one with all data and one with capped data
  # First, the actual data with no capping
  samples_summary_actual <- samples %>%
    filter(!Error_Flag) %>%  # Filter out error values for mean calculation
    group_by(Type) %>%
    summarize(
      Mean_OD = mean(OD, na.rm = TRUE),
      Mean_OD_cor = mean(OD_cor, na.rm = TRUE),
      Mean_Response = mean(Response, na.rm = TRUE),
      SD_Response = sd(Response, na.rm = TRUE),
      Mean_Concentration = mean(Predicted_Concentration, na.rm = TRUE),
      SD_Concentration = sd(Predicted_Concentration, na.rm = TRUE),
      SEM_Concentration = sd(Predicted_Concentration, na.rm = TRUE) / sqrt(sum(!is.na(Predicted_Concentration))),
      n = n(),
      # Add information about which model was used
      Models_Used = paste(unique(Model_Used), collapse=", "),
      Out_Of_Range_Count = sum(Out_of_Range, na.rm = TRUE),
      Has_Out_of_Range = any(Out_of_Range, na.rm = TRUE),
      Has_Unreliable = any(Is_Unreliable, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Summarize the capped version
  samples_summary_capped <- samples %>%
    filter(!Error_Flag) %>%
    group_by(Type) %>%
    summarize(
      Mean_OD = mean(OD, na.rm = TRUE),
      Mean_OD_cor = mean(OD_cor, na.rm = TRUE),
      Mean_Response = mean(Response, na.rm = TRUE),
      SD_Response = sd(Response, na.rm = TRUE),
      Mean_Concentration = mean(Capped_Concentration, na.rm = TRUE),
      SD_Concentration = sd(Capped_Concentration, na.rm = TRUE),
      SEM_Concentration = sd(Capped_Concentration, na.rm = TRUE) / sqrt(sum(!is.na(Capped_Concentration))),
      n = n(),
      # Flag to indicate this has capped values
      Has_Capped_Values = any(Capped_Concentration != Predicted_Concentration, na.rm = TRUE),
      # Add information about which model was used
      Models_Used = paste(unique(Model_Used), collapse=", "),
      Out_Of_Range_Count = sum(Out_of_Range, na.rm = TRUE),
      Has_Out_of_Range = any(Out_of_Range, na.rm = TRUE),
      Has_Unreliable = any(Is_Unreliable, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Combine data from both sample summaries
  sample_summary <- samples_summary_actual %>%
    left_join(
      samples_summary_capped %>%
        dplyr::select(Type, Mean_Concentration, SD_Concentration, Has_Capped_Values) %>%
        rename(
          Capped_Mean_Concentration = Mean_Concentration,
          Capped_SD_Concentration = SD_Concentration
        ),
      by = "Type"
    )
  
  return(list(
    actual = samples_summary_actual,
    capped = samples_summary_capped,
    combined = sample_summary
  ))
}