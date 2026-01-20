# ELISA Analyzer Functions
# This file contains the main ELISA analysis function and helper functions

#' Analyze Competitive ELISA Data with Enhanced Features
#'
#' @param file_path Path to Excel file with ELISA data
#' @param sheet_name Name or index of the sheet to read (default: 1)
#' @param well_col Column name for well IDs (default: "Well ID")
#' @param type_col Column name for sample types (default: "Sample Type")
#' @param conc_col Column name for concentration (default: "Concentration")
#' @param od_col Column name for raw OD values (default: "OD")
#' @param od_corr_col Column name for corrected OD values (default: "OD (corrected)")
#' @param use_corrected Logical; whether to use corrected OD values (default: TRUE)
#' @param nsb_label Label for non-specific binding wells (default: "NSB")
#' @param b0_label Label for maximum binding wells (default: "B0")
#' @param std_label Label for standard wells (default: "Standard")
#' @param skip_normalization Logical; skip B0/NSB normalization and use raw or corrected OD (default: FALSE)
#' @param log_transform Logical; whether to log-transform concentrations (default: TRUE)
#' @param offset Small value to add to concentrations before log transform (default: 1e-5)
#' @param removeUnlabeled Logical; remove unlabeled samples (default: TRUE)
#' @return List containing model, plot, and predicted concentrations

analyze_melatonin_elisa <- function(
    file_path,
    sheet_name = 1,
    well_col = "Well ID",
    type_col = "Sample Type",
    conc_col = "Concentration",
    od_col = "OD",
    od_corr_col = "OD (corrected)",
    use_corrected = TRUE,
    nsb_label = "NSB",
    b0_label = "B0",
    blank_label="Blank",
    std_label = "Standard",
    skip_normalization = FALSE,
    log_transform = TRUE,
    offset = 1e-5,
    removeUnlabeled = TRUE
) {
  
  # Step 1: Read data from Excel file
  raw_data <- read_excel(file_path, sheet = sheet_name)
  
  # Attempt to identify columns if they don't exist
  all_cols <- colnames(raw_data)

  # Try to find well column
  if(!(well_col %in% all_cols)) {
    possible_well_cols <- grep("well|id|position", all_cols, ignore.case = TRUE, value = TRUE)
    if(length(possible_well_cols) > 0) {
      well_col <- possible_well_cols[1]
    } else {
      well_col <- all_cols[1]  # Default to first column
    }
  }

  # Try to find type column
  if(!(type_col %in% all_cols)) {
    possible_type_cols <- grep("type|sample|category|group", all_cols, ignore.case = TRUE, value = TRUE)
    if(length(possible_type_cols) > 0) {
      type_col <- possible_type_cols[1]
    } else {
      type_col <- all_cols[2]  # Default to second column
    }
  }

  # Try to find OD column
  if(!(od_col %in% all_cols)) {
    possible_od_cols <- grep("od|absorbance|optical|density", all_cols, ignore.case = TRUE, value = TRUE)
    if(length(possible_od_cols) > 0) {
      od_col <- possible_od_cols[1]
    } else {
      od_col <- all_cols[min(4, length(all_cols))]  # Default to fourth column or last if fewer
    }
  }

  # Try to find concentration column if needed
  if(!skip_normalization && !(conc_col %in% all_cols)) {
    possible_conc_cols <- grep("conc|concentration|std|standard", all_cols, ignore.case = TRUE, value = TRUE)
    if(length(possible_conc_cols) > 0) {
      conc_col <- possible_conc_cols[1]
    } else {
      conc_col <- all_cols[min(3, length(all_cols))]  # Default to third column or last if fewer
    }
  }

  # Check for corrected OD if needed
  if(use_corrected && !(od_corr_col %in% all_cols)) {
    possible_corr_cols <- grep("correct|adjusted|normalized", all_cols, ignore.case = TRUE, value = TRUE)
    if(length(possible_corr_cols) > 0) {
      od_corr_col <- possible_corr_cols[1]
    } else {
      # If no corrected column is found, disable use_corrected
      use_corrected <- FALSE
    }
  }
  
  # Log the columns used
  message("Using columns: Well=", well_col, ", Type=", type_col, ", OD=", od_col,
         ", Concentration=", conc_col, ", Corrected OD=", od_corr_col)

  # Normalize column names
  data <- raw_data

  # Use tryCatch to avoid errors with non-existing columns
  tryCatch({
    colnames(data)[colnames(data) == well_col] <- "Well"
  }, error = function(e) {
    message("Warning: Could not rename well column: ", e$message)
  })

  tryCatch({
    colnames(data)[colnames(data) == type_col] <- "Type"
  }, error = function(e) {
    message("Warning: Could not rename type column: ", e$message)
  })

  tryCatch({
    colnames(data)[colnames(data) == conc_col] <- "Concentration"
  }, error = function(e) {
    message("Warning: Could not rename concentration column: ", e$message)
  })

  tryCatch({
    colnames(data)[colnames(data) == od_col] <- "OD"
  }, error = function(e) {
    message("Warning: Could not rename OD column: ", e$message)
  })

  if(use_corrected) {
    tryCatch({
      colnames(data)[colnames(data) == od_corr_col] <- "OD_corrected"
    }, error = function(e) {
      message("Warning: Could not rename corrected OD column: ", e$message)
      use_corrected <- FALSE
    })
  }

  # Check if required columns are present after renaming
  required_cols <- c("Well", "Type", "OD")
  if(!skip_normalization) required_cols <- c(required_cols, "Concentration")

  missing_cols <- setdiff(required_cols, colnames(data))
  if(length(missing_cols) > 0) {
    message("Warning: Still missing required columns: ", paste(missing_cols, collapse=", "))
    # Create missing columns with dummy data
    for(col in missing_cols) {
      message("Creating dummy column: ", col)
      if(col == "Well") {
        data$Well <- paste0("Well", seq_len(nrow(data)))
      } else if(col == "Type") {
        # Guess types based on patterns in other columns
        data$Type <- "Unknown"
        if("Concentration" %in% colnames(data)) {
          # Rows with concentration might be standards
          data$Type[!is.na(data$Concentration) & data$Concentration > 0] <- std_label
          data$Type[!is.na(data$Concentration) & data$Concentration == 0] <- b0_label
        }
      } else if(col == "OD") {
        # Use another numeric column if available
        num_cols <- sapply(data, is.numeric)
        if(any(num_cols)) {
          data$OD <- data[[names(which(num_cols))[1]]]
        } else {
          data$OD <- runif(nrow(data), 0, 1)  # Last resort
        }
      } else if(col == "Concentration") {
        # Standards might have numbers in their labels
        data$Concentration <- 0
        if("Type" %in% colnames(data)) {
          # Extract numbers from type names that might be concentrations
          conc_values <- as.numeric(gsub("[^0-9.]", "", as.character(data$Type)))
          data$Concentration[!is.na(conc_values)] <- conc_values[!is.na(conc_values)]
        }
      }
    }
  }
  
  # Step 2: Process different groups of samples
  # Optionally remove unlabeled samples
  if(removeUnlabeled) {
    # Filter out rows with empty or NA Type
    data <- data %>% 
      dplyr::filter(!is.na(Type) & trimws(as.character(Type)) != "")
  }
  
  # Determine which OD values to use
  od_column <- if(use_corrected && "OD_corrected" %in% colnames(data)) "OD_corrected" else "OD"
  
  # Extract NSB (bottom)
  nsb_data <- data %>% dplyr::filter(Type == nsb_label)
  if(nrow(nsb_data) == 0) {
    nsb_od <- 0
  } else {
    nsb_od <- mean(nsb_data[[od_column]], na.rm = TRUE)
  }
  
  # Extract Blank samples
  blank_data <- data %>% dplyr::filter(Type == blank_label)
  if(nrow(blank_data) == 0) {
    blank_od <- 0
  } else {
    blank_od <- mean(blank_data[[od_column]], na.rm = TRUE)
  }
  
  # Extract B0 (maximum binding)
  b0_data <- data %>% dplyr::filter(Type == b0_label)
  if(nrow(b0_data) == 0) {
    if(!skip_normalization) {
      # Try to find B0 as standard with concentration 0
      b0_data <- data %>% dplyr::filter(Type == std_label & Concentration == 0)
      
      if(nrow(b0_data) == 0 && !skip_normalization) {
        skip_normalization <- TRUE
      }
    }
  }
  
  if(!skip_normalization) {
    b0_od <- mean(b0_data[[od_column]], na.rm = TRUE)
    
    # Check if B0 and NSB are too close
    if(abs(b0_od - nsb_od) < 0.05) {
      skip_normalization <- TRUE
    }
  }
  
  # Extract standards
  standards <- data %>% 
    dplyr::filter(Type == std_label) %>%
    dplyr::mutate(
      Concentration = as.numeric(Concentration),
      OD = as.numeric(OD)
    )
  
  if(nrow(standards) == 0) {
    stop("No standards found with label", std_label)
  }
  
  # Create corrected OD values for all data
  if(nrow(blank_data) > 0) {
    data$OD_cor <- data[[od_column]] - blank_od  # Use blank samples for OD correction
  } else {
    data$OD_cor <- data[[od_column]] - nsb_od  # Fallback to NSB if no blanks
  }
  
  # Re-extract standards with the OD_cor column
  standards <- data %>% 
    filter(Type == std_label) %>%
    mutate(
      Concentration = as.numeric(Concentration),
      OD = as.numeric(OD)
    )
  
  # Calculate normalized values for standards
  if(skip_normalization) {
    standards$Response <- standards$OD_cor
    
    # For competitive ELISA, higher concentration = lower signal
    cor_test <- cor(standards$Concentration, standards$Response, use = "complete.obs")
  } else {
    # Check for standards with OD higher than B0 (common issue in competitive ELISA)
    high_od_standards <- standards[standards[[od_column]] > b0_od, ]
    
    
    # Calculate normalized values for all standards
    b0_norm <- b0_od - nsb_od
    standards$Response <- standards$OD_cor / b0_norm
    
    # Store raw values before any capping for diagnostic purposes
    standards$Raw_Response <- standards$Response
    
    # More flexible criteria for invalid flags - only flag extreme outliers
    standards$Invalid_Flag <- standards$Response < -0.5 | standards$Response > 10 | is.na(standards$Response)
    
    # Check for extreme outliers
    invalid_idx <- which(standards$Invalid_Flag)
  }
  
  # Prepare concentration for modeling
  if(log_transform) {
    # Add small offset to ensure all values are positive
    standards$log_Concentration <- log10(pmax(standards$Concentration + offset, offset))
  }
  
  # Extract samples (anything not NSB, B0, blank or Standard)
  # Initialize with all needed columns to avoid warnings
  samples <- data %>%
    dplyr::filter(!Type %in% c(nsb_label, b0_label, std_label, blank_label)) %>%
    dplyr::mutate(
      Predicted_Concentration = NA_real_,
      Capped_Concentration = NA_real_,
      Model_Used = NA_character_,
      Error_Flag = FALSE,
      Out_of_Range = FALSE,
      Is_Capped = FALSE
    )
  
  # Ensure OD_cor exists in the samples dataframe
  if(!"OD_cor" %in% colnames(samples)) {
    samples$OD_cor <- samples[[od_column]] - nsb_od
  }
  
  if(skip_normalization) {
    samples$Response <- samples$OD_cor
  } else {
    # Calculate normalized values for all samples
    b0_norm <- b0_od - nsb_od
    samples$Response <- samples$OD_cor / b0_norm
    
    # Store raw values before any capping for diagnostic purposes
    samples$Raw_Response <- samples$Response
    
    # Check for invalid normalized values, using more flexible criteria
    samples$Invalid_Flag <- samples$Response < -0.5 | samples$Response > 10 | is.na(samples$Response)
  }
  
  # Prepare summary of standards for later use
  std_summary <- standards %>%
    group_by(Concentration) %>%
    summarize(
      Mean_Response = mean(Response, na.rm = TRUE),
      SD_Response = sd(Response, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    arrange(Concentration)
  
  # Step 3: Fit models to standard curve

  # Initialize model objects
  models <- list()
  model_errors <- list()

  # Define standard range for capping/validation
  std_range <- c(
    min = min(standards$Concentration[standards$Concentration > 0], na.rm = TRUE),
    max = max(standards$Concentration, na.rm = TRUE)
  )

  # Prepare model data
  if(log_transform) {
    model_data <- data.frame(
      Concentration = standards$Concentration,
      logConc = standards$log_Concentration,
      Response = standards$Response
    )

    # Remove any rows with NA in critical columns
    model_data <- model_data[complete.cases(model_data[, c("logConc", "Response")]), ]
  } else {
    model_data <- data.frame(
      Concentration = standards$Concentration,
      Response = standards$Response
    )

    # Remove any rows with NA in critical columns
    model_data <- model_data[complete.cases(model_data[, c("Concentration", "Response")]), ]
  }
  
  # 1. Try fitting 4PL model
  tryCatch({
    if(log_transform) {
      models$fourpl <- drm(
        Response ~ logConc, 
        data = model_data,
        fct = L.4(),
        control = drmc(errorm = FALSE)  # Suppress error messages for cleaner output
      )
    } else {
      models$fourpl <- drm(
        Response ~ Concentration, 
        data = model_data,
        fct = L.4(),
        control = drmc(errorm = FALSE)
      )
    }
    
    # Extract criteria for model comparison
    models$fourpl$logLik <- logLik(models$fourpl)[1]
    models$fourpl$aic <- AIC(models$fourpl)
  }, error = function(e) {
    model_errors$fourpl <- e$message
  })
  
  # 2. Always fit linear model as well
  tryCatch({
    if(log_transform) {
      models$linear <- lm(Response ~ logConc, data = model_data)
    } else {
      models$linear <- lm(Response ~ Concentration, data = model_data)
    }
    
    # Extract criteria for model comparison
    models$linear$logLik <- logLik(models$linear)[1]
    models$linear$aic <- AIC(models$linear)
  }, error = function(e) {
    model_errors$linear <- e$message
  })
  
  # Compare models and select the best one based on AIC
  model_summary <- data.frame(
    Model = character(0),
    R_squared = numeric(0),
    LogLik = numeric(0),
    AIC = numeric(0)
  )
  
  # Add 4PL model stats
  if(!is.null(models$fourpl)) {
    # Calculate R-squared manually for 4PL
    fitted_values <- fitted(models$fourpl)
    observed_values <- models$fourpl$data$Response
    tss <- sum((observed_values - mean(observed_values))^2)
    rss <- sum((observed_values - fitted_values)^2)
    r_squared_4pl <- 1 - (rss/tss)
    
    model_summary <- rbind(model_summary, data.frame(
      Model = "4PL",
      R_squared = r_squared_4pl,
      LogLik = models$fourpl$logLik,
      AIC = models$fourpl$aic
    ))
  }
  
  # Add linear model stats
  if(!is.null(models$linear)) {
    model_summary <- rbind(model_summary, data.frame(
      Model = "Linear",
      R_squared = summary(models$linear)$r.squared,
      LogLik = models$linear$logLik,
      AIC = models$linear$aic
    ))
  }
  
  # Select best model for prediction
  best_model <- NULL
  
  if(nrow(model_summary) > 0) {
    best_model_name <- model_summary$Model[which.min(model_summary$AIC)]
    
    if(best_model_name == "4PL") {
      best_model <- models$fourpl
      best_model$type <- "4PL"
    } else if(best_model_name == "Linear") {
      best_model <- list(
        type = "linear",
        model = models$linear,
        log_transform = log_transform
      )
    }
  } else {
    stop("All model fitting approaches failed.")
  }
  
  # Step 4: Predict sample concentrations from models
  source("R/prediction.R")  # Load prediction functions
  samples <- predict_sample_concentrations(samples, models, standards, std_summary, log_transform, offset, std_range)
  
  # Step 5: Create curve data for plotting
  source("R/plotting.R")  # Load plotting functions
  curve_data_list <- generate_curve_data(models, standards, log_transform, offset)
  
  # Prepare summary data for plots and tables
  # Summarize standards by concentration (average replicates)
  std_summary <- standards %>%
    group_by(Concentration) %>%
    summarize(
      Mean_OD = mean(OD, na.rm = TRUE),
      Mean_OD_cor = mean(OD_cor, na.rm = TRUE),
      Mean_Response = mean(Response, na.rm = TRUE),
      SD_Response = sd(Response, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # Summarize samples by type - both actual and capped versions
  samples_summary <- summarize_samples(samples, std_range)
  
  # Generate plot equations for subtitles
  equations <- create_model_equations(models, log_transform)

  # Define y-axis label based on normalization
  y_axis_label <- if(skip_normalization) "OD (corrected)" else "Normalized OD (B/B0)"

  # Create actual and capped plots
  plots_actual <- create_plots(std_summary, samples_summary$actual, curve_data_list,
                              equations, std_range, y_axis_label, use_capped_data = FALSE)
  plots_capped <- create_plots(std_summary, samples_summary$capped, curve_data_list,
                              equations, std_range, y_axis_label, use_capped_data = TRUE)
  
  # Return results
  return(list(
    models = models,
    best_model = best_model,
    best_model_name = best_model_name,
    model_comparison = model_summary,
    standards = standards,
    samples = samples,
    plots_actual = plots_actual,
    plots_capped = plots_capped,
    equations = equations,
    well_results = bind_rows(
      # Standard wells
      standards %>%
        dplyr::select(Type, Well, OD, OD_cor, Response, Concentration) %>% 
        mutate(
          Predicted_Concentration = Concentration,
          Capped_Concentration = Concentration,
          Model_Used = "Standard",
          Error_Flag = FALSE, 
          Out_of_Range = FALSE,
          Known_Concentration = Concentration,
          Is_Capped = FALSE,
          Is_Unreliable = FALSE
        ) %>%
        dplyr::select(-Concentration),
      
      # Sample wells
      samples %>%
        dplyr::select(Type, Well, OD, OD_cor, Response, 
               Predicted_Concentration, Capped_Concentration, Model_Used, 
               Error_Flag, Out_of_Range, Is_Capped, Is_Unreliable) %>%
        mutate(Known_Concentration = NA)
    ) %>%
      arrange(Type, Well),
    sample_summary = samples_summary$combined,
    std_summary = std_summary,
    nsb_od = nsb_od,
    b0_od = if(exists("b0_od")) b0_od else NA,
    skip_normalization = skip_normalization,
    use_corrected = use_corrected,
    log_transform = log_transform,
    model_errors = model_errors,
    std_range = std_range
  ))
}