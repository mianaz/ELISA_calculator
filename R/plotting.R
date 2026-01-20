# ELISA Plotting Functions
# Functions for creating plots of ELISA standard curves and samples

#' Generate curve data for plotting
#'
#' @param models List of fitted models
#' @param standards Standards data frame
#' @param log_transform Whether to log transform concentrations
#' @param offset Small value for log transformation
#' @return List of curve data for each model
generate_curve_data <- function(models, standards, log_transform = TRUE, offset = 1e-5) {
  # Create a sequence of concentrations for curve plotting
  min_conc <- min(standards$Concentration[standards$Concentration > 0], na.rm = TRUE) / 10
  max_conc <- max(standards$Concentration, na.rm = TRUE) * 10
  conc_seq <- exp(seq(from = log(min_conc), to = log(max_conc), length.out = 100))
  
  if(log_transform) {
    log_conc_seq <- log10(conc_seq)
  }
  
  # Generate curve data for models
  curve_data_list <- list()
  
  # Generate curve data for 4PL model
  if(!is.null(models$fourpl)) {
    tryCatch({
      if(log_transform) {
        pred_response <- predict(models$fourpl, newdata = data.frame(logConc = log_conc_seq))
      } else {
        pred_response <- predict(models$fourpl, newdata = data.frame(Concentration = conc_seq))
      }
      curve_data_list$fourpl <- data.frame(Concentration = conc_seq, Response = pred_response, Model = "4PL")
    }, error = function(e) {
      # Try manual calculation as fallback
      tryCatch({
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
          stop("Cannot identify parameter names")
        }
        
        # Calculate 4PL curve manually
        pred_response <- d + (a - d) / (1 + (conc_seq / 10^c)^b)
        curve_data_list$fourpl <- data.frame(Concentration = conc_seq, Response = pred_response, Model = "4PL")
      }, error = function(e2) {
        # Fail silently if manual calculation also fails
      })
    })
  }
  
  # Generate curve data for linear model
  if(!is.null(models$linear)) {
    tryCatch({
      if(log_transform) {
        pred_response <- predict(models$linear, newdata = data.frame(logConc = log_conc_seq))
      } else {
        pred_response <- predict(models$linear, newdata = data.frame(Concentration = conc_seq))
      }
      curve_data_list$linear <- data.frame(Concentration = conc_seq, Response = pred_response, Model = "Linear")
    }, error = function(e) {
      # Fail silently if calculation fails
    })
  }
  
  return(curve_data_list)
}

#' Create model equations for plot subtitles
#'
#' @param models List of fitted models
#' @param log_transform Whether to log transform concentrations
#' @return List of equation strings for each model
create_model_equations <- function(models, log_transform = TRUE) {
  equations <- list()
  
  # 4PL equation
  if(!is.null(models$fourpl)) {
    tryCatch({
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
        stop("Cannot identify parameter names")
      }
      
      if(log_transform) {
        equations$fourpl <- paste0(
          "y = ", round(d, 3), " + (", round(a, 3), " - ", round(d, 3), 
          ")/(1 + (10^x/10^", round(c, 3), ")^", round(b, 3), ")"
        )
      } else {
        equations$fourpl <- paste0(
          "y = ", round(d, 3), " + (", round(a, 3), " - ", round(d, 3), 
          ")/(1 + (x/", round(c, 3), ")^", round(b, 3), ")"
        )
      }
      
      # Calculate R-squared for 4PL
      fitted_values <- fitted(models$fourpl)
      observed_values <- models$fourpl$data$Response
      tss <- sum((observed_values - mean(observed_values))^2)
      rss <- sum((observed_values - fitted_values)^2)
      r_squared_4pl <- 1 - (rss/tss)
      
      equations$fourpl <- paste0(equations$fourpl, ", R² = ", round(r_squared_4pl, 4))
      
    }, error = function(e) {
      equations$fourpl <- "Error generating equation"
    })
  }
  
  # Linear equation
  if(!is.null(models$linear)) {
    tryCatch({
      coefs <- coef(models$linear)
      r_squared <- summary(models$linear)$r.squared
      
      if(log_transform) {
        equations$linear <- paste0(
          "y = ", round(coefs[1], 3), " + ", round(coefs[2], 3), " * log10(x)",
          ", R² = ", round(r_squared, 4)
        )
      } else {
        equations$linear <- paste0(
          "y = ", round(coefs[1], 3), " + ", round(coefs[2], 3), " * x",
          ", R² = ", round(r_squared, 4)
        )
      }
    }, error = function(e) {
      equations$linear <- "Error generating equation"
    })
  }
  
  return(equations)
}

#' Create standard curve plots
#'
#' @param std_summary Summarized standards data
#' @param samples_summary Summarized samples data
#' @param curve_data_list List of curve data for each model
#' @param equations Model equations
#' @param std_range Range of standard concentrations
#' @param y_axis_label Y-axis label for plots
#' @param use_capped_data Whether to use capped sample data
#' @return List of plot objects for different models
create_plots <- function(std_summary, samples_summary, curve_data_list, equations, 
                         std_range, y_axis_label, use_capped_data = FALSE) {
  # Title suffix based on data mode
  title_suffix <- if(use_capped_data) " (Out-of-Range Values Capped)" else " (Actual Values)"
  
  # 1. Combined model plot
  combined_equation <- paste0(
    "4PL: ", ifelse(!is.null(equations[[MODEL_4PL]]), equations[[MODEL_4PL]], "Not available"),
    "\nLinear: ", ifelse(!is.null(equations[[MODEL_LINEAR]]), equations[[MODEL_LINEAR]], "Not available")
  )
  
  p_combined <- ggplot() +
    # Standard points
    geom_point(data = std_summary, aes(x = Concentration, y = Mean_Response), 
               color = "red", size = 3, shape = 16) +
    # Error bars for standards
    geom_errorbar(data = std_summary, 
                  aes(x = Concentration, 
                      ymin = Mean_Response - SD_Response,
                      ymax = Mean_Response + SD_Response),
                  width = 0.1, color = "red", alpha = 0.5)
  
  # Add curves for both models
  if(!is.null(curve_data_list) && length(curve_data_list) > 0) {
    # Combine all curve data
    curve_data <- do.call(rbind, curve_data_list)
    
    if(nrow(curve_data) > 0) {
      p_combined <- p_combined + 
        geom_line(data = curve_data, aes(x = Concentration, y = Response, color = Model, linetype = Model), 
                  size = 1)
    }
  }
  
  # Only add samples if we have any (other than blanks)
  if(nrow(samples_summary) > 0) {
    p_combined <- p_combined +
      # Sample points with different shapes
      geom_point(data = samples_summary, 
                 aes(x = Mean_Concentration, y = Mean_Response, color = Type, shape = Type), 
                 size = 3) +
      # Error bars for samples (horizontal for concentration)
      geom_errorbarh(data = samples_summary,
                     aes(y = Mean_Response,
                         xmin = Mean_Concentration - SD_Concentration,
                         xmax = Mean_Concentration + SD_Concentration,
                         color = Type),
                     height = 0.01, alpha = 0.5) +
      # Error bars for samples (vertical for response)
      geom_errorbar(data = samples_summary,
                    aes(x = Mean_Concentration,
                        y = Mean_Response,
                        ymin = Mean_Response - SD_Response,
                        ymax = Mean_Response + SD_Response,
                        color = Type),
                    width = 0.1, alpha = 0.5)
  }
  
  # Create warning text for out-of-range samples if in actual mode
  caption <- NULL
  
  if(!use_capped_data) {
    # In actual mode, we use warning text for out-of-range values
    out_of_range_samples <- samples_summary %>%
      dplyr::filter(Has_Out_of_Range) %>%
      dplyr::pull(Type) %>%
      unique()
    
    if(length(out_of_range_samples) > 0) {
      warning_text <- paste("Note: The following samples are outside the standard curve range: ",
                           paste(out_of_range_samples, collapse=", "))
      caption <- warning_text
    }
  }
  
  # Complete the plot
  p_combined <- p_combined +
    scale_x_log10(
      labels = scales::label_number(),
      breaks = scales::breaks_log(n = 8)
    ) +
    labs(
      title = paste0("ELISA Standard Curve - All Models", title_suffix),
      subtitle = combined_equation,
      x = "Concentration (log scale)",
      y = y_axis_label,
      caption = caption
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 9),
      axis.title = element_text(size = 12),
      plot.caption = element_text(hjust = 0, color = "red", size = 10),
      legend.position = "bottom"
    )
  
  # Add markers for standard range
  p_combined <- p_combined + 
    geom_vline(xintercept = min(std_summary$Concentration, na.rm = TRUE), 
               linetype = "dashed", alpha = 0.3) +
    geom_vline(xintercept = max(std_summary$Concentration, na.rm = TRUE), 
               linetype = "dashed", alpha = 0.3)
  
  # 2. 4PL model plot
  p_4pl <- NULL
  if(!is.null(curve_data_list[[MODEL_4PL]])) {
    p_4pl <- ggplot() +
      # Standard points
      geom_point(data = std_summary, aes(x = Concentration, y = Mean_Response), 
                 color = "red", size = 3, shape = 16) +
      # Error bars for standards
      geom_errorbar(data = std_summary, 
                    aes(x = Concentration, 
                        ymin = Mean_Response - SD_Response,
                        ymax = Mean_Response + SD_Response),
                    width = 0.1, color = "red", alpha = 0.5) +
      # 4PL curve
      geom_line(data = curve_data_list[[MODEL_4PL]], aes(x = Concentration, y = Response),
                color = "blue", size = 1)
    
    # Only add samples if we have any
    if(nrow(samples_summary) > 0) {
      p_4pl <- p_4pl +
        # Sample points with different shapes
        geom_point(data = samples_summary, 
                   aes(x = Mean_Concentration, y = Mean_Response, color = Type, shape = Type), 
                   size = 3) +
        # Error bars for samples
        geom_errorbarh(data = samples_summary,
                       aes(y = Mean_Response,
                           xmin = Mean_Concentration - SD_Concentration,
                           xmax = Mean_Concentration + SD_Concentration,
                           color = Type),
                       height = 0.01, alpha = 0.5) +
        geom_errorbar(data = samples_summary,
                      aes(x = Mean_Concentration,
                          y = Mean_Response,
                          ymin = Mean_Response - SD_Response,
                          ymax = Mean_Response + SD_Response,
                          color = Type),
                      width = 0.1, alpha = 0.5)
    }
    
    # Create warning text for out-of-range samples if in actual mode
    caption <- NULL
    
    if(!use_capped_data) {
      # In actual mode, we use warning text for out-of-range values
      out_of_range_samples <- samples_summary %>%
        dplyr::filter(Has_Out_of_Range) %>%
        dplyr::pull(Type) %>%
        unique()
      
      if(length(out_of_range_samples) > 0) {
        warning_text <- paste("Note: The following samples are outside the 4PL standard curve range: ",
                             paste(out_of_range_samples, collapse=", "))
        caption <- warning_text
      }
    }
    
    p_4pl <- p_4pl +
      scale_x_log10(
        labels = scales::label_number(),
        breaks = scales::breaks_log(n = 8)
      ) +
      labs(
        title = paste0("ELISA Standard Curve - 4PL Model", title_suffix),
        subtitle = ifelse(!is.null(equations[[MODEL_4PL]]), equations[[MODEL_4PL]], "Equation not available"),
        x = "Concentration (log scale)",
        y = y_axis_label,
        caption = caption
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0, color = "red", size = 10),
        legend.position = "bottom"
      )
    
    # Mark the standard curve range
    p_4pl <- p_4pl + 
      geom_vline(xintercept = min(std_summary$Concentration, na.rm = TRUE), 
                 linetype = "dashed", alpha = 0.3) +
      geom_vline(xintercept = max(std_summary$Concentration, na.rm = TRUE), 
                 linetype = "dashed", alpha = 0.3)
  }
  
  # 3. Linear model plot
  p_linear <- NULL
  if(!is.null(curve_data_list[[MODEL_LINEAR]])) {
    p_linear <- ggplot() +
      # Standard points
      geom_point(data = std_summary, aes(x = Concentration, y = Mean_Response), 
                 color = "red", size = 3, shape = 16) +
      # Error bars for standards
      geom_errorbar(data = std_summary, 
                    aes(x = Concentration, 
                        ymin = Mean_Response - SD_Response,
                        ymax = Mean_Response + SD_Response),
                    width = 0.1, color = "red", alpha = 0.5) +
      # Linear curve
      geom_line(data = curve_data_list[[MODEL_LINEAR]], aes(x = Concentration, y = Response),
                color = "green", size = 1)
    
    # Only add samples if we have any
    if(nrow(samples_summary) > 0) {
      p_linear <- p_linear +
        # Sample points with different shapes
        geom_point(data = samples_summary, 
                   aes(x = Mean_Concentration, y = Mean_Response, color = Type, shape = Type), 
                   size = 3) +
        # Error bars for samples
        geom_errorbarh(data = samples_summary,
                       aes(y = Mean_Response,
                           xmin = Mean_Concentration - SD_Concentration,
                           xmax = Mean_Concentration + SD_Concentration,
                           color = Type),
                       height = 0.01, alpha = 0.5) +
        geom_errorbar(data = samples_summary,
                      aes(x = Mean_Concentration,
                          y = Mean_Response,
                          ymin = Mean_Response - SD_Response,
                          ymax = Mean_Response + SD_Response,
                          color = Type),
                      width = 0.1, alpha = 0.5)
    }
    
    # Create warning text for out-of-range samples if in actual mode
    caption <- NULL
    
    if(!use_capped_data) {
      # In actual mode, we use warning text for out-of-range values
      out_of_range_samples <- samples_summary %>%
        dplyr::filter(Has_Out_of_Range) %>%
        dplyr::pull(Type) %>%
        unique()
      
      if(length(out_of_range_samples) > 0) {
        warning_text <- paste("Note: The following samples are outside the Linear standard curve range: ",
                             paste(out_of_range_samples, collapse=", "))
        caption <- warning_text
      }
    }
    
    p_linear <- p_linear +
      scale_x_log10(
        labels = scales::label_number(),
        breaks = scales::breaks_log(n = 8)
      ) +
      labs(
        title = paste0("ELISA Standard Curve - Linear Model", title_suffix),
        subtitle = ifelse(!is.null(equations[[MODEL_LINEAR]]), equations[[MODEL_LINEAR]], "Equation not available"),
        x = "Concentration (log scale)",
        y = y_axis_label,
        caption = caption
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 12),
        plot.caption = element_text(hjust = 0, color = "red", size = 10),
        legend.position = "bottom"
      )
    
    # Mark the standard curve range
    p_linear <- p_linear + 
      geom_vline(xintercept = min(std_summary$Concentration, na.rm = TRUE), 
                 linetype = "dashed", alpha = 0.3) +
      geom_vline(xintercept = max(std_summary$Concentration, na.rm = TRUE), 
                 linetype = "dashed", alpha = 0.3)
  }
  
  return(list(
    combined = p_combined,
    fourpl = p_4pl,
    linear = p_linear
  ))
}

#' Create a unified standard curve plot
#'
#' @param results ELISA analysis results
#' @param model_choice Model choice (combined, 4pl, linear)
#' @param show_reliability_colors Whether to color-code unreliable values
#' @return Plotly plot object
create_standard_curve_plot <- function(results, model_choice = "combined", show_reliability_colors = TRUE) {
  # Extract needed data elements
  std_summary <- results$std_summary
  samples_data <- results$samples
  log_transform <- results$log_transform
  std_range <- results$std_range
  y_axis_label <- if(results$skip_normalization) "OD (corrected)" else "Normalized OD (B/B0)"
  best_model <- results$best_model_name
  
  # Extract appropriate model
  model <- NULL
  model_name <- ""
  model_color <- "blue"
  model_equation <- NULL

  if(model_choice == "combined") {
    # For combined view, we'll use all models
    model_name <- "Combined Models"
    fourpl_model <- results$models[[MODEL_4PL]]
    fivepl_model <- results$models[[MODEL_5PL]]
    linear_model <- results$models[[MODEL_LINEAR]]
  } else if(model_choice == "4pl") {
    # For 4PL view
    model <- results$models[[MODEL_4PL]]
    model_name <- "4PL Model"
    model_color <- "blue"
    model_equation <- if(!is.null(results$equations[[MODEL_4PL]])) results$equations[[MODEL_4PL]] else "Equation not available"

    if(is.null(model) || !model$is_valid()) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "4PL model was not fitted"))
    }
  } else if(model_choice == "5pl") {
    # For 5PL view
    model <- results$models[[MODEL_5PL]]
    model_name <- "5PL Model"
    model_color <- "purple"
    model_equation <- if(!is.null(results$equations[[MODEL_5PL]])) results$equations[[MODEL_5PL]] else "Equation not available"

    if(is.null(model) || !model$is_valid()) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "5PL model was not fitted"))
    }
  } else if(model_choice == "linear") {
    # For linear view
    model <- results$models[[MODEL_LINEAR]]
    model_name <- "Linear Model"
    model_color <- "green"
    model_equation <- if(!is.null(results$equations[[MODEL_LINEAR]])) results$equations[[MODEL_LINEAR]] else "Equation not available"

    if(is.null(model) || !model$is_valid()) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
             layout(title = "Linear model was not fitted"))
    }
  }
  
  # Generate curve data points for x-axis
  min_conc <- min(std_summary$Concentration[std_summary$Concentration > 0], na.rm = TRUE) / 10
  max_conc <- max(std_summary$Concentration, na.rm = TRUE) * 10
  x_points <- exp(seq(log(min_conc), log(max_conc), length.out = 100))
  
  # Initialize curve data list
  curve_data_list <- list()
  
  # Generate appropriate curve data based on model selection
  if(model_choice == "combined") {
    # Generate curves for all models
    if(!is.null(fourpl_model) && fourpl_model$is_valid()) {
      y_points_4pl <- tryCatch({
        fourpl_model$predict_response(x_points)
      }, error = function(e) { NULL })

      if(!is.null(y_points_4pl)) {
        curve_data_list$fourpl <- data.frame(x = x_points, y = y_points_4pl, Model = "4PL")
      }
    }

    if(!is.null(fivepl_model) && fivepl_model$is_valid()) {
      y_points_5pl <- tryCatch({
        fivepl_model$predict_response(x_points)
      }, error = function(e) { NULL })

      if(!is.null(y_points_5pl)) {
        curve_data_list$fivepl <- data.frame(x = x_points, y = y_points_5pl, Model = "5PL")
      }
    }

    if(!is.null(linear_model) && linear_model$is_valid()) {
      y_points_linear <- tryCatch({
        linear_model$predict_response(x_points)
      }, error = function(e) { NULL })

      if(!is.null(y_points_linear)) {
        curve_data_list$linear <- data.frame(x = x_points, y = y_points_linear, Model = "Linear")
      }
    }
  } else if((model_choice == "4pl" || model_choice == "5pl" || model_choice == "linear") &&
            !is.null(model) && model$is_valid()) {
    # Generate curve for single model
    y_points <- tryCatch({
      model$predict_response(x_points)
    }, error = function(e) {
      # If prediction fails, use simple line
      seq(min(std_summary$Mean_Response), max(std_summary$Mean_Response), length.out = 100)
    })

    curve_data_list$single <- data.frame(x = x_points, y = y_points, Model = model_name)
  }
  
  # Combine all curve data
  curve_data <- do.call(rbind, curve_data_list)
  
  # Process sample data - always show all samples with appropriate coloring
  samples_summary <- samples_data %>%
    # Filter out samples with no response values
    dplyr::filter(!is.na(Response)) %>%
    dplyr::group_by(Type) %>%
    dplyr::summarize(
      Mean_Response = mean(Response, na.rm = TRUE),
      SD_Response = sd(Response, na.rm = TRUE),
      Mean_Concentration = mean(Capped_Concentration, na.rm = TRUE),  # Always use capped concentration
      SD_Concentration = sd(Capped_Concentration, na.rm = TRUE),
      Has_Out_of_Range = any(Out_of_Range, na.rm = TRUE),
      Has_Uncalculatable = any(Model_Used == "Uncalculatable", na.rm = TRUE),
      Is_Unreliable = any(Is_Unreliable, na.rm = TRUE),
      Model_Used = first(na.omit(Model_Used)),
      n = n(),
      .groups = "drop"
    ) %>%
    dplyr::filter(!Type %in% c("NSB", "B0", "Standard", "Blank"))
  
  # Create warning text for out-of-range and uncalculatable samples
  warning_messages <- character(0)
  
  # Check for out-of-range samples
  out_of_range_samples <- samples_data %>%
    dplyr::filter(Out_of_Range == TRUE) %>%
    dplyr::pull(Type) %>%
    unique()
  
  if(length(out_of_range_samples) > 0) {
    warning_messages <- c(warning_messages,
                        paste("Note: The following samples are outside the standard curve range: ",
                              paste(out_of_range_samples, collapse=", ")))
  }
  
  # Check for uncalculatable samples
  uncalculatable_samples <- samples_data %>%
    dplyr::filter(Model_Used == "Uncalculatable") %>%
    dplyr::pull(Type) %>%
    unique()
  
  if(length(uncalculatable_samples) > 0) {
    warning_messages <- c(warning_messages,
                         paste("Warning: The following samples could not be calculated and use estimated values: ",
                               paste(uncalculatable_samples, collapse=", ")))
  }
  
  # Combine warnings if needed
  if(length(warning_messages) > 0) {
    caption <- paste(warning_messages, collapse = "\n")
  } else {
    caption <- NULL
  }
  
  # Create the plot
  p <- ggplot() +
    # Plot standards
    geom_point(data = std_summary, aes(x = Concentration, y = Mean_Response),
             color = "red", size = 3, shape = 16) +
    # Error bars for standards
    geom_errorbar(data = std_summary,
                aes(x = Concentration,
                    ymin = Mean_Response - SD_Response,
                    ymax = Mean_Response + SD_Response),
                width = 0.1, color = "red", alpha = 0.5)
  
  # Add model curves if we have data
  if(!is.null(curve_data) && nrow(curve_data) > 0) {
    if(model_choice == "combined") {
      # For combined models, show both with different colors/styles
      p <- p +
        geom_line(data = curve_data,
                aes(x = x, y = y, color = Model, linetype = Model),
                size = 1)
    } else {
      # For single model, use appropriate color
      p <- p +
        geom_line(data = curve_data,
                aes(x = x, y = y),
                color = model_color, size = 1)
    }
  }
  
  # Add samples to plot if any exist
  if(nrow(samples_summary) > 0) {
    if(show_reliability_colors) {
      # Add color-coding for reliability
      p <- p +
        # Sample points with reliability coloring and different shapes
        geom_point(data = samples_summary,
                 aes(x = Mean_Concentration, y = Mean_Response, 
                     color = Is_Unreliable, shape = Type),
                 size = 3) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                        name = "Unreliable Value")
    } else {
      # Regular coloring by sample type
      p <- p +
        # Sample points with different shapes
        geom_point(data = samples_summary,
                 aes(x = Mean_Concentration, y = Mean_Response, color = Type, shape = Type),
                 size = 3)
    }
    
    # Add error bars for all samples
    p <- p +
      # Error bars for samples (horizontal for concentration)
      geom_errorbarh(data = samples_summary,
                    aes(y = Mean_Response,
                        xmin = pmax(Mean_Concentration - SD_Concentration, min_conc),
                        xmax = Mean_Concentration + SD_Concentration),
                    height = 0.01, alpha = 0.5, color = "gray50") +
      # Error bars for samples (vertical for response)
      geom_errorbar(data = samples_summary,
                  aes(x = Mean_Concentration,
                      y = Mean_Response,
                      ymin = Mean_Response - SD_Response,
                      ymax = Mean_Response + SD_Response),
                  width = 0.1, alpha = 0.5, color = "gray50")
  }
  
  # Determine subtitle based on model choice
  subtitle <- if(model_choice == "combined") {
    paste0("Best model: ", best_model)
  } else if(!is.null(model_equation)) {
    model_equation
  } else {
    paste0(model_name, " parameters")
  }
  
  # Add labels and formatting
  p <- p +
    # Formatting
    scale_x_log10(labels = scales::label_number(), breaks = scales::breaks_log(n = 8)) +
    # Labels
    labs(
      title = paste0("ELISA Standard Curve - ", model_name),
      subtitle = subtitle,
      x = "Concentration (log scale)",
      y = y_axis_label,
      caption = caption
    ) +
    # Theme
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.title = element_text(size = 12),
      plot.caption = element_text(hjust = 0, color = "red", size = 10),
      legend.position = "bottom"
    ) +
    # Standard curve range indicators
    geom_vline(xintercept = min(std_summary$Concentration, na.rm = TRUE),
             linetype = "dashed", alpha = 0.3) +
    geom_vline(xintercept = max(std_summary$Concentration, na.rm = TRUE),
             linetype = "dashed", alpha = 0.3)
  
  # Convert to plotly for interactivity
  p_plotly <- ggplotly(p) %>%
    layout(
      legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center"),
      margin = list(b = 120)  # Add bottom margin for legend and warning
    )
  
  return(p_plotly)
}