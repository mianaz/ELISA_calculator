# ELISA Plotting Functions
# Interactive standard curve plot for the Shiny UI

#' Create a unified standard curve plot
#'
#' @param results ELISA analysis results
#' @param model_choice Model choice (combined, 4pl, linear)
#' @param show_reliability_colors Whether to color-code unreliable values
#' @return Plotly plot object
create_standard_curve_plot <- function(results, model_choice = "combined", show_reliability_colors = TRUE, flip_axes = FALSE,
                                       point_size = 3, line_size = 1.2, palette = "candy_bright",
                                       theme_name = "cowplot", font_family = "Arial",
                                       std_color = "#dc2626", sample_color = "#2563eb", unreliable_color = "#eab308",
                                       show_r2 = TRUE, show_x_errbar = FALSE, show_ci = FALSE,
                                       plot_title = NULL, x_title = NULL, y_title = NULL,
                                       color_samples_individually = FALSE,
                                       return_ggplot = FALSE) {
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
  
  # Build a unified points data frame for standards + samples with Category column.
  # This produces one clean plotly legend entry per category.

  # Standards hover text
  std_points <- std_summary %>%
    dplyr::mutate(
      Category = "Standards",
      x = Concentration,
      y = Mean_Response,
      y_lo = Mean_Response - SD_Response,
      y_hi = Mean_Response + SD_Response,
      x_lo = NA_real_,
      x_hi = NA_real_,
      hover_text = paste0(
        "Standard\n",
        "Conc: ", signif(Concentration, 4), "\n",
        "Response: ", round(Mean_Response, 4), "\n",
        "SD: ", round(SD_Response, 4), "\n",
        "N: ", n
      )
    ) %>%
    dplyr::select(Category, x, y, y_lo, y_hi, x_lo, x_hi, hover_text)

  # Sample hover text & categories
  sample_points <- NULL
  if (nrow(samples_summary) > 0) {
    samples_summary <- samples_summary %>%
      dplyr::mutate(
        hover_text = paste0(
          Type, "\n",
          "Conc: ", signif(Mean_Concentration, 4), "\n",
          "Response: ", round(Mean_Response, 4), "\n",
          "N: ", n,
          ifelse(Is_Unreliable, "\n(Unreliable)", ""),
          ifelse(Has_Out_of_Range, "\n(Out of range)", "")
        )
      )

    if (isTRUE(color_samples_individually)) {
      samples_summary <- samples_summary %>%
        dplyr::mutate(Category = Type)
    } else if (show_reliability_colors) {
      samples_summary <- samples_summary %>%
        dplyr::mutate(Category = ifelse(Is_Unreliable, "Samples (Unreliable)", "Samples (Reliable)"))
    } else {
      samples_summary <- samples_summary %>%
        dplyr::mutate(Category = "Samples")
    }

    sample_points <- samples_summary %>%
      dplyr::mutate(
        x = Mean_Concentration,
        y = Mean_Response,
        y_lo = Mean_Response - SD_Response,
        y_hi = Mean_Response + SD_Response,
        x_lo = pmax(Mean_Concentration - SD_Concentration, min_conc),
        x_hi = Mean_Concentration + SD_Concentration
      ) %>%
      dplyr::select(Category, x, y, y_lo, y_hi, x_lo, x_hi, hover_text)
  }

  # Combine all points
  all_points <- dplyr::bind_rows(std_points, sample_points)

  # Define base colors and shapes per category
  cat_colors <- c(
    "Standards"             = std_color,
    "Samples"               = sample_color,
    "Samples (Reliable)"    = sample_color,
    "Samples (Unreliable)"  = unreliable_color
  )
  cat_shapes <- c(
    "Standards"             = 15,   # square
    "Samples"               = 16,   # circle
    "Samples (Reliable)"    = 16,   # circle
    "Samples (Unreliable)"  = 16    # circle
  )

  # When coloring individually, generate a distinct color per sample type
  if (isTRUE(color_samples_individually) && !is.null(sample_points) && nrow(sample_points) > 0) {
    sample_types <- unique(all_points$Category[all_points$Category %in% unique(samples_summary$Type)])
    n <- length(sample_types)
    if (n > 0) {
      palette_colors <- if (n <= 8) {
        RColorBrewer::brewer.pal(max(3, n), "Set1")[seq_len(n)]
      } else if (n <= 12) {
        RColorBrewer::brewer.pal(max(3, n), "Paired")[seq_len(n)]
      } else {
        scales::hue_pal()(n)
      }
      extra_colors <- setNames(palette_colors, sample_types)
      extra_shapes <- setNames(rep(16L, n), sample_types)
      cat_colors <- c(cat_colors, extra_colors)
      cat_shapes <- c(cat_shapes, extra_shapes)
    }
  }

  # Set factor order so legend is: Standards, then samples
  if (isTRUE(color_samples_individually)) {
    sample_cats <- setdiff(unique(all_points$Category), "Standards")
    cat_levels <- c("Standards", sort(sample_cats))
  } else {
    cat_levels <- c("Standards", "Samples", "Samples (Reliable)", "Samples (Unreliable)")
  }
  all_points$Category <- factor(all_points$Category, levels = cat_levels[cat_levels %in% unique(all_points$Category)])

  # When flip_axes is TRUE, swap x/y so users can read Concentration from OD
  if (flip_axes) {
    all_points <- all_points %>%
      dplyr::mutate(
        x_orig = x, y_orig = y,
        x = y_orig, y = x_orig,
        y_lo_orig = y_lo, y_hi_orig = y_hi,
        x_lo_orig = x_lo, x_hi_orig = x_hi,
        # vertical error bars become the old x error bars
        y_lo = x_lo_orig, y_hi = x_hi_orig,
        # horizontal error bars become the old y error bars
        x_lo = y_lo_orig, x_hi = y_hi_orig
      )

    std_points <- all_points %>% dplyr::filter(Category == "Standards")
    if (!is.null(sample_points) && nrow(sample_points) > 0) {
      sample_points <- all_points %>% dplyr::filter(Category != "Standards")
    }

    if (!is.null(curve_data) && nrow(curve_data) > 0) {
      curve_data <- curve_data %>%
        dplyr::mutate(x_orig = x, x = y, y = x_orig)
    }
  }

  # Build the plot
  p <- ggplot() +
    # All data points with unified legend
    geom_point(data = all_points,
               aes(x = x, y = y, color = Category, shape = Category, text = hover_text),
               size = point_size) +
    scale_color_manual(values = cat_colors, name = NULL) +
    scale_shape_manual(values = cat_shapes, name = NULL) +
    # Error bars for standards (vertical only in normal mode)
    geom_errorbar(data = std_points,
                  aes(x = x, ymin = y_lo, ymax = y_hi),
                  width = 0.1, color = "#dc2626", alpha = 0.4)

  # Add model curves
  if (!is.null(curve_data) && nrow(curve_data) > 0) {
    if (model_choice == "combined") {
      p <- p +
        geom_line(data = curve_data,
                  aes(x = x, y = y, linetype = Model),
                  color = "#475569", linewidth = line_size) +
        scale_linetype_manual(values = c("4PL" = "solid", "5PL" = "longdash", "Linear" = "dotted"),
                              name = "Model")
    } else {
      p <- p +
        geom_line(data = curve_data,
                  aes(x = x, y = y),
                  color = model_color, linewidth = line_size)
    }
  }

  # 95% CI ribbon
  if (isTRUE(show_ci) && !is.null(curve_data) && nrow(curve_data) > 0) {
    tryCatch({
      for (mod_name in unique(curve_data$Model)) {
        mod_obj <- if (mod_name == "4PL") results$models[["4PL"]] else
                   if (mod_name == "5PL") results$models[["5PL"]] else
                   if (mod_name == "Linear") results$models[["Linear"]] else NULL
        if (is.null(mod_obj) || !mod_obj$is_valid()) next
        x_ci <- curve_data$x[curve_data$Model == mod_name]
        ci <- tryCatch(mod_obj$predict_ci(x_ci), error = function(e) NULL)
        if (!is.null(ci) && is.data.frame(ci) && "lower" %in% names(ci)) {
          ci_df <- data.frame(x = x_ci, ymin = ci$lower, ymax = ci$upper)
          p <- p + geom_ribbon(data = ci_df, aes(x = x, ymin = ymin, ymax = ymax),
                               alpha = 0.12, fill = "#475569", inherit.aes = FALSE)
        }
      }
    }, error = function(e) NULL)
  }

  # Error bars for samples (y-axis always; x-axis only if show_x_errbar)
  if (!is.null(sample_points) && nrow(sample_points) > 0) {
    p <- p +
      geom_errorbar(data = sample_points,
                    aes(x = x, ymin = y_lo, ymax = y_hi),
                    width = 0.08, alpha = 0.4, color = "gray40")
    if (isTRUE(show_x_errbar)) {
      p <- p +
        geom_errorbarh(data = sample_points,
                       aes(y = y, xmin = x_lo, xmax = x_hi),
                       height = 0.01, alpha = 0.3, color = "gray50")
    }
  }

  # Determine subtitle based on model choice
  subtitle <- if(model_choice == "combined") {
    paste0("Best model: ", best_model)
  } else if(!is.null(model_equation)) {
    model_equation
  } else {
    paste0(model_name, " parameters")
  }

  # Axis labels and log scale depend on flip_axes
  if (flip_axes) {
    x_label <- y_axis_label
    y_label <- "Concentration (log scale)"
    p <- p +
      scale_y_log10(labels = scales::label_number(), breaks = scales::breaks_log(n = 8)) +
      geom_hline(yintercept = min(std_summary$Concentration, na.rm = TRUE),
                 linetype = "dashed", alpha = 0.3) +
      geom_hline(yintercept = max(std_summary$Concentration, na.rm = TRUE),
                 linetype = "dashed", alpha = 0.3)
  } else {
    x_label <- "Concentration (log scale)"
    y_label <- y_axis_label
    p <- p +
      scale_x_log10(labels = scales::label_number(), breaks = scales::breaks_log(n = 8)) +
      geom_vline(xintercept = min(std_summary$Concentration, na.rm = TRUE),
                 linetype = "dashed", alpha = 0.3) +
      geom_vline(xintercept = max(std_summary$Concentration, na.rm = TRUE),
                 linetype = "dashed", alpha = 0.3)
  }

  # Apply custom titles/labels
  final_title  <- if (!is.null(plot_title)  && nchar(plot_title)  > 0) plot_title  else paste0("ELISA Standard Curve - ", model_name)
  final_xlabel <- if (!is.null(x_title)     && nchar(x_title)     > 0) x_title     else x_label
  final_ylabel <- if (!is.null(y_title)     && nchar(y_title)     > 0) y_title     else y_label

  p <- p +
    labs(
      title    = final_title,
      subtitle = subtitle,
      x        = final_xlabel,
      y        = final_ylabel,
      caption  = caption
    )

  # Apply selected theme
  base_theme <- switch(theme_name,
    "cowplot" = cowplot::theme_cowplot(),
    "prism"   = ggprism::theme_prism(border = TRUE),
    "minimal" = theme_minimal(),
    "classic" = theme_classic(),
    "bw"      = theme_bw(),
    cowplot::theme_cowplot()
  )
  p <- p + base_theme +
    theme(
      text         = element_text(family = font_family),
      plot.title   = element_text(hjust = 0.5, size = 14, face = "bold", family = font_family),
      plot.subtitle = element_text(hjust = 0.5, size = 10, family = font_family),
      axis.title   = element_text(size = 12, family = font_family),
      axis.text    = element_text(family = font_family),
      plot.caption = element_text(hjust = 0, color = "red", size = 10),
      legend.position = "bottom"
    )

  # Add R² annotation if requested
  if (isTRUE(show_r2)) {
    r2_val <- tryCatch({
      best_mod_obj <- results$models[[results$best_model_name]]
      if (!is.null(best_mod_obj) && best_mod_obj$is_valid()) {
        stats <- best_mod_obj$get_fit_stats()
        if (!is.null(stats$r_squared)) round(stats$r_squared, 4) else NULL
      } else NULL
    }, error = function(e) NULL)
    if (!is.null(r2_val)) {
      p <- p + annotate("text", x = -Inf, y = Inf,
                        label = paste0("R² = ", r2_val),
                        hjust = -0.1, vjust = 1.5, size = 4,
                        family = font_family, fontface = "italic")
    }
  }
  
  # Return raw ggplot if requested (for PNG export)
  if (isTRUE(return_ggplot)) return(p)

  # Convert to plotly for interactivity
  p_plotly <- ggplotly(p, tooltip = "text") %>%
    layout(
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.15,
        xanchor = "center",
        font = list(size = 12),
        itemsizing = "constant"
      ),
      margin = list(b = 100)
    )

  # Clean up plotly trace names for readable legends
  seen_groups <- list()
  for (i in seq_along(p_plotly$x$data)) {
    trace <- p_plotly$x$data[[i]]
    raw_name <- trace$name %||% ""

    # Strip ggplotly's parenthesized tuple formats:
    #   "(value,1)" -> "value"
    #   "(value,1,NA)" -> "value"
    #   "(Standards,1,Standards)" -> "Standards"
    cleaned <- gsub("^\\(([^,]+)(?:,[^)]*)*\\)$", "\\1", raw_name, perl = TRUE)

    p_plotly$x$data[[i]]$name <- cleaned
    p_plotly$x$data[[i]]$legendgroup <- cleaned

    # Hide duplicate legend entries and unnamed traces (error bars, vlines)
    if (cleaned == "" || cleaned %in% names(seen_groups)) {
      p_plotly$x$data[[i]]$showlegend <- FALSE
    } else {
      seen_groups[[cleaned]] <- TRUE
    }
  }

  return(p_plotly)
}