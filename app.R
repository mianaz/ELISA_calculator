# Main Shiny application file for ELISA Standard Curve Calculator v2.0
library(shiny)
library(shinydashboard)
library(shinyBS)
library(DT)
library(readxl)
library(readr)
library(writexl)
library(drc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)
library(plotly)
library(magrittr)
library(tibble)
library(R6)

# Load modular components
source("R/ui.R")
source("R/analysis/analyzer.R")  # Generalized analyzer with all dependencies

# Create UI
ui <- create_ui()

# Define server logic
server <- function(input, output, session) {
  # Define a helper function for NULL handling
  `%||%` <- function(a, b) if (is.null(a)) b else a

  # Reactive values to store analysis results
  results <- reactiveVal(NULL)
  raw_data <- reactiveVal(NULL)
  exclusion_audit_trail <- reactiveVal(list(entries = list(), created_at = Sys.time()))

  # Helper function to parse dilution factors from text input
  parse_dilution_factors <- function(text) {
    if (is.null(text) || text == "") return(NULL)

    lines <- strsplit(text, "\n")[[1]]
    lines <- trimws(lines)
    lines <- lines[lines != ""]

    if (length(lines) == 0) return(NULL)

    factors <- list()
    for (line in lines) {
      parts <- strsplit(line, "=")[[1]]
      if (length(parts) == 2) {
        sample_type <- trimws(parts[1])
        factor_val <- as.numeric(trimws(parts[2]))
        if (!is.na(factor_val)) {
          factors[[sample_type]] <- factor_val
        }
      }
    }

    if (length(factors) == 0) return(NULL)
    return(unlist(factors))
  }

  # Helper function to read data file based on extension
  read_data_file <- function(file_path, file_name, sheet = NULL) {
    ext <- tolower(tools::file_ext(file_name))

    if (ext %in% c("xlsx", "xls")) {
      # Excel file
      if (is.null(sheet)) {
        sheet_names <- readxl::excel_sheets(file_path)
        sheet <- if(length(sheet_names) > 0) sheet_names[1] else 1
      }
      return(readxl::read_excel(file_path, sheet = sheet))
    } else if (ext == "csv") {
      # CSV file (comma-separated)
      return(readr::read_csv(file_path, show_col_types = FALSE))
    } else if (ext == "tsv") {
      # TSV file (tab-separated)
      return(readr::read_tsv(file_path, show_col_types = FALSE))
    } else {
      stop("Unsupported file format. Please use .xlsx, .xls, .csv, or .tsv files.")
    }
  }

  # Helper function to check if file is Excel
  is_excel_file <- function(file_name) {
    ext <- tolower(tools::file_ext(file_name))
    return(ext %in% c("xlsx", "xls"))
  }

  # Debounced version of analysis trigger
  analysis_trigger <- debounce(reactive({
    list(
      file = input$file,
      sheet = input$sheet,
      well_col = input$well_col,
      type_col = input$type_col,
      conc_col = input$conc_col,
      od_col = input$od_col,
      od_corr_col = input$od_corr_col,
      use_corrected = input$use_corrected,
      assay_type = input$assay_type,
      nsb_label = input$nsb_label,
      b0_label = input$b0_label,
      std_label = input$std_label,
      blank_label = input$blank_label,
      skip_normalization = input$skip_normalization,
      log_transform = input$log_transform,
      remove_unlabeled = input$remove_unlabeled,
      weight_type = input$weight_type,
      models_to_fit = input$models_to_fit,
      dilution_factors_text = input$dilution_factors_text,
      calculate_limits = input$calculate_limits
    )
  }), 500)  # 500ms debounce

  # Set flags for UI conditional elements
  output$fileUploaded <- reactive({
    return(!is.null(input$file))
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

  output$analysisComplete <- reactive({
    return(!is.null(results()))
  })
  outputOptions(output, "analysisComplete", suspendWhenHidden = FALSE)

  # Data preview output
  output$rawPreview <- renderDT({
    req(input$file)

    # If no raw data is stored yet, try to read it
    if(is.null(raw_data())) {
      tryCatch({
        # Use helper function to read data based on file type
        if (is_excel_file(input$file$name)) {
          # Excel file - try to read from currently selected sheet
          sheet_to_use <- if(!is.null(input$sheet)) input$sheet else NULL
          data <- read_data_file(input$file$datapath, input$file$name, sheet = sheet_to_use)
        } else {
          # CSV/TSV file - no sheet selection needed
          data <- read_data_file(input$file$datapath, input$file$name)
        }

        # Store the data for later use
        raw_data(data)
      }, error = function(e) {
        # Just return an error without storing data
        return(
          DT::datatable(data.frame(Error = paste("Could not read data:", e$message)),
                      options = list(pageLength = 10, dom = 't'))
        )
      })
    }

    # Use the stored raw data for display
    if(!is.null(raw_data())) {
      return(
        DT::datatable(raw_data(),
                  options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'),
                  rownames = FALSE)
      )
    } else {
      # Fallback for when there's no data
      return(
        DT::datatable(data.frame(Message = "No data available. Please upload a file."),
                    options = list(pageLength = 10, dom = 't'))
      )
    }
  })

  # Observer for analyze button
  observeEvent(input$analyze, {
    req(input$file)
    req(input$sheet)

    # Show a progress notification
    withProgress(message = "Analyzing ELISA data...", value = 0, {

      # Increase the progress bar
      incProgress(0.2, detail = "Processing data")

      # Parse dilution factors from text input
      dilution_factors <- parse_dilution_factors(input$dilution_factors_text)

      # Get models to fit (ensure at least one)
      models <- input$models_to_fit
      if (is.null(models) || length(models) == 0) {
        models <- c("4PL", "Linear")
      }

      incProgress(0.1, detail = "Fitting models")

      # Run the analysis with user-selected parameters
      tryCatch({
        analysis_results <- analyze_elisa(
          file_path = input$file$datapath,
          sheet_name = input$sheet,
          file_name = input$file$name,
          well_col = input$well_col,
          type_col = input$type_col,
          conc_col = input$conc_col,
          od_col = input$od_col,
          od_corr_col = if(input$od_corr_col == "None") input$od_col else input$od_corr_col,
          use_corrected = input$use_corrected %||% FALSE,
          assay_type = input$assay_type %||% "auto",
          nsb_label = input$nsb_label %||% "NSB",
          b0_label = input$b0_label %||% "B0",
          std_label = input$std_label %||% "Standard",
          blank_label = input$blank_label %||% "Blank",
          skip_normalization = input$skip_normalization %||% FALSE,
          log_transform = input$log_transform %||% TRUE,
          remove_unlabeled = input$remove_unlabeled %||% TRUE,
          dilution_factors = dilution_factors,
          weight_type = input$weight_type %||% "none",
          models_to_fit = models,
          calculate_limits = input$calculate_limits %||% TRUE
        )

        incProgress(0.5, detail = "Generating outputs")

        # Store updated results
        results(analysis_results)

        # Final progress
        incProgress(0.2, detail = "Complete")

        # Show a success notification with QC status
        qc_status <- if (!is.null(analysis_results$qc_summary)) {
          analysis_results$qc_summary$overall_status
        } else {
          "COMPLETE"
        }

        showNotification(
          paste("Analysis completed! QC Status:", qc_status),
          type = if(qc_status == "PASS") "message" else "warning"
        )

      }, error = function(e) {
        # Display error to user
        showNotification(
          paste("Error in analysis:", e$message),
          type = "error",
          duration = 10
        )
      })
    })
  })

  # Observer for file input
  observeEvent(input$file, {
    req(input$file)

    # Reset raw data when new file is uploaded
    raw_data(NULL)

    # Get file extension
    ext <- tolower(tools::file_ext(input$file$name))

    tryCatch({
      # Read file based on extension
      if (ext %in% c("xlsx", "xls")) {
        # Excel file - get sheet names
        sheet_names <- readxl::excel_sheets(input$file$datapath)
        updateSelectInput(session, "sheet", choices = sheet_names)

        # Use first sheet by default
        sheet_to_use <- if(length(sheet_names) > 0) sheet_names[1] else 1

        # Read first sheet to get column names
        first_sheet_data <- readxl::read_excel(input$file$datapath, sheet = sheet_to_use)
        column_names <- colnames(first_sheet_data)

      } else if (ext %in% c("csv", "tsv")) {
        # CSV/TSV file - no sheets, set to "Data"
        updateSelectInput(session, "sheet", choices = c("Data"), selected = "Data")

        # Read data to get column names
        if (ext == "csv") {
          first_sheet_data <- readr::read_csv(input$file$datapath, show_col_types = FALSE)
        } else {
          first_sheet_data <- readr::read_tsv(input$file$datapath, show_col_types = FALSE)
        }
        column_names <- colnames(first_sheet_data)

      } else {
        showNotification("Unsupported file format. Please use .xlsx, .xls, .csv, or .tsv files.", type = "error")
        return()
      }

      # Update column selection dropdowns
      updateSelectInput(session, "well_col", choices = column_names,
                       selected = if("Well ID" %in% column_names) "Well ID" else column_names[1])
      updateSelectInput(session, "type_col", choices = column_names,
                       selected = if("Sample Type" %in% column_names) "Sample Type" else column_names[2])
      updateSelectInput(session, "conc_col", choices = column_names,
                       selected = if("Concentration" %in% column_names) "Concentration" else column_names[3])
      updateSelectInput(session, "od_col", choices = column_names,
                       selected = if("OD" %in% column_names) "OD" else column_names[4])
      updateSelectInput(session, "od_corr_col", choices = c("None", column_names),
                       selected = if("OD (corrected)" %in% column_names) "OD (corrected)" else "None")

      # Store raw data for preview
      raw_data(first_sheet_data)

      # Show a notification that file is loaded and ready for analysis
      file_type <- if(ext %in% c("xlsx", "xls")) "Excel" else toupper(ext)
      showNotification(paste0(file_type, " file loaded successfully. Please click 'Analyze Data' when ready."),
                      type = "message", duration = 5)

    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error", duration = 10)
    })
  })

  # Helper function to load example data
  load_example_data <- function(example_file, example_name) {
    tryCatch({
      # Read the example data
      example_path <- file.path("example_data", example_file)

      if (!file.exists(example_path)) {
        showNotification(paste("Example file not found:", example_file), type = "error")
        return()
      }

      # Read the CSV data
      data <- readr::read_csv(example_path, show_col_types = FALSE)
      column_names <- colnames(data)

      # Store raw data for preview
      raw_data(data)

      # Update sheet selection (CSV has no sheets)
      updateSelectInput(session, "sheet", choices = c("Data"), selected = "Data")

      # Update column selections
      updateSelectInput(session, "well_col", choices = column_names,
                       selected = if("Well ID" %in% column_names) "Well ID" else column_names[1])
      updateSelectInput(session, "type_col", choices = column_names,
                       selected = if("Sample Type" %in% column_names) "Sample Type" else column_names[2])
      updateSelectInput(session, "conc_col", choices = column_names,
                       selected = if("Concentration" %in% column_names) "Concentration" else column_names[3])
      updateSelectInput(session, "od_col", choices = column_names,
                       selected = if("OD" %in% column_names) "OD" else column_names[4])
      updateSelectInput(session, "od_corr_col", choices = c("None", column_names),
                       selected = if("OD (corrected)" %in% column_names) "OD (corrected)" else "None")

      # Run analysis with the example data
      withProgress(message = paste("Loading", example_name, "..."), value = 0, {
        incProgress(0.3, detail = "Reading data")

        # Parse any dilution factors
        dilution_factors <- parse_dilution_factors(input$dilution_factors_text)

        # Get models to fit
        models <- input$models_to_fit
        if (is.null(models) || length(models) == 0) {
          models <- c("4PL", "5PL", "Linear")
        }

        incProgress(0.3, detail = "Fitting models")

        analysis_results <- analyze_elisa(
          file_path = example_path,
          sheet_name = "Data",
          file_name = example_file,
          well_col = "Well ID",
          type_col = "Sample Type",
          conc_col = "Concentration",
          od_col = "OD",
          od_corr_col = "OD (corrected)",
          use_corrected = FALSE,
          assay_type = "auto",
          nsb_label = input$nsb_label %||% "NSB",
          b0_label = input$b0_label %||% "B0",
          std_label = input$std_label %||% "Standard",
          blank_label = input$blank_label %||% "Blank",
          skip_normalization = input$skip_normalization %||% FALSE,
          log_transform = input$log_transform %||% TRUE,
          remove_unlabeled = input$remove_unlabeled %||% TRUE,
          dilution_factors = dilution_factors,
          weight_type = input$weight_type %||% "none",
          models_to_fit = models,
          calculate_limits = input$calculate_limits %||% TRUE
        )

        incProgress(0.4, detail = "Complete")

        # Store results
        results(analysis_results)

        # Show success notification
        qc_status <- if (!is.null(analysis_results$qc_summary)) {
          analysis_results$qc_summary$overall_status
        } else {
          "COMPLETE"
        }

        showNotification(
          paste(example_name, "loaded successfully! QC Status:", qc_status),
          type = if(qc_status == "PASS" || qc_status == "WARNING") "message" else "warning"
        )
      })

    }, error = function(e) {
      showNotification(paste("Error loading example data:", e$message), type = "error", duration = 10)
    })
  }

  # Observer for competitive ELISA example
  observeEvent(input$loadExampleCompetitive, {
    load_example_data("competitive_elisa_melatonin.csv", "Competitive ELISA (Melatonin)")
  })

  # Observer for sandwich ELISA example
  observeEvent(input$loadExampleSandwich, {
    load_example_data("sandwich_elisa_IL6.csv", "Sandwich ELISA (IL-6)")
  })

  # Observer for debounced analysis parameters (auto re-analyze)
  observeEvent(analysis_trigger(), {
    # Only run if a file has been uploaded and sheet is selected
    req(input$file)
    req(input$sheet)

    # Parse dilution factors
    dilution_factors <- parse_dilution_factors(input$dilution_factors_text)

    # Get models to fit
    models <- input$models_to_fit
    if (is.null(models) || length(models) == 0) {
      models <- c("4PL", "Linear")
    }

    # Re-analyze with updated parameters
    tryCatch({
      analysis_results <- analyze_elisa(
        file_path = input$file$datapath,
        sheet_name = input$sheet,
        file_name = input$file$name,
        well_col = input$well_col,
        type_col = input$type_col,
        conc_col = input$conc_col,
        od_col = input$od_col,
        od_corr_col = if(input$od_corr_col == "None") input$od_col else input$od_corr_col,
        use_corrected = input$use_corrected %||% FALSE,
        assay_type = input$assay_type %||% "auto",
        nsb_label = input$nsb_label %||% "NSB",
        b0_label = input$b0_label %||% "B0",
        std_label = input$std_label %||% "Standard",
        blank_label = input$blank_label %||% "Blank",
        skip_normalization = input$skip_normalization %||% FALSE,
        log_transform = input$log_transform %||% TRUE,
        remove_unlabeled = input$remove_unlabeled %||% TRUE,
        dilution_factors = dilution_factors,
        weight_type = input$weight_type %||% "none",
        models_to_fit = models,
        calculate_limits = input$calculate_limits %||% TRUE
      )

      # Store updated results
      results(analysis_results)
    }, error = function(e) {
      # Display error to user but keep app running
      showNotification(
        paste("Error in analysis:", e$message),
        type = "error",
        duration = 10
      )
    })
  }, ignoreInit = TRUE)
  
  # Reactive for filtered results based on toggle
  filtered_results <- reactive({
    # Check if results exist, if not return NULL
    if(is.null(results())) return(NULL)

    # Return the complete results object
    return(results())
  })
  
  # Standard curve plot output
  output$standardCurvePlot <- renderPlotly({
    req(filtered_results())

    # Get the currently selected model
    selected_model <- input$plotModelChoice

    # Set default if not found
    if(is.null(selected_model)) selected_model <- "combined"

    # Generate the plot based on filtered results
    create_standard_curve_plot(
      filtered_results(),
      selected_model,
      show_reliability_colors = input$showReliabilityColors %||% TRUE
    )
  })
  
  # Table outputs
  output$standardsTable <- renderDT({
    req(filtered_results())

    # Format the standards data for display
    standards_data <- filtered_results()$standards %>%
      select(Well, Concentration, OD) %>%
      arrange(Concentration)

    # Create the table
    datatable(standards_data,
              options = list(pageLength = 10, scrollX = TRUE, dom = 'tp'),
              rownames = FALSE)
  })

  output$samplesTable <- renderDT({
    req(filtered_results())
    model <- input$modelSelect

    # Get samples data
    samples_data <- filtered_results()$samples

    # Create the table with conditional formatting
    datatable(samples_data,
              options = list(pageLength = 20, scrollX = TRUE, dom = 'tp'),
              rownames = FALSE) %>%
      formatStyle(
        "Is_Unreliable",
        target = "row",
        backgroundColor = styleEqual(
          c(TRUE, FALSE),
          c("#FFDDDD", "white")
        )
      )
  })

  output$sampleSummaryTable <- renderDT({
    req(filtered_results())

    # Get sample summary data
    sample_summary <- filtered_results()$sample_summary

    # Create the table
    datatable(sample_summary,
              options = list(pageLength = 20, scrollX = TRUE, dom = 'tp'),
              rownames = FALSE)
  })

  # ============ Quality Control Tab Outputs ============

  # Assay Limits Table (LOD/LOQ/ULOQ)
  output$assayLimitsTable <- renderTable({
    req(filtered_results())

    limits <- filtered_results()$assay_limits

    if (is.null(limits)) {
      return(data.frame(
        Metric = c("LOD", "LOQ", "ULOQ"),
        Value = c("N/A", "N/A", "N/A"),
        stringsAsFactors = FALSE
      ))
    }

    data.frame(
      Metric = c("LOD", "LOQ", "ULOQ", "Dynamic Range"),
      Value = c(
        if (!is.null(limits$lod$concentration) && !is.na(limits$lod$concentration))
          format(limits$lod$concentration, digits = 4, scientific = TRUE) else "N/A",
        if (!is.null(limits$loq$concentration) && !is.na(limits$loq$concentration))
          format(limits$loq$concentration, digits = 4, scientific = TRUE) else "N/A",
        if (!is.null(limits$uloq$concentration) && !is.na(limits$uloq$concentration))
          format(limits$uloq$concentration, digits = 4, scientific = TRUE) else "N/A",
        if (!is.null(limits$dynamic_range) && !is.na(limits$dynamic_range$fold_range))
          paste0(round(limits$dynamic_range$fold_range, 1), "-fold") else "N/A"
      ),
      stringsAsFactors = FALSE
    )
  })

  # QC Status Box
  output$qcStatusBox <- renderUI({
    req(filtered_results())

    qc <- filtered_results()$qc_summary

    if (is.null(qc)) {
      return(tags$div(
        class = "alert alert-info",
        tags$strong("QC Status: "), "Not calculated"
      ))
    }

    status_class <- switch(qc$overall_status,
      "PASS" = "alert-success",
      "WARNING" = "alert-warning",
      "FAIL" = "alert-danger",
      "alert-info"
    )

    tags$div(
      class = paste("alert", status_class),
      tags$h4(paste("Overall Status:", qc$overall_status)),
      if (!is.null(qc$issues) && length(qc$issues) > 0) {
        tags$ul(
          lapply(qc$issues, function(issue) tags$li(issue))
        )
      }
    )
  })

  # QC Summary Table
  output$qcSummaryTable <- renderTable({
    req(filtered_results())

    qc <- filtered_results()$qc_summary

    if (is.null(qc)) {
      return(data.frame(Metric = "N/A", Value = "N/A"))
    }

    data.frame(
      Metric = c("Best R²", "Standards Passing", "Analysis Time"),
      Value = c(
        if (!is.null(qc$best_r_squared)) round(qc$best_r_squared, 4) else "N/A",
        if (!is.null(qc$standards_passing)) paste0(qc$standards_passing, "/", qc$standards_total) else "N/A",
        format(qc$generated_at, "%Y-%m-%d %H:%M:%S")
      ),
      stringsAsFactors = FALSE
    )
  })

  # Model Performance Table
  output$modelPerformanceTable <- renderTable({
    req(filtered_results())

    comparison <- filtered_results()$model_comparison

    if (is.null(comparison) || nrow(comparison) == 0) {
      return(data.frame(Model = "N/A", `R²` = "N/A", AIC = "N/A"))
    }

    comparison %>%
      mutate(
        R_squared = round(R_squared, 4),
        RMSE = round(RMSE, 4),
        AIC = round(AIC, 2)
      ) %>%
      select(Model, R_squared, RMSE, AIC)
  })

  # Standards Accuracy Table
  output$standardsAccuracyTable <- renderDT({
    req(filtered_results())

    accuracy <- filtered_results()$standards_accuracy

    if (is.null(accuracy) || nrow(accuracy) == 0) {
      return(datatable(
        data.frame(Message = "Standards accuracy data not available"),
        options = list(dom = 't')
      ))
    }

    # Format for display
    display_data <- accuracy %>%
      mutate(
        Concentration = format(Concentration, digits = 4, scientific = TRUE),
        Mean_Response = round(Mean_Response, 4),
        CV_Pct = round(CV_Pct, 2),
        Mean_Back_Calc = format(Mean_Back_Calc, digits = 4, scientific = TRUE),
        Mean_Recovery_Pct = round(Mean_Recovery_Pct, 1),
        Mean_Percent_RE = round(Mean_Percent_RE, 1)
      ) %>%
      select(
        Concentration, N, Mean_Response, CV_Pct,
        Mean_Back_Calc, Mean_Recovery_Pct, Mean_Percent_RE, QC_Status
      )

    datatable(
      display_data,
      options = list(pageLength = 20, scrollX = TRUE, dom = 'tp'),
      rownames = FALSE
    ) %>%
      formatStyle(
        "QC_Status",
        backgroundColor = styleEqual(c("PASS", "FAIL"), c("#d4edda", "#f8d7da"))
      ) %>%
      formatStyle(
        "Mean_Recovery_Pct",
        backgroundColor = styleInterval(c(80, 120), c("#f8d7da", "#d4edda", "#f8d7da"))
      )
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste("ELISA_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      req(filtered_results())

      # Create a list of data frames to write to different sheets
      data_list <- list(
        "Standards" = filtered_results()$standards,
        "Samples" = filtered_results()$samples,
        "Sample Summary" = filtered_results()$sample_summary,
        "Model Comparison" = filtered_results()$model_comparison
      )

      # Write to Excel file
      writexl::write_xlsx(data_list, file)
    }
  )
  
  # Sheet names are handled in the file upload observer

  # Model comparison table
  output$modelComparison <- renderTable({
    req(filtered_results())
    filtered_results()$model_comparison
  })

  # Model equations table
  output$modelEquations <- renderTable({
    req(filtered_results())

    # Create a data frame with equations
    equations <- filtered_results()$equations

    if(is.null(equations)) {
      return(data.frame(Model = character(0), Equation = character(0)))
    }

    data.frame(
      Model = names(equations),
      Equation = unlist(equations),
      stringsAsFactors = FALSE
    )
  })

  # Download handlers for individual files
  output$downloadSampleSummary <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_SampleSummary_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(filtered_results())
      writexl::write_xlsx(filtered_results()$sample_summary, file)
    }
  )

  output$downloadSamples <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_Samples_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(filtered_results())
      writexl::write_xlsx(filtered_results()$samples, file)
    }
  )

  output$downloadStandards <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_Standards_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(filtered_results())
      writexl::write_xlsx(filtered_results()$standards, file)
    }
  )

  # Helper function to create ggplot version for export
  create_export_plot <- function(results, model_choice) {
    # Extract needed data elements
    std_summary <- results$std_summary
    samples_data <- results$samples
    log_transform <- results$log_transform
    std_range <- results$std_range
    y_axis_label <- if(results$skip_normalization) "OD (corrected)" else "Normalized OD (B/B0)"
    best_model <- results$best_model_name

    # Generate curve data
    min_conc <- min(std_summary$Concentration[std_summary$Concentration > 0], na.rm = TRUE) / 10
    max_conc <- max(std_summary$Concentration, na.rm = TRUE) * 10
    x_points <- exp(seq(log(min_conc), log(max_conc), length.out = 100))

    # Build the plot
    p <- ggplot() +
      geom_point(data = std_summary, aes(x = Concentration, y = Mean_Response),
                color = "red", size = 3, shape = 16) +
      geom_errorbar(data = std_summary,
                   aes(x = Concentration,
                       ymin = Mean_Response - SD_Response,
                       ymax = Mean_Response + SD_Response),
                   width = 0.1, color = "red", alpha = 0.5)

    # Add model curves based on choice
    if (model_choice == "combined" || model_choice == "4pl") {
      if (!is.null(results$models[["4PL"]]) && results$models[["4PL"]]$is_valid()) {
        y_4pl <- tryCatch(results$models[["4PL"]]$predict_response(x_points), error = function(e) NULL)
        if (!is.null(y_4pl)) {
          p <- p + geom_line(data = data.frame(x = x_points, y = y_4pl),
                            aes(x = x, y = y), color = "blue", size = 1)
        }
      }
    }

    if (model_choice == "combined" || model_choice == "5pl") {
      if (!is.null(results$models[["5PL"]]) && results$models[["5PL"]]$is_valid()) {
        y_5pl <- tryCatch(results$models[["5PL"]]$predict_response(x_points), error = function(e) NULL)
        if (!is.null(y_5pl)) {
          p <- p + geom_line(data = data.frame(x = x_points, y = y_5pl),
                            aes(x = x, y = y), color = "purple", size = 1, linetype = "dashed")
        }
      }
    }

    if (model_choice == "combined" || model_choice == "linear") {
      if (!is.null(results$models[["Linear"]]) && results$models[["Linear"]]$is_valid()) {
        y_lin <- tryCatch(results$models[["Linear"]]$predict_response(x_points), error = function(e) NULL)
        if (!is.null(y_lin)) {
          p <- p + geom_line(data = data.frame(x = x_points, y = y_lin),
                            aes(x = x, y = y), color = "green", size = 1)
        }
      }
    }

    # Add formatting
    p <- p +
      scale_x_log10(labels = scales::label_number(), breaks = scales::breaks_log(n = 8)) +
      labs(
        title = paste0("ELISA Standard Curve - ", toupper(model_choice)),
        subtitle = paste("Best model:", best_model),
        x = "Concentration (log scale)",
        y = y_axis_label
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 12)
      ) +
      geom_vline(xintercept = min(std_summary$Concentration, na.rm = TRUE),
                linetype = "dashed", alpha = 0.3) +
      geom_vline(xintercept = max(std_summary$Concentration, na.rm = TRUE),
                linetype = "dashed", alpha = 0.3)

    return(p)
  }

  # Handlers for plot downloads (using ggplot2 for reliable PNG export)
  output$downloadPlotsCombined <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_CombinedPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "combined")
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150, bg = "white")
    }
  )

  output$downloadPlots4PL <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_4PLPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "4pl")
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150, bg = "white")
    }
  )

  output$downloadPlots5PL <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_5PLPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "5pl")
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150, bg = "white")
    }
  )

  output$downloadPlotsLinear <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_LinearPlot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      req(filtered_results())
      p <- create_export_plot(filtered_results(), "linear")
      ggsave(file, plot = p, width = 12, height = 8, dpi = 150, bg = "white")
    }
  )

  # Combined download handler (ZIP file)
  output$downloadAllFiles <- downloadHandler(
    filename = function() {
      paste0(input$downloadPrefix, "_AllResults_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(filtered_results())

      # Create temp directory
      temp_dir <- tempdir()

      # Create files to include in the ZIP
      excel_file <- file.path(temp_dir, "ELISA_Results_AllData.xlsx")
      combined_plot <- file.path(temp_dir, "ELISA_CombinedPlot.png")
      fourpl_plot <- file.path(temp_dir, "ELISA_4PLPlot.png")
      fivepl_plot <- file.path(temp_dir, "ELISA_5PLPlot.png")
      linear_plot <- file.path(temp_dir, "ELISA_LinearPlot.png")
      qc_report <- file.path(temp_dir, "ELISA_QC_Report.xlsx")

      # Create Excel file with all data
      data_list <- list(
        "Standards" = filtered_results()$standards,
        "Samples" = filtered_results()$samples,
        "Sample Summary" = filtered_results()$sample_summary,
        "Model Comparison" = filtered_results()$model_comparison
      )

      # Add accuracy data if available
      if (!is.null(filtered_results()$standards_accuracy)) {
        data_list[["Standards Accuracy"]] <- filtered_results()$standards_accuracy
      }

      writexl::write_xlsx(data_list, excel_file)

      # Create QC report
      tryCatch({
        qc_data <- list()

        # Assay limits
        if (!is.null(filtered_results()$assay_limits)) {
          limits <- filtered_results()$assay_limits
          qc_data[["Assay Limits"]] <- data.frame(
            Metric = c("LOD", "LOQ", "ULOQ"),
            Concentration = c(
              limits$lod$concentration %||% NA,
              limits$loq$concentration %||% NA,
              limits$uloq$concentration %||% NA
            )
          )
        }

        # QC Summary
        if (!is.null(filtered_results()$qc_summary)) {
          qc <- filtered_results()$qc_summary
          qc_data[["QC Summary"]] <- data.frame(
            Metric = c("Overall Status", "Best R²", "Standards Passing"),
            Value = c(
              qc$overall_status %||% "N/A",
              as.character(qc$best_r_squared %||% "N/A"),
              paste0(qc$standards_passing %||% "N/A", "/", qc$standards_total %||% "N/A")
            )
          )
        }

        if (length(qc_data) > 0) {
          writexl::write_xlsx(qc_data, qc_report)
        }
      }, error = function(e) {
        message("Error creating QC report: ", e$message)
      })

      # Create plot files using ggplot2 for reliable export
      tryCatch({
        p_combined <- create_export_plot(filtered_results(), "combined")
        ggsave(combined_plot, plot = p_combined, width = 12, height = 8, dpi = 150, bg = "white")

        p_4pl <- create_export_plot(filtered_results(), "4pl")
        ggsave(fourpl_plot, plot = p_4pl, width = 12, height = 8, dpi = 150, bg = "white")

        p_5pl <- create_export_plot(filtered_results(), "5pl")
        ggsave(fivepl_plot, plot = p_5pl, width = 12, height = 8, dpi = 150, bg = "white")

        p_linear <- create_export_plot(filtered_results(), "linear")
        ggsave(linear_plot, plot = p_linear, width = 12, height = 8, dpi = 150, bg = "white")
      }, error = function(e) {
        message("Error generating plot: ", e$message)
      })

      # Create ZIP file with all files
      files_to_zip <- c(excel_file, combined_plot, fourpl_plot, fivepl_plot, linear_plot, qc_report)
      files_to_zip <- files_to_zip[file.exists(files_to_zip)]

      zip(file, files = files_to_zip, flags = "-j")
    },
    contentType = "application/zip"
  )
}

# Run the Shiny application
shinyApp(ui, server)