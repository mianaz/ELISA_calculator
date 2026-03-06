# ELISA Analyzer UI Definition
# Using shinythemes "yeti" Bootstrap theme

#' Create the UI for the ELISA analyzer app
#' @return Shiny UI object
create_ui <- function() {

  # JavaScript for plot invalidation and file input reset
  custom_js <- HTML("
    Shiny.addCustomMessageHandler('plot_invalidate', function(outputId) {
      Shiny.unbindOutput(outputId);
      Shiny.bindOutput(outputId);
    });
    Shiny.addCustomMessageHandler('resetFileInput', function(inputId) {
      var el = document.getElementById(inputId);
      if (el) { el.value = ''; }
      var container = $(\"#\" + inputId).closest(\".form-group\");
      if (container.length) {
        container.find(\".form-control\").val(\"\");
      }
      Shiny.setInputValue(inputId, null);
    });
  ")

  navbarPage(
    title = "ELISA Analyzer v2.0",
    id = "mainNav",
    theme = shinythemes::shinytheme("yeti"),
    header = tags$head(tags$script(custom_js)),

    # =========================================================================
    # TAB 1 — Upload & Analyze
    # =========================================================================
    tabPanel("Upload & Analyze", icon = icon("cloud-arrow-up"),

      fluidRow(
        column(12,
          actionButton("resetAnalysis", "New Analysis",
                       icon = icon("rotate-left"),
                       class = "btn btn-default btn-sm pull-right")
        )
      ),

      br(),

      # Upload section
      wellPanel(
        h4(icon("file-arrow-up"), "Upload Data"),
        fileInput("file", "Select Data File",
                  accept = c(".xlsx", ".xls", ".csv", ".tsv")),
        p(class = "text-muted small",
          icon("info-circle"),
          " Supported formats: Excel (.xlsx, .xls), CSV (.csv), TSV (.tsv)"),

        hr(),

        h5("Or try example data"),
        fluidRow(
          column(6,
            actionButton("loadExampleCompetitive",
                         div(icon("flask"), " Competitive ELISA",
                             tags$br(),
                             tags$small(class = "text-muted", "Melatonin")),
                         class = "btn btn-default btn-block")
          ),
          column(6,
            actionButton("loadExampleSandwich",
                         div(icon("vial"), " Sandwich ELISA",
                             tags$br(),
                             tags$small(class = "text-muted", "IL-6")),
                         class = "btn btn-default btn-block")
          )
        )
      ),

      # Parameters (shown after file upload)
      conditionalPanel(
        condition = "output.fileUploaded",

        uiOutput("dataSummaryInfo"),

        wellPanel(
          h4(icon("sliders"), "Analysis Parameters"),

          fluidRow(
            column(6,
              conditionalPanel(
                condition = "output.isExcelFile",
                selectInput("sheet", "Select Sheet", choices = NULL)
              )
            ),
            column(6,
              selectInput("assay_type", "Assay Type",
                          choices = c("Auto-Detect"           = "auto",
                                      "Competitive ELISA"     = "competitive",
                                      "Direct/Sandwich ELISA" = "direct"),
                          selected = "auto")
            )
          ),

          # Column mapping (collapsible)
          tags$details(
            tags$summary(tags$strong(icon("columns"), " Column Mapping & Control Labels")),
            br(),
            h5("Column Mapping"),
            fluidRow(
              column(4, selectInput("well_col", "Well ID Column", choices = NULL)),
              column(4, selectInput("type_col", "Sample Type Column", choices = NULL)),
              column(4, selectInput("conc_col", "Concentration Column", choices = NULL))
            ),
            fluidRow(
              column(4, selectInput("od_col", "OD Column", choices = NULL)),
              column(4, selectInput("od_corr_col", "Corrected OD Column (Optional)", choices = NULL)),
              column(4, checkboxInput("use_corrected", "Use Corrected OD Values", value = FALSE))
            ),

            hr(),
            h5("Control Labels"),
            fluidRow(
              column(3, textInput("nsb_label",   "NSB Label",      value = "NSB")),
              column(3, textInput("b0_label",    "B0 Label",       value = "B0")),
              column(3, textInput("std_label",   "Standard Label", value = "Standard")),
              column(3, textInput("blank_label", "Blank Label",    value = "Blank"))
            )
          ),

          br(),

          fluidRow(
            column(4, checkboxInput("skip_normalization", "Skip B0/NSB Normalization", value = FALSE)),
            column(4, checkboxInput("log_transform",      "Log-Transform Concentrations", value = TRUE)),
            column(4, checkboxInput("remove_unlabeled",   "Remove Unlabeled Samples", value = TRUE))
          ),

          # Advanced options (collapsible)
          tags$details(
            tags$summary(tags$strong(icon("gear"), " Advanced Options")),
            br(),
            fluidRow(
              column(4,
                h5("Curve Fitting"),
                selectInput("weight_type", "Regression Weighting",
                            choices = c("None"                              = "none",
                                        "1/Y (Constant CV)"                 = "1/Y",
                                        "1/Y\u00b2 (Proportional Error)"    = "1/Y^2",
                                        "1/X\u00b2 (Concentration-based)"   = "1/X^2"),
                            selected = "none"),
                checkboxGroupInput("models_to_fit", "Models to Fit",
                                   choices = c("4-Parameter Logistic (4PL)" = "4PL",
                                               "5-Parameter Logistic (5PL)" = "5PL",
                                               "Linear"                     = "Linear"),
                                   selected = c("4PL", "5PL", "Linear"))
              ),
              column(4,
                h5("Dilution Factors"),
                textAreaInput("dilution_factors_text", NULL,
                              placeholder = "Enter as: SampleType=Factor\nExample:\nSerum=10\nPlasma=5",
                              rows = 5)
              ),
              column(4,
                h5("Quality Control"),
                checkboxInput("calculate_limits", "Calculate LOD/LOQ/ULOQ", value = TRUE),
                actionButton("resetExclusions", "Reset Exclusions",
                             icon  = icon("rotate-right"),
                             class = "btn btn-default btn-sm")
              )
            )
          )
        ),

        # Analyze button
        div(class = "text-center",
          actionButton("analyze", "Analyze Data",
                       icon  = icon("play"),
                       class = "btn btn-primary btn-lg"),
          br(), br()
        ),

        # Data preview
        wellPanel(
          h4(icon("table"), "Data Preview"),
          tabsetPanel(
            id = "previewTabs",
            tabPanel("Raw Data",
              br(),
              DT::dataTableOutput("rawPreview"),
              p(class = "text-muted small", "Raw data as loaded from file.")
            ),
            tabPanel("Cleaned Data",
              br(),
              DT::dataTableOutput("cleanedPreview"),
              p(class = "text-muted small",
                "Data after applying selected cleaning options.")
            )
          )
        )
      )
    ),

    # =========================================================================
    # TAB 2 — Results
    # =========================================================================
    tabPanel("Results", icon = icon("chart-line"),

      conditionalPanel(
        condition = "output.analysisComplete",

        uiOutput("analysisMetadata"),

        # Model information
        wellPanel(
          h4(icon("square-root-variable"), "Model Information"),
          fluidRow(
            column(6,
              h5("Model Comparison"),
              tableOutput("modelComparison")
            ),
            column(6,
              h5("Equations"),
              tableOutput("modelEquations")
            )
          )
        ),

        # Standard curve plot
        wellPanel(
          h4(icon("chart-area"), "Standard Curve Plot"),
          fluidRow(
            column(8,
              radioButtons("plotModelChoice", "Model:",
                           choices = c("Combined Models" = "combined",
                                       "4PL Model"       = "4pl",
                                       "5PL Model"       = "5pl",
                                       "Linear Model"    = "linear"),
                           selected = "combined",
                           inline   = TRUE)
            ),
            column(4,
              checkboxInput("showReliabilityColors",
                            "Color code unreliable values", value = TRUE)
            )
          ),
          plotlyOutput("standardCurvePlot", height = "600px")
        ),

        # Quick download
        wellPanel(
          h4(icon("download"), "Quick Download"),
          fluidRow(
            column(3, downloadButton("downloadAllFiles2", "All Results (ZIP)",
                                     class = "btn btn-primary btn-sm btn-block")),
            column(3, downloadButton("downloadSampleSummary2", "Sample Summary",
                                     class = "btn btn-success btn-sm btn-block")),
            column(3, downloadButton("downloadSamples2", "Individual Samples",
                                     class = "btn btn-success btn-sm btn-block")),
            column(3, downloadButton("downloadStandards2", "Standards",
                                     class = "btn btn-success btn-sm btn-block"))
          )
        )
      ),

      conditionalPanel(
        condition = "!output.analysisComplete",
        div(class = "text-center", style = "padding: 4rem 1.5rem; color: #999;",
          icon("chart-line", class = "fa-3x"),
          h4("No analysis results yet"),
          p("Upload data and run analysis to see results here.")
        )
      )
    ),

    # =========================================================================
    # TAB 3 — Quality Control
    # =========================================================================
    tabPanel("Quality Control", icon = icon("clipboard-check"),

      conditionalPanel(
        condition = "output.analysisComplete",

        fluidRow(
          column(4,
            wellPanel(
              h4(icon("bullseye"), "Assay Limits"),
              h5("Dynamic Range"),
              tableOutput("assayLimitsTable"),
              hr(),
              tags$dl(class = "small",
                tags$dt("LOD"),  tags$dd("Limit of Detection"),
                tags$dt("LOQ"),  tags$dd("Limit of Quantitation"),
                tags$dt("ULOQ"), tags$dd("Upper Limit of Quantitation")
              )
            )
          ),
          column(4,
            wellPanel(
              h4(icon("shield"), "QC Summary"),
              uiOutput("qcStatusBox"),
              hr(),
              tableOutput("qcSummaryTable")
            )
          ),
          column(4,
            wellPanel(
              h4(icon("star"), "Model Performance"),
              tableOutput("modelPerformanceTable"),
              hr(),
              p(class = "text-muted small",
                "Models ranked by AIC (Akaike Information Criterion). Lower AIC indicates better fit.")
            )
          )
        ),

        # Standards back-calculation accuracy
        wellPanel(
          h4(icon("search"), "Standard Curve Back-Calculation Accuracy"),
          p(class = "text-muted small",
            "Each standard should back-calculate within 80\u2013120% of nominal (%RE within \u00b120%)."),
          DT::dataTableOutput("standardsAccuracyTable")
        )
      ),

      conditionalPanel(
        condition = "!output.analysisComplete",
        div(class = "text-center", style = "padding: 4rem 1.5rem; color: #999;",
          icon("clipboard-check", class = "fa-3x"),
          h4("No analysis results yet"),
          p("Upload data and run analysis to see quality control metrics here.")
        )
      )
    ),

    # =========================================================================
    # TAB 4 — Data Tables
    # =========================================================================
    tabPanel("Data Tables", icon = icon("table"),

      conditionalPanel(
        condition = "output.analysisComplete",

        wellPanel(
          fluidRow(
            column(6,
              selectInput("modelSelect", "Model for Concentration Calculation:",
                          choices = c("Auto Select (Best AIC)" = "auto",
                                      "4PL Model"              = "4pl",
                                      "5PL Model"              = "5pl",
                                      "Linear Model"           = "linear"),
                          selected = "auto")
            ),
            column(6,
              checkboxInput("showCIColumns", "Show Confidence Intervals", value = FALSE)
            )
          )
        ),

        tabsetPanel(
          id = "tablesTabs",
          tabPanel("Sample Summary",
            br(),
            DT::dataTableOutput("sampleSummaryTable")
          ),
          tabPanel("Individual Samples",
            br(),
            DT::dataTableOutput("samplesTable")
          ),
          tabPanel("Standards",
            br(),
            DT::dataTableOutput("standardsTable")
          )
        )
      ),

      conditionalPanel(
        condition = "!output.analysisComplete",
        div(class = "text-center", style = "padding: 4rem 1.5rem; color: #999;",
          icon("table", class = "fa-3x"),
          h4("No analysis results yet"),
          p("Upload data and run analysis to see data tables here.")
        )
      )
    ),

    # =========================================================================
    # TAB 5 — Download
    # =========================================================================
    tabPanel("Download", icon = icon("download"),

      conditionalPanel(
        condition = "output.analysisComplete",

        wellPanel(
          h4(icon("file-export"), "Export Results"),
          textInput("downloadPrefix", "File Name Prefix", value = "ELISA_Results"),
          hr(),

          fluidRow(
            column(4,
              h5(icon("image"), "Plots"),
              downloadButton("downloadPlotsCombined", "Combined Models",
                             class = "btn btn-default btn-sm btn-block"),
              downloadButton("downloadPlots4PL", "4PL Model",
                             class = "btn btn-default btn-sm btn-block"),
              downloadButton("downloadPlots5PL", "5PL Model",
                             class = "btn btn-default btn-sm btn-block"),
              downloadButton("downloadPlotsLinear", "Linear Model",
                             class = "btn btn-default btn-sm btn-block")
            ),
            column(4,
              h5(icon("file-excel"), "Data Tables"),
              downloadButton("downloadSampleSummary", "Sample Summary",
                             class = "btn btn-success btn-sm btn-block"),
              downloadButton("downloadSamples", "Individual Samples",
                             class = "btn btn-success btn-sm btn-block"),
              downloadButton("downloadStandards", "Standards",
                             class = "btn btn-success btn-sm btn-block")
            ),
            column(4,
              h5(icon("file-zipper"), "Complete Package"),
              downloadButton("downloadAllFiles", "Download All (ZIP)",
                             class = "btn btn-primary btn-lg btn-block"),
              p(class = "text-muted small",
                "Includes all plots, data tables, and QC report")
            )
          )
        )
      ),

      conditionalPanel(
        condition = "!output.analysisComplete",
        div(class = "text-center", style = "padding: 4rem 1.5rem; color: #999;",
          icon("download", class = "fa-3x"),
          h4("No analysis results yet"),
          p("Upload data and run analysis to download results.")
        )
      )
    ),

    # =========================================================================
    # TAB 6 — Help
    # =========================================================================
    tabPanel("Help", icon = icon("question-circle"),

      wellPanel(
        h3(icon("book"), "ELISA Analysis Application v2.0"),
        p(class = "lead",
          "Comprehensive ELISA data analysis following industry best practices."),

        h4("Quick Start"),
        tags$ol(
          tags$li(tags$b("Try Example Data:"),
                  " Click one of the example data buttons in the Upload tab to see the analyzer in action."),
          tags$li(tags$b("Upload Your Data:"),
                  " Or upload your own Excel/CSV file with ELISA results."),
          tags$li(tags$b("Configure:"),
                  " Verify column mappings and adjust settings (assay type, normalization, etc.)."),
          tags$li(tags$b("Analyze:"),
                  " Click the Analyze Data button to process your data."),
          tags$li(tags$b("Review:"),
                  " Check Results, Quality Control, and Data Tables tabs.")
        ),

        hr(),
        h4("Supported Assay Types"),
        tags$ul(
          tags$li(tags$b("Competitive ELISA:"),
                  " High concentration = low signal (e.g., melatonin, cortisol). Auto-detected by negative correlation."),
          tags$li(tags$b("Direct/Sandwich ELISA:"),
                  " High concentration = high signal (e.g., cytokines, hormones). Auto-detected by positive correlation.")
        ),

        hr(),
        h4("Curve Fitting Models"),
        tags$ul(
          tags$li(tags$b("4-Parameter Log-Logistic (4PL):"),
                  " Standard dose-response curve. Best for most ELISA assays."),
          tags$li(tags$b("5-Parameter Log-Logistic (5PL):"),
                  " Adds asymmetry parameter for curves with hook effects."),
          tags$li(tags$b("Linear:"),
                  " Simple fallback model for limited dynamic range."),
          tags$li(tags$b("Weighted Regression:"),
                  " Optional 1/Y, 1/Y\u00b2, 1/X\u00b2 weighting for heteroscedastic data.")
        ),

        hr(),
        h4("Quality Control Features"),
        tags$ul(
          tags$li(tags$b("LOD/LOQ/ULOQ:"),
                  " Automatic calculation of assay detection and quantitation limits."),
          tags$li(tags$b("Back-Calculation Accuracy:"),
                  " Each standard should recover 80\u2013120% of nominal concentration."),
          tags$li(tags$b("CV Monitoring:"),
                  " Replicate precision checks (< 15% CV for duplicates)."),
          tags$li(tags$b("Outlier Detection:"),
                  " Grubb\u2019s and Dixon\u2019s Q tests for identifying outliers.")
        ),

        hr(),
        h4("Expected Data Format"),
        p("Your data file should contain the following columns:"),
        tags$table(class = "table table-striped table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Column"), tags$th("Description"), tags$th("Example")
            )
          ),
          tags$tbody(
            tags$tr(tags$td("Well ID"),
                    tags$td("Well position identifier"),
                    tags$td("A1, B2, C3 ...")),
            tags$tr(tags$td("Sample Type"),
                    tags$td("Identifies standards, samples, controls"),
                    tags$td("Standard, Sample, Blank, NSB, B0")),
            tags$tr(tags$td("Concentration"),
                    tags$td("Known concentration for standards"),
                    tags$td("0, 7.8, 15.6, 31.25, ...")),
            tags$tr(tags$td("OD"),
                    tags$td("Optical density reading"),
                    tags$td("0.125, 0.456, 1.234 ...")),
            tags$tr(tags$td("OD (corrected)"),
                    tags$td("Optional: blank-corrected OD"),
                    tags$td("0.082, 0.413, 1.191 ..."))
          )
        ),

        hr(),
        h4("Troubleshooting"),
        tags$ul(
          tags$li(tags$b("Unexpected curve shape:"),
                  " Try toggling \u2018Skip B0/NSB Normalization\u2019 \u2014 sandwich ELISAs typically don\u2019t need it."),
          tags$li(tags$b("Poor model fit:"),
                  " Check that standards have concentration > 0 (zero concentration is excluded from curve fitting)."),
          tags$li(tags$b("Samples out of range:"),
                  " Values are color-coded when outside the standard curve range."),
          tags$li(tags$b("High CV warnings:"),
                  " Replicate CV > 15% indicates inconsistent measurements."),
          tags$li(tags$b("Standards failing accuracy:"),
                  " Recovery should be 80\u2013120%; check pipetting or reagents if many fail.")
        ),

        hr(),
        p(class = "text-muted small",
          "ELISA Analyzer v2.0 \u2014 Uses log-logistic 4PL/5PL models from the ",
          tags$code("drc"), " package for accurate dose-response curve fitting.")
      )
    )
  )
}
