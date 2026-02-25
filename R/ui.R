# ELISA Analyzer UI Definition
# Modern interface using bslib (Bootstrap 5)

#' Create the UI for the ELISA analyzer app
#' @return Shiny UI object
create_ui <- function() {

  # ---------------------------------------------------------------------------
  # Custom CSS
  # ---------------------------------------------------------------------------
  custom_css <- HTML("
    /* Card refinements */
    .card {
      border: none;
      box-shadow: 0 1px 3px 0 rgb(0 0 0 / 0.06), 0 1px 2px -1px rgb(0 0 0 / 0.06);
      transition: box-shadow 0.2s ease;
    }
    .card:hover {
      box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.08), 0 2px 4px -2px rgb(0 0 0 / 0.06);
    }
    .card-header {
      background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
      border-bottom: 1px solid #e2e8f0;
      font-weight: 600;
      font-size: 0.95rem;
    }

    /* Analyze button */
    .btn-analyze {
      background: linear-gradient(135deg, #2563eb, #1d4ed8);
      border: none;
      padding: 14px 40px;
      font-size: 1.1rem;
      font-weight: 600;
      letter-spacing: 0.02em;
      border-radius: 0.625rem;
      box-shadow: 0 4px 14px 0 rgb(37 99 235 / 0.35);
      transition: all 0.2s ease;
    }
    .btn-analyze:hover {
      background: linear-gradient(135deg, #1d4ed8, #1e40af);
      transform: translateY(-1px);
      box-shadow: 0 6px 20px 0 rgb(37 99 235 / 0.45);
    }

    /* Example data buttons */
    .example-btn {
      border-radius: 0.5rem;
      padding: 12px 16px;
      font-weight: 500;
      transition: all 0.2s ease;
      text-align: center;
    }
    .example-btn:hover { transform: translateY(-1px); }

    /* Section labels for form groups */
    .section-label {
      font-size: 0.7rem;
      font-weight: 700;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      color: #64748b;
      margin-bottom: 0.75rem;
      margin-top: 0.25rem;
    }

    /* Empty state placeholders */
    .empty-state {
      text-align: center;
      padding: 4rem 1.5rem;
      color: #94a3b8;
    }
    .empty-state .fa, .empty-state .fas, .empty-state .far {
      font-size: 3rem;
      margin-bottom: 1rem;
      display: block;
      opacity: 0.4;
    }
    .empty-state h4 { color: #64748b; font-weight: 600; }
    .empty-state p  { color: #94a3b8; }

    /* Accordion tweaks */
    .accordion-button:not(.collapsed) {
      background-color: #eff6ff;
      color: #1e40af;
    }

    /* Download grid */
    .download-grid .btn { width: 100%; margin-bottom: 0.5rem; }

    /* Navbar badge */
    .version-badge {
      font-size: 0.6rem;
      vertical-align: middle;
      padding: 0.2em 0.6em;
    }

    /* File input zone */
    .upload-card .form-group { margin-bottom: 0; }

    /* Tighter form spacing inside parameter cards */
    .param-card .form-group { margin-bottom: 0.5rem; }

    /* QC definition list */
    .qc-defs dt { font-weight: 600; color: #334155; }
    .qc-defs dd { color: #64748b; margin-bottom: 0.35rem; }

    /* Help accordion content */
    .help-table th { background: #f1f5f9; }
  ")

  # ---------------------------------------------------------------------------
  # JavaScript (kept from original for plot invalidation)
  # ---------------------------------------------------------------------------
  custom_js <- HTML("
    Shiny.addCustomMessageHandler('plot_invalidate', function(outputId) {
      Shiny.unbindOutput(outputId);
      Shiny.bindOutput(outputId);
    });
  ")

  # ---------------------------------------------------------------------------
  # Page
  # ---------------------------------------------------------------------------
  page_navbar(

    # --- Title / brand ------------------------------------------------------
    title = div(
      class = "d-flex align-items-center gap-2",
      icon("flask"),
      span("ELISA Analyzer", class = "fw-bold"),
      span("v2.0", class = "badge bg-light text-dark rounded-pill version-badge")
    ),

    id = "mainNav",
    fillable = FALSE,

    # --- Theme ---------------------------------------------------------------
    theme = bs_theme(
      version = 5,
      bg        = "#ffffff",
      fg        = "#1e293b",
      primary   = "#2563eb",
      secondary = "#64748b",
      success   = "#059669",
      info      = "#0891b2",
      warning   = "#d97706",
      danger    = "#dc2626",
      base_font    = font_google("Inter"),
      heading_font = font_google("Inter"),
      code_font    = font_google("JetBrains Mono"),
      "navbar-bg"          = "#1e293b",
      "card-border-radius" = "0.75rem",
      "btn-border-radius"  = "0.5rem",
      "input-border-radius" = "0.375rem"
    ),

    # --- Head elements -------------------------------------------------------
    header = tags$head(
      tags$style(custom_css),
      tags$script(custom_js)
    ),

    # =========================================================================
    # TAB 1 — Upload & Analyze
    # =========================================================================
    nav_panel(
      title = "Upload & Analyze",
      icon  = icon("cloud-arrow-up"),

      # Upload card -----------------------------------------------------------
      card(
        class = "upload-card mb-3",
        card_header(
          class = "d-flex align-items-center gap-2",
          icon("file-arrow-up", class = "text-primary"),
          "Upload Data"
        ),
        card_body(
          fileInput("file", "Select Data File",
                    accept = c(".xlsx", ".xls", ".csv", ".tsv")),
          p(class = "text-muted small mb-3",
            icon("circle-info"),
            " Supported formats: Excel (.xlsx, .xls), CSV (.csv), TSV (.tsv)"),

          hr(class = "my-3"),

          div(class = "section-label", "Or try example data"),
          layout_columns(
            col_widths = c(6, 6),
            actionButton("loadExampleCompetitive",
                         div(icon("flask"), " Competitive ELISA",
                             tags$br(),
                             tags$small(class = "text-muted", "Melatonin")),
                         class = "btn btn-outline-primary example-btn w-100"),
            actionButton("loadExampleSandwich",
                         div(icon("vial"), " Sandwich ELISA",
                             tags$br(),
                             tags$small(class = "text-muted", "IL-6")),
                         class = "btn btn-outline-info example-btn w-100")
          )
        )
      ),

      # Parameters (shown after file upload) ----------------------------------
      conditionalPanel(
        condition = "output.fileUploaded",

        card(
          class = "param-card mb-3",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("sliders", class = "text-primary"),
            "Analysis Parameters"
          ),
          card_body(
            # -- Row 1: Sheet & assay type --
            layout_columns(
              col_widths = c(6, 6),
              selectInput("sheet", "Select Sheet", choices = NULL),
              selectInput("assay_type", "Assay Type",
                          choices = c("Auto-Detect"           = "auto",
                                      "Competitive ELISA"     = "competitive",
                                      "Direct/Sandwich ELISA" = "direct"),
                          selected = "auto")
            ),

            hr(class = "my-2"),
            div(class = "section-label", "Column Mapping"),

            # -- Row 2: Column mapping --
            layout_columns(
              col_widths = c(4, 4, 4),
              selectInput("well_col", "Well ID Column", choices = NULL),
              selectInput("type_col", "Sample Type Column", choices = NULL),
              selectInput("conc_col", "Concentration Column", choices = NULL)
            ),
            layout_columns(
              col_widths = c(4, 4, 4),
              selectInput("od_col", "OD Column", choices = NULL),
              selectInput("od_corr_col", "Corrected OD Column (Optional)", choices = NULL),
              checkboxInput("use_corrected", "Use Corrected OD Values", value = FALSE)
            ),

            hr(class = "my-2"),
            div(class = "section-label", "Control Labels & Options"),

            layout_columns(
              col_widths = c(3, 3, 3, 3),
              textInput("nsb_label",   "NSB Label",      value = "NSB"),
              textInput("b0_label",    "B0 Label",       value = "B0"),
              textInput("std_label",   "Standard Label", value = "Standard"),
              textInput("blank_label", "Blank Label",    value = "Blank")
            ),

            layout_columns(
              col_widths = c(4, 4, 4),
              checkboxInput("skip_normalization", "Skip B0/NSB Normalization", value = FALSE),
              checkboxInput("log_transform",      "Log-Transform Concentrations", value = TRUE),
              checkboxInput("remove_unlabeled",   "Remove Unlabeled Samples", value = TRUE)
            ),

            # -- Advanced options (collapsed) --
            accordion(
              id = "advancedAccordion",
              open = FALSE,
              accordion_panel(
                title = "Advanced Options",
                icon  = icon("gear"),

                layout_columns(
                  col_widths = c(4, 4, 4),

                  # Curve fitting
                  div(
                    div(class = "section-label", "Curve Fitting"),
                    selectInput("weight_type", "Regression Weighting",
                                choices = c("None"                        = "none",
                                            "1/Y (Constant CV)"           = "1/Y",
                                            "1/Y\u00b2 (Proportional Error)" = "1/Y^2",
                                            "1/X\u00b2 (Concentration-based)" = "1/X^2"),
                                selected = "none"),
                    checkboxGroupInput("models_to_fit", "Models to Fit",
                                       choices = c("4-Parameter Logistic (4PL)" = "4PL",
                                                   "5-Parameter Logistic (5PL)" = "5PL",
                                                   "Linear"                     = "Linear"),
                                       selected = c("4PL", "5PL", "Linear"))
                  ),

                  # Dilution factors
                  div(
                    div(class = "section-label", "Dilution Factors"),
                    textAreaInput("dilution_factors_text", NULL,
                                  placeholder = "Enter as: SampleType=Factor\nExample:\nSerum=10\nPlasma=5",
                                  rows = 5)
                  ),

                  # Quality control
                  div(
                    div(class = "section-label", "Quality Control"),
                    checkboxInput("calculate_limits", "Calculate LOD/LOQ/ULOQ", value = TRUE),
                    actionButton("resetExclusions", "Reset Exclusions",
                                 icon  = icon("rotate-right"),
                                 class = "btn btn-outline-secondary btn-sm mt-2")
                  )
                )
              )
            )
          )
        ),

        # Analyze button ------------------------------------------------------
        div(
          class = "text-center my-4",
          actionButton("analyze", "Analyze Data",
                       icon  = icon("play"),
                       class = "btn-analyze btn btn-primary btn-lg")
        ),

        # Data preview ---------------------------------------------------------
        navset_card_tab(
          id    = "previewTabs",
          title = div(icon("table-list", class = "text-info me-1"), "Data Preview"),

          nav_panel(
            "Raw Data",
            DT::dataTableOutput("rawPreview"),
            p(class = "text-muted small mt-2", "Raw data as loaded from file.")
          ),
          nav_panel(
            "Cleaned Data",
            DT::dataTableOutput("cleanedPreview"),
            p(class = "text-muted small mt-2",
              "Data after applying selected cleaning options.")
          )
        )
      )
    ),

    # =========================================================================
    # TAB 2 — Results
    # =========================================================================
    nav_panel(
      title = "Results",
      icon  = icon("chart-line"),

      conditionalPanel(
        condition = "output.analysisComplete",

        # Model information ---------------------------------------------------
        card(
          class = "mb-3",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("square-root-variable", class = "text-primary"),
            "Model Information"
          ),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              div(
                h6(class = "fw-semibold text-muted", "Model Comparison"),
                tableOutput("modelComparison")
              ),
              div(
                h6(class = "fw-semibold text-muted", "Equations"),
                tableOutput("modelEquations")
              )
            )
          )
        ),

        # Standard curve plot -------------------------------------------------
        card(
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("chart-area", class = "text-primary"),
            "Standard Curve Plot"
          ),
          card_body(
            layout_columns(
              col_widths = c(8, 4),
              radioButtons("plotModelChoice", "Model:",
                           choices = c("Combined Models" = "combined",
                                       "4PL Model"       = "4pl",
                                       "5PL Model"       = "5pl",
                                       "Linear Model"    = "linear"),
                           selected = "combined",
                           inline   = TRUE),
              checkboxInput("showReliabilityColors",
                            "Color code unreliable values", value = TRUE)
            ),
            plotlyOutput("standardCurvePlot", height = "600px")
          )
        )
      ),

      conditionalPanel(
        condition = "!output.analysisComplete",
        div(
          class = "empty-state",
          icon("chart-line"),
          h4("No analysis results yet"),
          p("Upload data and run analysis to see results here.")
        )
      )
    ),

    # =========================================================================
    # TAB 3 — Quality Control
    # =========================================================================
    nav_panel(
      title = "Quality Control",
      icon  = icon("clipboard-check"),

      conditionalPanel(
        condition = "output.analysisComplete",

        layout_columns(
          col_widths = c(4, 4, 4),

          # Assay limits
          card(
            card_header(
              class = "d-flex align-items-center gap-2",
              icon("bullseye", class = "text-primary"),
              "Assay Limits"
            ),
            card_body(
              h6(class = "fw-semibold text-muted", "Dynamic Range"),
              tableOutput("assayLimitsTable"),
              hr(),
              tags$dl(class = "small qc-defs mb-0",
                tags$dt("LOD"),  tags$dd("Limit of Detection"),
                tags$dt("LOQ"),  tags$dd("Limit of Quantitation"),
                tags$dt("ULOQ"), tags$dd("Upper Limit of Quantitation")
              )
            )
          ),

          # QC summary
          card(
            card_header(
              class = "d-flex align-items-center gap-2",
              icon("shield-halved", class = "text-info"),
              "QC Summary"
            ),
            card_body(
              uiOutput("qcStatusBox"),
              hr(),
              tableOutput("qcSummaryTable")
            )
          ),

          # Model performance
          card(
            card_header(
              class = "d-flex align-items-center gap-2",
              icon("ranking-star", class = "text-success"),
              "Model Performance"
            ),
            card_body(
              tableOutput("modelPerformanceTable"),
              hr(),
              p(class = "text-muted small mb-0",
                "Models ranked by AIC (Akaike Information Criterion). Lower AIC indicates better fit.")
            )
          )
        ),

        # Standards back-calculation accuracy
        card(
          class = "mt-3",
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("magnifying-glass-chart", class = "text-warning"),
            "Standard Curve Back-Calculation Accuracy"
          ),
          card_body(
            p(class = "text-muted small",
              "Each standard should back-calculate within 80\u2013120% of nominal (%RE within \u00b120%)."),
            DT::dataTableOutput("standardsAccuracyTable")
          )
        )
      ),

      conditionalPanel(
        condition = "!output.analysisComplete",
        div(
          class = "empty-state",
          icon("clipboard-check"),
          h4("No analysis results yet"),
          p("Upload data and run analysis to see quality control metrics here.")
        )
      )
    ),

    # =========================================================================
    # TAB 4 — Data Tables
    # =========================================================================
    nav_panel(
      title = "Data Tables",
      icon  = icon("table-cells"),

      conditionalPanel(
        condition = "output.analysisComplete",

        # Controls bar
        card(
          class = "mb-3",
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              selectInput("modelSelect", "Model for Concentration Calculation:",
                          choices = c("Auto Select (Best AIC)" = "auto",
                                      "4PL Model"              = "4pl",
                                      "5PL Model"              = "5pl",
                                      "Linear Model"           = "linear"),
                          selected = "auto"),
              checkboxInput("showCIColumns", "Show Confidence Intervals", value = FALSE)
            )
          )
        ),

        # Data tables
        navset_card_tab(
          id    = "tablesTabs",
          title = div(icon("table", class = "text-primary me-1"), "Data"),

          nav_panel("Sample Summary",
                    DT::dataTableOutput("sampleSummaryTable")),

          nav_panel("Individual Samples",
                    DT::dataTableOutput("samplesTable")),

          nav_panel("Standards",
                    DT::dataTableOutput("standardsTable"))
        )
      ),

      conditionalPanel(
        condition = "!output.analysisComplete",
        div(
          class = "empty-state",
          icon("table-cells"),
          h4("No analysis results yet"),
          p("Upload data and run analysis to see data tables here.")
        )
      )
    ),

    # =========================================================================
    # TAB 5 — Download
    # =========================================================================
    nav_panel(
      title = "Download",
      icon  = icon("download"),

      conditionalPanel(
        condition = "output.analysisComplete",

        card(
          card_header(
            class = "d-flex align-items-center gap-2",
            icon("file-export", class = "text-primary"),
            "Export Results"
          ),
          card_body(
            textInput("downloadPrefix", "File Name Prefix", value = "ELISA_Results"),
            hr(),

            layout_columns(
              col_widths = c(4, 4, 4),

              # Plots
              div(
                h6(class = "fw-semibold mb-3", icon("image", class = "text-primary me-1"), "Plots"),
                div(class = "d-grid gap-2 download-grid",
                  downloadButton("downloadPlotsCombined", "Combined Models",
                                 class = "btn-outline-primary btn-sm"),
                  downloadButton("downloadPlots4PL", "4PL Model",
                                 class = "btn-outline-primary btn-sm"),
                  downloadButton("downloadPlots5PL", "5PL Model",
                                 class = "btn-outline-primary btn-sm"),
                  downloadButton("downloadPlotsLinear", "Linear Model",
                                 class = "btn-outline-primary btn-sm")
                )
              ),

              # Data tables
              div(
                h6(class = "fw-semibold mb-3", icon("file-excel", class = "text-success me-1"), "Data Tables"),
                div(class = "d-grid gap-2 download-grid",
                  downloadButton("downloadSampleSummary", "Sample Summary",
                                 class = "btn-outline-success btn-sm"),
                  downloadButton("downloadSamples", "Individual Samples",
                                 class = "btn-outline-success btn-sm"),
                  downloadButton("downloadStandards", "Standards",
                                 class = "btn-outline-success btn-sm")
                )
              ),

              # Complete package
              div(
                h6(class = "fw-semibold mb-3", icon("file-zipper", class = "text-info me-1"), "Complete Package"),
                div(class = "d-grid gap-2",
                  downloadButton("downloadAllFiles", "Download All (ZIP)",
                                 class = "btn-primary btn-lg"),
                  p(class = "text-muted small mt-2 mb-0",
                    "Includes all plots, data tables, and QC report")
                )
              )
            )
          )
        )
      ),

      conditionalPanel(
        condition = "!output.analysisComplete",
        div(
          class = "empty-state",
          icon("download"),
          h4("No analysis results yet"),
          p("Upload data and run analysis to download results.")
        )
      )
    ),

    # =========================================================================
    # TAB 6 — Help
    # =========================================================================
    nav_panel(
      title = "Help",
      icon  = icon("circle-question"),

      card(
        card_header(
          class = "d-flex align-items-center gap-2",
          icon("book-open", class = "text-primary"),
          "ELISA Analysis Application v2.0"
        ),
        card_body(
          p(class = "lead",
            "Comprehensive ELISA data analysis following industry best practices."),

          accordion(
            id   = "helpAccordion",
            open = "Quick Start",

            # Quick Start
            accordion_panel(
              title = "Quick Start",
              icon  = icon("rocket"),
              tags$ol(class = "mb-0",
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
              )
            ),

            # Supported assay types
            accordion_panel(
              title = "Supported Assay Types",
              icon  = icon("vials"),
              tags$ul(class = "mb-0",
                tags$li(tags$b("Competitive ELISA:"),
                        " High concentration = low signal (e.g., melatonin, cortisol). Auto-detected by negative correlation."),
                tags$li(tags$b("Direct/Sandwich ELISA:"),
                        " High concentration = high signal (e.g., cytokines, hormones). Auto-detected by positive correlation.")
              )
            ),

            # Curve fitting models
            accordion_panel(
              title = "Curve Fitting Models",
              icon  = icon("chart-line"),
              tags$ul(class = "mb-0",
                tags$li(tags$b("4-Parameter Log-Logistic (4PL):"),
                        " Standard dose-response curve. Best for most ELISA assays."),
                tags$li(tags$b("5-Parameter Log-Logistic (5PL):"),
                        " Adds asymmetry parameter for curves with hook effects."),
                tags$li(tags$b("Linear:"),
                        " Simple fallback model for limited dynamic range."),
                tags$li(tags$b("Weighted Regression:"),
                        " Optional 1/Y, 1/Y\u00b2, 1/X\u00b2 weighting for heteroscedastic data.")
              )
            ),

            # Quality control features
            accordion_panel(
              title = "Quality Control Features",
              icon  = icon("shield-halved"),
              tags$ul(class = "mb-0",
                tags$li(tags$b("LOD/LOQ/ULOQ:"),
                        " Automatic calculation of assay detection and quantitation limits."),
                tags$li(tags$b("Back-Calculation Accuracy:"),
                        " Each standard should recover 80\u2013120% of nominal concentration."),
                tags$li(tags$b("CV Monitoring:"),
                        " Replicate precision checks (< 15% CV for duplicates)."),
                tags$li(tags$b("Outlier Detection:"),
                        " Grubb\u2019s and Dixon\u2019s Q tests for identifying outliers.")
              )
            ),

            # Expected data format
            accordion_panel(
              title = "Expected Data Format",
              icon  = icon("file-lines"),
              p("Your data file should contain the following columns:"),
              tags$table(class = "table table-sm table-bordered help-table mb-0",
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
              )
            ),

            # Troubleshooting
            accordion_panel(
              title = "Troubleshooting",
              icon  = icon("wrench"),
              tags$ul(class = "mb-0",
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
              )
            )
          ),

          hr(),
          p(class = "text-muted small mb-0",
            "ELISA Analyzer v2.0 \u2014 Uses log-logistic 4PL/5PL models from the ",
            tags$code("drc"), " package for accurate dose-response curve fitting.")
        )
      )
    )
  )
}
