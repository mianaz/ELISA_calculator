# ELISA Analyzer UI Definition
# This file defines the user interface for the ELISA analyzer app

#' Create the UI for the ELISA analyzer app
#' @return Shiny UI object
create_ui <- function() {
  dashboardPage(
    dashboardHeader(title = "ELISA Analyzer v2.0"),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Upload & Analyze", tabName = "upload", icon = icon("upload")),
        menuItem("Results", tabName = "results", icon = icon("chart-bar")),
        menuItem("Quality Control", tabName = "qc", icon = icon("check-circle")),
        menuItem("Data Tables", tabName = "tables", icon = icon("table")),
        menuItem("Download", tabName = "download", icon = icon("download")),
        menuItem("Help", tabName = "help", icon = icon("question-circle"))
      )
    ),
    
    dashboardBody(
      # Add custom JavaScript to handle plot invalidation
      tags$head(
        tags$script(HTML("
          Shiny.addCustomMessageHandler('plot_invalidate', function(outputId) {
            Shiny.unbindOutput(outputId);
            Shiny.bindOutput(outputId);
          });
        "))
      ),
      tabItems(
        # Upload & Analyze tab
        tabItem(tabName = "upload",
                fluidRow(
                  box(
                    title = "Upload Data",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    
                    fileInput("file", "Select Data File",
                              accept = c(".xlsx", ".xls", ".csv", ".tsv")),
                    p(class = "text-muted", "Supported formats: Excel (.xlsx, .xls) or CSV (.csv, .tsv)"),

                    # Example data buttons
                    hr(),
                    h5("Try Example Data"),
                    fluidRow(
                      column(6,
                             actionButton("loadExampleCompetitive", "Load Competitive ELISA (Melatonin)",
                                         icon = icon("flask"),
                                         style = "width: 100%; margin-bottom: 10px;")
                      ),
                      column(6,
                             actionButton("loadExampleSandwich", "Load Sandwich ELISA (IL-6)",
                                         icon = icon("vial"),
                                         style = "width: 100%; margin-bottom: 10px;")
                      )
                    ),
                    p(class = "text-muted", "Click to load example data and see how the analyzer works."),
                    hr(),
                    
                    # Parameters section
                    conditionalPanel(
                      condition = "output.fileUploaded",
                      h4("Parameters"),

                      selectInput("sheet", "Select Sheet", choices = NULL),

                      selectInput("assay_type", "Assay Type",
                                 choices = c("Auto-Detect" = "auto",
                                           "Competitive ELISA" = "competitive",
                                           "Direct/Sandwich ELISA" = "direct"),
                                 selected = "auto"),
                      bsTooltip("assay_type", "Select assay type or use auto-detection based on standard curve pattern", placement = "right", trigger = "hover"),

                      fluidRow(
                        column(4, 
                               selectInput("well_col", "Well ID Column", choices = NULL),
                               selectInput("type_col", "Sample Type Column", choices = NULL)
                        ),
                        column(4, 
                               selectInput("conc_col", "Concentration Column", choices = NULL),
                               selectInput("od_col", "OD Column", choices = NULL)
                        ),
                        column(4, 
                               selectInput("od_corr_col", "Corrected OD Column (Optional)", choices = NULL),
                               checkboxInput("use_corrected", "Use Corrected OD Values", value = FALSE)
                        )
                      ),
                      
                      fluidRow(
                        column(3,
                               textInput("nsb_label", "NSB Label", value = "NSB"),
                               textInput("b0_label", "B0 Label", value = "B0")
                        ),
                        column(3,
                               textInput("std_label", "Standard Label", value = "Standard"),
                               textInput("blank_label", "Blank Label", value = "Blank")
                        ),
                        column(3,
                               checkboxInput("skip_normalization", "Skip B0/NSB Normalization", value = FALSE),
                               checkboxInput("log_transform", "Log-Transform Concentrations", value = TRUE)
                        ),
                        column(3,
                               h5("Data Cleaning"),
                               checkboxInput("remove_unlabeled", "Remove Unlabeled Samples", value = TRUE),
                               bsTooltip("remove_unlabeled", "Remove samples with blank or empty labels", placement = "right", trigger = "hover")
                        )
                      ),

                      hr(),
                      h4("Advanced Options"),

                      fluidRow(
                        column(4,
                               h5("Curve Fitting"),
                               selectInput("weight_type", "Regression Weighting",
                                          choices = c("None" = "none",
                                                    "1/Y (Constant CV)" = "1/Y",
                                                    "1/Y² (Proportional Error)" = "1/Y^2",
                                                    "1/X² (Concentration-based)" = "1/X^2"),
                                          selected = "none"),
                               bsTooltip("weight_type", "Apply weighting for heteroscedastic data. Use 1/Y for constant CV, 1/Y² for proportional error.", placement = "right", trigger = "hover"),

                               checkboxGroupInput("models_to_fit", "Models to Fit",
                                                 choices = c("4-Parameter Logistic (4PL)" = "4PL",
                                                           "5-Parameter Logistic (5PL)" = "5PL",
                                                           "Linear" = "Linear"),
                                                 selected = c("4PL", "5PL", "Linear")),
                               bsTooltip("models_to_fit", "5PL adds asymmetry parameter for non-symmetric dose-response curves", placement = "right", trigger = "hover")
                        ),
                        column(4,
                               h5("Dilution Factors"),
                               textAreaInput("dilution_factors_text", "Sample Dilution Factors",
                                           placeholder = "Enter as: SampleType=Factor\nExample:\nSerum=10\nPlasma=5",
                                           rows = 4),
                               bsTooltip("dilution_factors_text", "Enter dilution factors for sample types (one per line)", placement = "right", trigger = "hover")
                        ),
                        column(4,
                               h5("Quality Control"),
                               checkboxInput("calculate_limits", "Calculate LOD/LOQ/ULOQ", value = TRUE),
                               bsTooltip("calculate_limits", "Calculate Limit of Detection, Limit of Quantitation, and Upper Limit of Quantitation", placement = "right", trigger = "hover"),
                               actionButton("resetExclusions", "Reset All Exclusions", icon = icon("refresh")),
                               bsTooltip("resetExclusions", "Clear all manually excluded samples", placement = "right", trigger = "hover")
                        )
                      )
                    )
                  )
                ),
                
                fluidRow(
                  conditionalPanel(
                    condition = "output.fileUploaded",
                    box(
                      width = 12,
                      actionButton("analyze", "Analyze Data", 
                                   icon = icon("play"), 
                                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      bsTooltip("analyze", "Run analysis with selected parameters", placement = "right", trigger = "hover")
                    )
                  )
                ),
                
                # Preview section
                conditionalPanel(
                  condition = "output.fileUploaded",
                  fluidRow(
                    box(
                      title = "Data Preview",
                      status = "info",
                      solidHeader = TRUE,
                      width = 12,
                      
                      tabsetPanel(
                        id = "previewTabs",
                        tabPanel("Raw Data", 
                                 DT::dataTableOutput("rawPreview"),
                                 br(),
                                 p("Raw data as loaded from the Excel file.")
                        ),
                        tabPanel("Cleaned Data", 
                                 DT::dataTableOutput("cleanedPreview"),
                                 br(),
                                 p("Data after removing unlabeled samples and with invalid values flagged."),
                                 p("This view shows what will be used for analysis after applying the selected data cleaning options.")
                        )
                      )
                    )
                  )
                )
        ),
        
        # Results tab
        tabItem(tabName = "results",
                conditionalPanel(
                  condition = "output.analysisComplete",
                  fluidRow(
                    box(
                      title = "Model Information",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      
                      fluidRow(
                        column(6,
                               h4("Model Comparison"),
                               tableOutput("modelComparison")
                        ),
                        column(6,
                               h4("Equations"),
                               tableOutput("modelEquations")
                        )
                      )
                    )
                  ),
                  
                  fluidRow(
                    tabBox(
                      title = "Plots",
                      id = "plotTabs",
                      width = 12,
                      
                      # Consolidated single plot tab with color-coding
                      tabPanel("Standard Curve Plot",
                               fluidRow(
                                 column(12,
                                        # Add model selection radio buttons
                                        radioButtons("plotModelChoice", "Model:",
                                                    choices = c("Combined Models" = "combined",
                                                               "4PL Model" = "4pl",
                                                               "5PL Model" = "5pl",
                                                               "Linear Model" = "linear"),
                                                    selected = "combined",
                                                    inline = TRUE),
                                        checkboxInput("showReliabilityColors", "Color code unreliable values", value = TRUE)
                                 )
                               ),
                               fluidRow(
                                 column(12, plotlyOutput("standardCurvePlot", height = "600px"))
                               )
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "!output.analysisComplete",
                  fluidRow(
                    box(
                      width = 12,
                      h3("No analysis results yet."),
                      p("Upload data and run analysis to see results here.")
                    )
                  )
                )
        ),

        # Quality Control tab (NEW)
        tabItem(tabName = "qc",
                conditionalPanel(
                  condition = "output.analysisComplete",
                  fluidRow(
                    box(
                      title = "Assay Limits",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 4,
                      h4("Dynamic Range"),
                      tableOutput("assayLimitsTable"),
                      hr(),
                      p(tags$b("LOD:"), "Limit of Detection - lowest detectable concentration"),
                      p(tags$b("LOQ:"), "Limit of Quantitation - lowest quantifiable concentration"),
                      p(tags$b("ULOQ:"), "Upper Limit of Quantitation - highest quantifiable concentration")
                    ),
                    box(
                      title = "QC Summary",
                      status = "info",
                      solidHeader = TRUE,
                      width = 4,
                      uiOutput("qcStatusBox"),
                      hr(),
                      tableOutput("qcSummaryTable")
                    ),
                    box(
                      title = "Model Performance",
                      status = "success",
                      solidHeader = TRUE,
                      width = 4,
                      tableOutput("modelPerformanceTable"),
                      hr(),
                      p("Models are compared using AIC (Akaike Information Criterion). Lower AIC indicates better fit.")
                    )
                  ),
                  fluidRow(
                    box(
                      title = "Standard Curve Back-Calculation Accuracy",
                      status = "warning",
                      solidHeader = TRUE,
                      width = 12,
                      p("Each standard concentration should back-calculate within 80-120% of nominal (or %RE within +/-20%)"),
                      DT::dataTableOutput("standardsAccuracyTable")
                    )
                  )
                ),
                conditionalPanel(
                  condition = "!output.analysisComplete",
                  fluidRow(
                    box(
                      width = 12,
                      h3("No analysis results yet."),
                      p("Upload data and run analysis to see quality control metrics here.")
                    )
                  )
                )
        ),

        # Data Tables tab
        tabItem(tabName = "tables",
                conditionalPanel(
                  condition = "output.analysisComplete",
                  fluidRow(
                    column(6,
                      selectInput("modelSelect", "Model for Concentration Calculation:",
                                choices = c("Auto Select (Best AIC)" = "auto",
                                            "4PL Model" = "4pl",
                                            "5PL Model" = "5pl",
                                            "Linear Model" = "linear"),
                                selected = "auto")
                    ),
                    column(6,
                      checkboxInput("showCIColumns", "Show Confidence Intervals", value = FALSE),
                      bsTooltip("showCIColumns", "Display confidence interval columns in sample tables", placement = "right", trigger = "hover")
                    )
                  ),
                  fluidRow(
                    tabBox(
                      title = "Data Tables",
                      id = "tablesTabs",
                      width = 12,
                      
                      tabPanel("Sample Summary",
                               DT::dataTableOutput("sampleSummaryTable")
                      ),
                      
                      tabPanel("Individual Samples",
                               DT::dataTableOutput("samplesTable")
                      ),
                      
                      tabPanel("Standards",
                               DT::dataTableOutput("standardsTable")
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "!output.analysisComplete",
                  fluidRow(
                    box(
                      width = 12,
                      h3("No analysis results yet."),
                      p("Upload data and run analysis to see data tables here.")
                    )
                  )
                )
        ),
        
        # Download tab
        tabItem(tabName = "download",
                conditionalPanel(
                  condition = "output.analysisComplete",
                  fluidRow(
                    box(
                      title = "Download Options",
                      status = "primary",
                      solidHeader = TRUE,
                      width = 12,
                      
                      textInput("downloadPrefix", "File Name Prefix", value = "ELISA_Results"),
                      
                      h4("Download Files"),

                      fluidRow(
                        column(4,
                               h5("Plots"),
                               downloadButton("downloadPlotsCombined", "Combined Models", class = "btn-sm"),
                               br(), br(),
                               downloadButton("downloadPlots4PL", "4PL Model", class = "btn-sm"),
                               br(), br(),
                               downloadButton("downloadPlots5PL", "5PL Model", class = "btn-sm"),
                               br(), br(),
                               downloadButton("downloadPlotsLinear", "Linear Model", class = "btn-sm")
                        ),
                        column(4,
                               h5("Data Tables"),
                               downloadButton("downloadSampleSummary", "Sample Summary", class = "btn-sm"),
                               br(), br(),
                               downloadButton("downloadSamples", "Individual Samples", class = "btn-sm"),
                               br(), br(),
                               downloadButton("downloadStandards", "Standards", class = "btn-sm")
                        ),
                        column(4,
                               h5("Complete Package"),
                               downloadButton("downloadAllFiles", "Download All (ZIP)", class = "btn-primary btn-lg"),
                               br(), br(),
                               p("Includes all plots, data tables, and QC report")
                        )
                      )
                    )
                  )
                ),
                
                conditionalPanel(
                  condition = "!output.analysisComplete",
                  fluidRow(
                    box(
                      width = 12,
                      h3("No analysis results yet."),
                      p("Upload data and run analysis to download results.")
                    )
                  )
                )
        ),
        
        # Help tab
        tabItem(tabName = "help",
                fluidRow(
                  box(
                    title = "About This App",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,

                    h4("ELISA Analysis Application v2.0"),

                    p("This application provides comprehensive ELISA data analysis following industry best practices."),

                    h5("Quick Start"),
                    tags$ol(
                      tags$li(tags$b("Try Example Data:"), " Click one of the example data buttons in the Upload section to see the analyzer in action"),
                      tags$li(tags$b("Upload Your Data:"), " Or upload your own Excel/CSV file with ELISA results"),
                      tags$li(tags$b("Review Results:"), " Check the Results and Quality Control tabs for your standard curve and QC metrics")
                    ),

                    hr(),

                    h5("Supported Assay Types"),
                    tags$ul(
                      tags$li(tags$b("Competitive ELISA:"), " High concentration = low signal (e.g., melatonin, cortisol). Auto-detected by negative correlation."),
                      tags$li(tags$b("Direct/Sandwich ELISA:"), " High concentration = high signal (e.g., cytokines, hormones). Auto-detected by positive correlation.")
                    ),

                    h5("Curve Fitting Models"),
                    tags$ul(
                      tags$li(tags$b("4-Parameter Log-Logistic (4PL):"), " Standard dose-response curve. Best for most ELISA assays."),
                      tags$li(tags$b("5-Parameter Log-Logistic (5PL):"), " Adds asymmetry parameter for curves with hook effects."),
                      tags$li(tags$b("Linear:"), " Simple fallback model for limited dynamic range."),
                      tags$li(tags$b("Weighted Regression:"), " Optional 1/Y, 1/Y², 1/X² weighting for heteroscedastic data.")
                    ),

                    h5("Quality Control Features"),
                    tags$ul(
                      tags$li(tags$b("LOD/LOQ/ULOQ:"), " Automatic calculation of assay detection and quantitation limits"),
                      tags$li(tags$b("Back-Calculation Accuracy:"), " Each standard should recover 80-120% of nominal concentration"),
                      tags$li(tags$b("CV Monitoring:"), " Replicate precision checks (< 15% CV for duplicates)"),
                      tags$li(tags$b("Outlier Detection:"), " Grubb's and Dixon's Q tests for identifying outliers")
                    ),

                    hr(),

                    h4("How to Use"),

                    tags$ol(
                      tags$li(tags$b("Upload data:"), " Use the file upload or click an example data button"),
                      tags$li(tags$b("Configure parameters:"), " Verify column mappings and adjust settings as needed"),
                      tags$li(tags$b("Click 'Analyze Data':"), " Process your data (or it auto-runs for example data)"),
                      tags$li(tags$b("Review Results tab:"), " Check the standard curve fit and model comparison"),
                      tags$li(tags$b("Check Quality Control tab:"), " Verify LOD/LOQ, standards accuracy, and overall QC status"),
                      tags$li(tags$b("View Data Tables:"), " See individual sample predictions and reliability flags"),
                      tags$li(tags$b("Download results:"), " Export plots, data tables, or complete analysis package")
                    ),

                    h4("Expected Data Format"),

                    p("Your data file should contain columns for:"),

                    tags$table(class = "table table-bordered",
                      tags$thead(
                        tags$tr(
                          tags$th("Column"),
                          tags$th("Description"),
                          tags$th("Example")
                        )
                      ),
                      tags$tbody(
                        tags$tr(tags$td("Well ID"), tags$td("Well position identifier"), tags$td("A1, B2, C3...")),
                        tags$tr(tags$td("Sample Type"), tags$td("Identifies standards, samples, controls"), tags$td("Standard, Sample, Blank, NSB, B0")),
                        tags$tr(tags$td("Concentration"), tags$td("Known concentration for standards"), tags$td("0, 7.8, 15.6, 31.25, ...")),
                        tags$tr(tags$td("OD"), tags$td("Optical density reading"), tags$td("0.125, 0.456, 1.234...")),
                        tags$tr(tags$td("OD (corrected)"), tags$td("Optional: blank-corrected OD"), tags$td("0.082, 0.413, 1.191..."))
                      )
                    ),

                    h4("Troubleshooting"),

                    tags$ul(
                      tags$li(tags$b("Unexpected curve shape:"), " Try toggling 'Skip B0/NSB Normalization' - sandwich ELISAs typically don't need it"),
                      tags$li(tags$b("Poor model fit:"), " Check that standards have concentration > 0 (zero concentration is excluded from curve fitting)"),
                      tags$li(tags$b("Samples out of range:"), " Values are color-coded red when outside standard curve range"),
                      tags$li(tags$b("High CV warnings:"), " Replicate CV > 15% indicates inconsistent measurements"),
                      tags$li(tags$b("Standards failing accuracy:"), " Recovery should be 80-120%; check pipetting or reagents if many fail")
                    ),

                    hr(),
                    p(class = "text-muted", "ELISA Analyzer v2.0 - Uses log-logistic 4PL/5PL models from the drc package for accurate dose-response curve fitting.")
                  )
                )
        )
      )
    )
  )
}