# ELISA Analyzer Constants
# This file defines constants used throughout the application

# Assay types
ASSAY_TYPE_COMPETITIVE <- "competitive"
ASSAY_TYPE_DIRECT <- "direct"
ASSAY_TYPE_SANDWICH <- "sandwich"
ASSAY_TYPE_AUTO <- "auto"

ASSAY_TYPES <- c(
  ASSAY_TYPE_COMPETITIVE,
  ASSAY_TYPE_DIRECT,
  ASSAY_TYPE_SANDWICH,
  ASSAY_TYPE_AUTO
)

# Model types
MODEL_4PL <- "4PL"
MODEL_5PL <- "5PL"
MODEL_LINEAR <- "Linear"
MODEL_AUTO <- "auto"

MODEL_TYPES <- c(
  MODEL_4PL,
  MODEL_5PL,
  MODEL_LINEAR
)

# Weighting options for regression
WEIGHT_NONE <- "none"
WEIGHT_1_Y <- "1/Y"
WEIGHT_1_Y2 <- "1/Y^2"
WEIGHT_1_X2 <- "1/X^2"

WEIGHT_OPTIONS <- c(
  WEIGHT_NONE,
  WEIGHT_1_Y,
  WEIGHT_1_Y2,
  WEIGHT_1_X2
)

# Sample types
SAMPLE_TYPE_NSB <- "NSB"
SAMPLE_TYPE_B0 <- "B0"
SAMPLE_TYPE_STANDARD <- "Standard"
SAMPLE_TYPE_BLANK <- "Blank"
SAMPLE_TYPE_SAMPLE <- "Sample"

SAMPLE_TYPES_CONTROL <- c(
  SAMPLE_TYPE_NSB,
  SAMPLE_TYPE_B0,
  SAMPLE_TYPE_STANDARD,
  SAMPLE_TYPE_BLANK
)

# Column names (standardized internal names)
COL_WELL <- "Well"
COL_TYPE <- "Type"
COL_CONCENTRATION <- "Concentration"
COL_OD <- "OD"
COL_OD_CORRECTED <- "OD_corrected"
COL_OD_COR <- "OD_cor"  # Blank-corrected OD
COL_RESPONSE <- "Response"
COL_LOG_CONC <- "log_Concentration"

# Prediction columns
COL_PRED_CONC <- "Predicted_Concentration"
COL_CAPPED_CONC <- "Capped_Concentration"
COL_MODEL_USED <- "Model_Used"
COL_ERROR_FLAG <- "Error_Flag"
COL_OUT_OF_RANGE <- "Out_of_Range"
COL_IS_CAPPED <- "Is_Capped"
COL_IS_UNRELIABLE <- "Is_Unreliable"

# Quality control flags
QC_FLAG_HIGH_CV <- "High_CV"
QC_FLAG_OUT_OF_RANGE <- "Out_of_Range"
QC_FLAG_UNCALCULATABLE <- "Uncalculatable"
QC_FLAG_INVALID <- "Invalid"

# Default values
DEFAULT_LOG_OFFSET <- 1e-5
DEFAULT_CV_THRESHOLD <- 0.15  # 15%
DEFAULT_RESPONSE_MIN <- -0.5
DEFAULT_RESPONSE_MAX <- 10
DEFAULT_B0_NSB_MIN_DIFF <- 0.05

# LOD/LOQ/ULOQ calculation parameters
DEFAULT_LOD_MULTIPLIER <- 3    # Mean_Blank + 3*SD for LOD
DEFAULT_LOQ_MULTIPLIER <- 10   # Mean_Blank + 10*SD for LOQ
DEFAULT_RECOVERY_MIN <- 80     # Minimum acceptable recovery %
DEFAULT_RECOVERY_MAX <- 120    # Maximum acceptable recovery %
DEFAULT_ACCURACY_THRESHOLD <- 20  # Maximum %RE for standards (20%)

# Confidence interval settings
DEFAULT_CI_LEVEL <- 0.95  # 95% confidence interval

# Outlier detection settings
DEFAULT_GRUBBS_ALPHA <- 0.05  # Significance level for Grubb's test
DEFAULT_DIXON_ALPHA <- 0.05   # Significance level for Dixon's Q test

# Dilution factor column
COL_DILUTION_FACTOR <- "Dilution_Factor"
COL_FINAL_CONC <- "Final_Concentration"

# Additional QC columns
COL_RECOVERY_PCT <- "Recovery_Pct"
COL_BACK_CALC_CONC <- "Back_Calculated_Conc"
COL_PERCENT_RE <- "Percent_RE"
COL_CI_LOWER <- "CI_Lower"
COL_CI_UPPER <- "CI_Upper"
COL_IS_OUTLIER <- "Is_Outlier"
COL_EXCLUSION_REASON <- "Exclusion_Reason"
COL_EXCLUDED_BY <- "Excluded_By"
COL_EXCLUDED_AT <- "Excluded_At"

# Plot types
PLOT_TYPE_COMBINED <- "combined"
PLOT_TYPE_4PL <- "4pl"
PLOT_TYPE_5PL <- "5pl"
PLOT_TYPE_LINEAR <- "linear"

# File extensions
FILE_EXT_XLSX <- c(".xlsx", ".xls")
FILE_EXT_CSV <- c(".csv", ".txt")

# Color schemes for reliability
COLOR_RELIABLE <- "blue"
COLOR_UNRELIABLE <- "red"
COLOR_STANDARD <- "red"
COLOR_WARNING <- "orange"

# Messages
MSG_ANALYSIS_COMPLETE <- "Analysis completed successfully!"
MSG_ANALYSIS_ERROR <- "Error in analysis:"
MSG_FILE_LOADED <- "File loaded successfully. Please click 'Analyze Data' when ready."
MSG_INVALID_FILE <- "Please upload an Excel file (.xlsx or .xls)"

# Correlation thresholds for assay type detection
CORRELATION_THRESHOLD_COMPETITIVE <- -0.5  # Negative correlation
CORRELATION_THRESHOLD_DIRECT <- 0.5  # Positive correlation

# Model fitting parameters
DRM_ERROR_CONTROL <- drmc(errorm = FALSE)  # Suppress drc error messages

# 4PL parameter names (different naming conventions)
PARAM_NAMES_DRC <- list(
  upper = "d:(Intercept)",
  lower = "c:(Intercept)",
  ed50 = "e:(Intercept)",
  slope = "b:(Intercept)"
)

PARAM_NAMES_ALT <- list(
  upper = "Upper",
  lower = "Lower",
  ed50 = "ED50",
  slope = "Slope"
)
