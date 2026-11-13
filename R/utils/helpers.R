# ELISA Analyzer Helper Functions
# General utility functions used throughout the application

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

#' NULL-coalescing operator
#'
#' @param a First value
#' @param b Default value if a is NULL
#' @return a if not NULL, otherwise b
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

#' Safe division that handles division by zero
#'
#' @param numerator Numerator value
#' @param denominator Denominator value
#' @param default Default value to return if denominator is zero
#' @return Result of division or default value
safe_divide <- function(numerator, denominator, default = NA) {
  ifelse(denominator == 0 | is.na(denominator), default, numerator / denominator)
}

#' Calculate coefficient of variation (CV)
#'
#' @param x Numeric vector
#' @param na.rm Logical indicating whether to remove NA values
#' @return CV as a proportion (not percentage)
calculate_cv <- function(x, na.rm = TRUE) {
  mean_val <- mean(x, na.rm = na.rm)
  sd_val <- sd(x, na.rm = na.rm)
  safe_divide(sd_val, abs(mean_val), default = NA)
}

#' Calculate standard error of the mean
#'
#' @param x Numeric vector
#' @param na.rm Logical indicating whether to remove NA values
#' @return SEM value
calculate_sem <- function(x, na.rm = TRUE) {
  n <- sum(!is.na(x))
  if (n <= 1) return(NA)
  sd(x, na.rm = na.rm) / sqrt(n)
}

#' Check if a value is within a range
#'
#' @param x Value to check
#' @param range Numeric vector of length 2 (min, max)
#' @param inclusive Logical indicating if range boundaries are inclusive
#' @return Logical indicating if x is within range
in_range <- function(x, range, inclusive = TRUE) {
  if (length(range) != 2) stop("Range must be a numeric vector of length 2")

  if (inclusive) {
    x >= min(range) & x <= max(range)
  } else {
    x > min(range) & x < max(range)
  }
}

#' Detect columns in data frame using patterns
#'
#' @param data Data frame to search
#' @param patterns Character vector of regex patterns to match
#' @param default_col Default column name if no match found
#' @return Column name or default
detect_column <- function(data, patterns, default_col = NULL) {
  all_cols <- colnames(data)

  # Check if default column exists
  if (!is.null(default_col) && default_col %in% all_cols) {
    return(default_col)
  }

  # Try to find using patterns
  for (pattern in patterns) {
    matches <- grep(pattern, all_cols, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      return(matches[1])  # Return first match
    }
  }

  # If no match found and no default, return first column or NULL
  if (length(all_cols) > 0 && !is.null(default_col)) {
    warning(paste("Column not found for patterns:", paste(patterns, collapse = ", "),
                  "Using default:", default_col))
    return(default_col)
  }

  return(NULL)
}

#' Normalize column names in a data frame
#'
#' @param data Data frame
#' @param old_name Current column name
#' @param new_name New standardized column name
#' @return Data frame with renamed column
rename_column_safe <- function(data, old_name, new_name) {
  if (length(old_name) == 1 && !is.na(old_name) && old_name %in% colnames(data) && old_name != new_name) {
    colnames(data)[colnames(data) == old_name] <- new_name
  }
  return(data)
}

#' Create timestamp string for file naming
#'
#' @param format POSIXct format string
#' @return Formatted timestamp string
get_timestamp <- function(format = "%Y%m%d_%H%M%S") {
  format(Sys.time(), format)
}

#' Format number with appropriate precision
#'
#' @param x Numeric value
#' @param digits Number of significant digits
#' @return Formatted string
format_number <- function(x, digits = 3) {
  if (is.na(x)) return("NA")
  if (is.infinite(x)) return("Inf")

  # Use scientific notation for very small or very large numbers
  if (abs(x) < 0.001 || abs(x) > 10000) {
    return(formatC(x, format = "e", digits = digits))
  } else {
    return(formatC(round(x, digits), format = "f", digits = digits))
  }
}

#' Round to significant figures
#'
#' @param x Numeric value
#' @param n Number of significant figures
#' @return Rounded value
round_sig <- function(x, n = 3) {
  if (is.na(x) || x == 0) return(x)
  signif(x, n)
}

#' Create a formatted concentration range string
#'
#' @param min_conc Minimum concentration
#' @param max_conc Maximum concentration
#' @param units Units string (optional)
#' @return Formatted range string
format_concentration_range <- function(min_conc, max_conc, units = "") {
  paste0(format_number(min_conc), " - ", format_number(max_conc), " ", units)
}

#' Check if all values in a vector are NA
#'
#' @param x Vector to check
#' @return Logical indicating if all values are NA
all_na <- function(x) {
  all(is.na(x))
}

#' Check if any values in a vector are NA
#'
#' @param x Vector to check
#' @return Logical indicating if any values are NA
any_na <- function(x) {
  any(is.na(x))
}

#' Clamp a value to be within a range
#'
#' @param x Value to clamp
#' @param min_val Minimum value
#' @param max_val Maximum value
#' @return Clamped value
clamp <- function(x, min_val, max_val) {
  pmax(min_val, pmin(max_val, x))
}

#' Get the nearest value from a vector
#'
#' @param x Target value
#' @param values Vector of values to search
#' @return Value from 'values' closest to 'x'
get_nearest_value <- function(x, values) {
  if (length(values) == 0) return(NA)
  values[which.min(abs(values - x))]
}

#' Validate that required columns exist in data frame
#'
#' @param data Data frame to check
#' @param required_cols Character vector of required column names
#' @return Logical indicating if all required columns exist
validate_columns <- function(data, required_cols) {
  missing_cols <- setdiff(required_cols, colnames(data))

  if (length(missing_cols) > 0) {
    warning(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    return(FALSE)
  }

  return(TRUE)
}

#' Create a safe log10 transformation that handles zeros and negatives
#'
#' @param x Numeric vector
#' @param offset Small positive value to add before log transformation
#' @return Log10-transformed values
safe_log10 <- function(x, offset = 1e-5) {
  log10(pmax(x + offset, offset))
}

#' Inverse of safe_log10 transformation
#'
#' @param log_x Log10-transformed values
#' @return Back-transformed values
inverse_safe_log10 <- function(log_x) {
  10^log_x
}

#' Calculate R-squared for a model
#'
#' @param observed Observed values
#' @param predicted Predicted values
#' @return R-squared value
calculate_r_squared <- function(observed, predicted) {
  tss <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
  rss <- sum((observed - predicted)^2, na.rm = TRUE)
  1 - (rss / tss)
}

#' Calculate RMSE (Root Mean Squared Error)
#'
#' @param observed Observed values
#' @param predicted Predicted values
#' @return RMSE value
calculate_rmse <- function(observed, predicted) {
  sqrt(mean((observed - predicted)^2, na.rm = TRUE))
}

#' Generate a sequence of values on log scale
#'
#' @param from Starting value
#' @param to Ending value
#' @param length.out Number of points
#' @return Numeric vector of log-spaced values
log_seq <- function(from, to, length.out = 100) {
  exp(seq(log(from), log(to), length.out = length.out))
}

#' Create a summary table from a list of values
#'
#' @param ... Named arguments to include in summary
#' @return Data frame with one row
summary_row <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

#' Convert list to data frame, handling NULL values
#'
#' @param x List to convert
#' @return Data frame
safe_list_to_df <- function(x) {
  # Replace NULL with NA
  x <- lapply(x, function(item) if (is.null(item)) NA else item)

  # Convert to data frame
  tryCatch({
    as.data.frame(x, stringsAsFactors = FALSE)
  }, error = function(e) {
    # If conversion fails, return empty data frame with appropriate columns
    data.frame()
  })
}

#' Print a formatted message with timestamp
#'
#' @param ... Message components to paste together
#' @param level Message level (INFO, WARNING, ERROR)
print_message <- function(..., level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(sprintf("[%s] %s: %s", timestamp, level, paste(..., collapse = " ")))
}

#' Does a Sample Type value denote a standard?
#'
#' Matches the configured standard label exactly ("Standard") and numbered or
#' lettered variants such as "Standard 1", "Standard_A", "Standard6", "Std 3".
#' Words that merely start with the label ("Standard Control") do not match.
#'
#' @param type Character vector of Sample Type values
#' @param std_label Configured standard label
#' @return Logical vector
is_standard_type <- function(type, std_label = "Standard") {
  type <- trimws(as.character(type))
  esc <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", std_label)
  pat <- paste0("^(", esc, "|Std|Standard)([[:space:]_.:#-]*[A-Za-z0-9]{1,3})?$")
  !is.na(type) & (type == std_label | grepl(pat, type, ignore.case = TRUE))
}

#' Collapse numbered/lettered standard labels to the single standard label
#'
#' The analysis pipeline identifies standards by one exact label. Exports that
#' number each standard ("Standard 1" ... "Standard 6") would otherwise match
#' nothing, silently dropping the 0 standard and treating it as a sample.
#'
#' @param data Data frame with standardized column names
#' @param std_label Configured standard label
#' @return Data frame with standards relabelled to std_label
normalize_standard_labels <- function(data, std_label = "Standard") {
  if (!COL_TYPE %in% colnames(data)) return(data)
  type <- as.character(data[[COL_TYPE]])
  idx <- is_standard_type(type, std_label) & type != std_label
  if (any(idx)) {
    message("  Collapsed ", sum(idx), " wells (", length(unique(type[idx])),
            " labels, e.g. '", type[idx][1], "') into '", std_label, "'")
    type[idx] <- std_label
    data[[COL_TYPE]] <- type
  }
  data
}

#' Pick the unit string out of a column header such as "Concentration (ng/ml)"
#' @param col_name Column header
#' @return Unit with conventional capitalisation ("ng/mL"), or NULL
detect_conc_units <- function(col_name) {
  if (is.null(col_name) || length(col_name) == 0 || is.na(col_name)) return(NULL)
  m <- regmatches(col_name, regexpr("\\(([^)]*/[^)]*)\\)", col_name))
  if (length(m) == 0) return(NULL)
  u <- gsub("^\\(|\\)$", "", m)
  u <- gsub("ml$", "mL", u, ignore.case = TRUE)
  u <- sub("^ug", "\u00b5g", u)
  trimws(u)
}

#' Resolve the concentration unit string from the UI choice
#'
#' @param conc_units "auto" (parse from column header), "none", or a unit string
#' @param results Analysis results (uses results$conc_units_detected)
#' @return Unit string, or NULL when no unit should be shown
resolve_conc_units <- function(conc_units = "auto", results = NULL) {
  if (is.null(conc_units) || identical(conc_units, "auto")) {
    u <- if (!is.null(results)) results$conc_units_detected else NULL
    return(if (is.null(u) || !nzchar(u)) NULL else u)
  }
  if (identical(conc_units, "none") || !nzchar(conc_units)) return(NULL)
  conc_units
}

#' Append the unit to every concentration-valued column name of a table
#'
#' e.g. Mean_Concentration -> "Mean_Concentration (pg/mL)". Also covers
#' Back_Calculated_Conc and the prediction CI bounds (CI_Lower / CI_Upper).
#'
#' @param df Data frame
#' @param units Unit string or NULL (returns df unchanged)
label_conc_units <- function(df, units) {
  if (is.null(df) || !is.data.frame(df) || is.null(units) || !nzchar(units)) return(df)
  nm <- colnames(df)
  hit <- grepl("(Concentration|Conc|Back_Calc)$|^CI_(Lower|Upper)$", nm)
  nm[hit] <- paste0(nm[hit], " (", units, ")")
  colnames(df) <- nm
  df
}


#' Apply Plate View edits to the mapped data
#'
#' @param data Data frame with standardized column names (needs COL_WELL)
#' @param assignments Named list/vector: well ID -> sample type; entries named
#'   "<well>_conc" give a concentration for that well (standards)
#' @param excluded Character vector of well IDs to drop from the analysis
#' @return Data frame with overrides applied and excluded wells removed
apply_well_overrides <- function(data, assignments = NULL, excluded = NULL) {
  if (!COL_WELL %in% colnames(data)) return(data)
  wells <- trimws(as.character(data[[COL_WELL]]))

  if (length(assignments) > 0) {
    if (!COL_TYPE %in% colnames(data)) data[[COL_TYPE]] <- NA_character_
    data[[COL_TYPE]] <- as.character(data[[COL_TYPE]])
    n_set <- 0L
    for (nm in names(assignments)) {
      if (grepl("_conc$", nm)) next
      idx <- which(wells == nm)
      if (length(idx) == 0) next
      val <- as.character(assignments[[nm]])
      data[[COL_TYPE]][idx] <- val
      n_set <- n_set + length(idx)
      conc <- assignments[[paste0(nm, "_conc")]]
      if (!is.null(conc) && nzchar(trimws(as.character(conc)))) {
        cv <- suppressWarnings(as.numeric(conc))
        if (!is.na(cv)) data[[COL_CONCENTRATION]][idx] <- cv
      }
    }
    if (n_set > 0) message("  Plate View: applied sample-type assignments to ", n_set, " wells")
  }

  if (length(excluded) > 0) {
    drop <- wells %in% as.character(excluded)
    if (any(drop)) {
      message("  Plate View: excluded ", sum(drop), " wells (", paste(wells[drop], collapse = ", "), ")")
      data <- data[!drop, , drop = FALSE]
    }
  }
  data
}
