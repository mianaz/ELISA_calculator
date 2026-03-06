# ELISA Sample Summary Functions
# Functions for summarizing sample predictions

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