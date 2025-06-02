EEG_combined
EEG_combined <- mutate(EEG_combined, BrainRegion = case_when(
  Electrode %in% c("F3", "F7", "FC1", "FC5") ~ "Left Anterior",
  Electrode %in% c("F4", "F8", "FC2", "FC6") ~ "Right Anterior",
  Electrode %in% c("CP1", "CP5") ~ "Left Medial",
  Electrode %in% c("CP2", "CP6") ~ "Right Medial",
  Electrode %in% c("P3", "P7") ~ "Left Posterior",
  Electrode %in% c("P4", "P8") ~ "Right Posterior",
  Electrode == "Fz" ~ "Middle Anterior",
  Electrode == "FCz" ~ "Middle Medial",
  Electrode == "Pz" ~ "Middle Posterior",
  TRUE ~ NA_character_
)
)

EEG_combined2 <- filter(EEG_combined, !is.na(BrainRegion))

EEG_combined3 <-EEG_combined2 %>% group_by(File, Condition, Time_Window, BrainRegion) %>% summarize(Voltage = mean(Voltage))

# Rename 'File' in EEG data to match the participant column in temp3
EEG_combined3 <- EEG_combined3 %>%
  rename(Participant.Public.ID = File)

# Rename columns before the model
merged_all_data <- merged_all_data %>%
  rename(
    grammaticality = Condition,
    digit_span = max_digits_recalled,
    ASRT = ASRT_score,
    stroop = difference_reaction_time,
    multilingual_language_diversity = `Multilingual Language Diversity\nScore`,
    brain_region = BrainRegion,
    time_window = Time_Window
  )


merged_all_data$grammaticality <- factor(merged_all_data$grammaticality)
merged_all_data$brain_region <- factor(merged_all_data$brain_region)
merged_all_data$time_window <- factor(merged_all_data$time_window)




