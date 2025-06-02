library(dplyr)
library(stringr)
library(ggplot2)


#model <- lm(Voltage ~ max_digits_recalled + ASRT_score + difference_reaction_time, data = merged_EEG_score_data)

# Summarize results
#summary(model)


library(lme4)
library(lmerTest) # Provides p-values for mixed models

unique(merged_EEG_score_data$BrainRegion)
merged_EEG_score_data2 <- mutate(merged_EEG_score_data, Hemisphere = case_when(
  str_detect(BrainRegion,"Left") ~ "Left",
  str_detect(BrainRegion,"Right") ~ "Right",
  str_detect(BrainRegion,"Middle") ~ "Midline"
)) 

merged_EEG_score_data2 <- mutate(merged_EEG_score_data2, Caudality = case_when(
  str_detect(BrainRegion,"Anterior") ~ "Anterior",
  str_detect(BrainRegion,"Posterior") ~ "Posterior",
  str_detect(BrainRegion,"Medial") ~ "Medial"
)) 

# Copy the multilingual data into the merged EEG dataframe
merged_EEG_score_data2$multilingual_language_diversity = merged_all_data$multilingual_language_diversity

# Change the name of the conditions
merged_EEG_score_data2[merged_EEG_score_data2$Condition == "AverageCorrect_2",]$Condition   = "Average Correct"
merged_EEG_score_data2[merged_EEG_score_data2$Condition == "AverageViolation_2",]$Condition = "Average Violation"

# Load required package
library(lmerTest) 
 
# Filter the dataset to include only 200–500 ms
EEG_200_500 <- merged_EEG_score_data2 %>%
  filter(Time_Window == "200-500 ms")

# Model for 200-500 ms
model_200_500 <- lmer(Voltage ~ Condition * Hemisphere * Caudality + (1 | Participant.Public.ID), 
                      data = EEG_200_500)

# View summary
summary(model_200_500)


# Filter the dataset to include only 300–600 ms
EEG_300_600 <- merged_EEG_score_data2 %>%
  filter(Time_Window == "300-600 ms")

# Model for 300–600 ms
model_300_600 <- lmer(Voltage ~ Condition * Hemisphere * Caudality + (1 | Participant.Public.ID), 
                      data = EEG_300_600)

summary(model_300_600)


# Filter the dataset to include only 400–900 ms
EEG_400_900 <- merged_EEG_score_data2 %>%
  filter(Time_Window == "400-900 ms")

# Model for 300–600 ms
model_400_900 <- lmer(Voltage ~ Condition * Hemisphere * Caudality + (1 | Participant.Public.ID), 
                      data = EEG_400_900)

summary(model_400_900)

# -----------------------
# Final model: 200–500 ms
# ROI: Middle Posterior (Pz), Right Posterior (P4, P8)
# -----------------------
EEG_200_500_ROI <- EEG_200_500 %>%
  filter(BrainRegion %in% c("Middle Posterior", "Right Posterior"))

model_200_500_ROI <- lmer(Voltage ~ Condition * (max_digits_recalled + ASRT_score + difference_reaction_time) +
                            (1 | Participant.Public.ID),
                          data = EEG_200_500_ROI)

summary(model_200_500_ROI)

# -----------------------
# Final model: 300–600 ms
# ROI: Middle Posterior, Left Posterior, Right Posterior, Left Medial, Right Medial
# -----------------------
EEG_300_600_ROI <- EEG_300_600 %>%
  filter(BrainRegion %in% c("Middle Posterior", "Left Posterior", "Right Posterior",
                            "Left Medial", "Right Medial"))

model_300_600_ROI <- lmer(Voltage ~ Condition * (max_digits_recalled + ASRT_score + difference_reaction_time) +
                            (1 | Participant.Public.ID),
                          data = EEG_300_600_ROI)

summary(model_300_600_ROI)


# Final model: 400–900 ms
# ROI: Middle Posterior (Pz)
# -----------------------
EEG_400_900_ROI <- EEG_400_900 %>%
  filter(BrainRegion == "Middle Posterior")

model_400_900_ROI <- lmer(Voltage ~ Condition * (max_digits_recalled + ASRT_score + difference_reaction_time) +
                            (1 | Participant.Public.ID),
                          data = EEG_400_900_ROI)

summary(model_400_900_ROI)

### PlotStroop and Condition ###

library(ggeffects)
library(ggplot2)

### Plot Stroop and Condition (200–500 ms)
plot_stroop_200_500 <- ggpredict(model_200_500_ROI, terms = c("difference_reaction_time", "Condition"))

ggplot(plot_stroop_200_500, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "Stroop Score (Difference RT)",
    y = "Predicted Voltage"
  ) +
  theme_light()

### Plot ASRT and Condition (300–600 ms)
plot_asrt_300_600 <- ggpredict(model_300_600_ROI, terms = c("ASRT_score", "Condition"))

ggplot(plot_asrt_300_600, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "ASRT Score (Implicit Learning)",
    y = "Predicted Voltage"
  ) +
  theme_light()


### Add MLDS as a predictor ###
# Load packages (if not already loaded)
library(lme4)
library(lmerTest)

# ---------------------------
# 1. Model for 200–500 ms ROI
# ---------------------------
model_200_500_mlds <- lmer(
  Voltage ~ Condition * (max_digits_recalled + ASRT_score + difference_reaction_time + multilingual_language_diversity) +
    (1 | Participant.Public.ID),
  data = EEG_200_500_ROI
)
summary(model_200_500_mlds)

# ---------------------------
# 2. Model for 300–600 ms ROI
# ---------------------------
model_300_600_mlds <- lmer(
  Voltage ~ Condition * (max_digits_recalled + ASRT_score + difference_reaction_time + multilingual_language_diversity) +
    (1 | Participant.Public.ID),
  data = EEG_300_600_ROI
)
summary(model_300_600_mlds)

# ---------------------------
# 3. Model for 400–900 ms ROI
# ---------------------------
model_400_900_mlds <- lmer(
  Voltage ~ Condition * (max_digits_recalled + ASRT_score + difference_reaction_time + multilingual_language_diversity) +
    (1 | Participant.Public.ID),
  data = EEG_400_900_ROI
)
summary(model_400_900_mlds)


### run more model to answer RQ2 ### (executive function interactions on behavior)
library(lme4)
# Standardize predictor variables to improve model convergence
temp4$WM_z <- scale(temp4$max_digits_recalled.x)
temp4$ASRT_z <- scale(temp4$ASRT_score)
temp4$Inhib_z <- scale(temp4$difference_reaction_time.x)

# Run the interaction model using standardized predictors
model_EF_interaction_z <- glmer(correct ~ WM_z * ASRT_z * Inhib_z + 
                                  (1 | Participant.Public.ID),
                                data = temp4,
                                family = binomial)

# Show summary of model results
summary(model_EF_interaction_z)

#library(broom)
library(broom.mixed)
library(readr)
#model_200_500_ROI
tidy(model_200_500_ROI)
tbl_model_200_500_ROI <- tidy(model_200_500_ROI)
write_csv2(tbl_model_200_500_ROI, "tbl_model_200_500_ROI.csv")

#model_200_500_mlds
tidy(model_200_500_mlds)
tbl_model_200_500_mlds <- tidy(model_200_500_mlds)
write_csv2(tbl_model_200_500_mlds, "tbl_model_200_500_mlds.csv")

#model_300_600_ROI
tidy(model_300_600_ROI)
tbl_model_300_600_ROI <- tidy(model_300_600_ROI)
write_csv2(tbl_model_300_600_ROI, "tbl_model_300_600_ROI.csv")

#model_300_600_mlds
tidy(model_300_600_mlds)
tbl_model_300_600_mlds <- tidy(model_300_600_mlds)
write_csv2(tbl_model_300_600_mlds, "tbl_model_300_600_mlds.csv")

#model_400_900_ROI
tidy(model_400_900_ROI)
tbl_model_400_900_ROI <- tidy(model_400_900_ROI)
write_csv2(tbl_model_400_900_ROI, "tbl_model_400_900_ROI.csv")

#model_400_900_ROI
tidy(model_400_900_mlds)
tbl_model_400_900_mlds <- tidy(model_400_900_mlds)
write_csv2(tbl_model_400_900_mlds, "tbl_model_400_900_mlds.csv")















