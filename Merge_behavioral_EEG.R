# Script: Merge_behavioral_EEG.R
# Purpose: Merge aggregated EEG data with EF scores and behavioral accuracy
# Summarize EF data per participant
EF_summary <- temp3 %>%
  select(Participant.Public.ID,
         max_digits_recalled, ASRT_score, difference_reaction_time,
         max_digits_ST, ASRT_score_ST, difference_RT_ST) %>%
  distinct()


test <- EEG_combined3 %>% 
  mutate(country = ifelse(str_detect(Participant.Public.ID, "[a-z, A-Z]+", negate=FALSE),
                          "Spain", "Norway"),
         subject = ifelse(country == "Norway", Participant.Public.ID, 
                          str_extract(Participant.Public.ID, "^([:digit:])+")),
         subject = as.numeric(subject))

test_lookup <- test %>% 
  left_join(lookup_table, by = c("country", "subject"))

merged_EEG_score_data <- test_lookup %>% 
  left_join(scores, by=c("home_IDs" = "Participant.Public.ID"))
