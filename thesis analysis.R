temp <- Behavioral_session2_combined_data
colnames(temp)

unique(temp$home_IDs)

n_resp <- temp %>% group_by(SubjectCode) %>% summarise(N = n())

sub_ids <- select(data, home_IDs, subject, country)

temp <- left_join(temp, sub_ids, by = c("subject","country"))

temp2 <- select(temp, home_IDs, grammaticality,correct, country, sentence1)
temp2 <- rename(temp2, Participant.Public.ID = home_IDs)

library(dplyr)
library(ggplot2)

# Filter for grammatical and gender violation conditions
plot_data <- temp2 %>%
  filter(grammaticality %in% c("grammatical", "gender violation")) %>%
  group_by(Participant.Public.ID, grammaticality) %>%
  summarise(accuracy = mean(correct, na.rm = TRUE) * 100, .groups = 'drop')

# Rename conditions for clarity
plot_data$grammaticality <- dplyr::recode(plot_data$grammaticality,
                                          "grammatical" = "Grammatical",
                                          "gender violation" = "Gender Violation")

# Create the boxplot with legend
ggplot(plot_data, aes(x = grammaticality, y = accuracy, fill = grammaticality)) +
  geom_boxplot() +
  ylab("Accuracy Percentage (%)") +
  xlab("Conditions") +
  theme_minimal() +
  theme(legend.title = element_blank())  # Keeps legend but removes legend title

# Run paired-sample t-test on your summarized accuracy data
t_test_result <- t.test(
  accuracy ~ grammaticality,
  data = plot_data,
  paired = TRUE
)

# View the result
print(t_test_result)



wm <- rbind(Spain_DGS_max_digits,Norway_DGS_max_digits)

temp2 <- left_join(temp2, wm, by = "Participant.Public.ID")

unique(temp2$Participant.Public.ID)
temp3 <- filter(temp2, is.na(max_digits_recalled))

unique(temp3$Participant.Public.ID)

ASRTs <- rbind(Norway_ASRT_progress, Spain_ASRT_progress)
ASRTs$`Participant Public ID`
ASRTs <- rename(ASRTs, Participant.Public.ID = `Participant Public ID`)
ASRTs <- select(ASRTs, Participant.Public.ID, ASRT_score)

temp2 <- left_join(temp2, ASRTs, by = "Participant.Public.ID")

stroop <- rbind(Norway_difference_reaction_time, Spain_difference_reaction_time)
stroop <- select(stroop, Participant.Public.ID, difference_reaction_time)

temp2 <- left_join(temp2, stroop, by = "Participant.Public.ID")

unique(temp2$grammaticality)
temp3 <- filter(temp2, grammaticality !="number violation")

count(temp2,Participant.Public.ID,grammaticality)

counts_con <- temp3 %>% group_by(grammaticality) %>% summarise(N )

unique(temp3$sentence1)

library(lme4)

m_wm <- glmer(correct ~ max_digits_recalled + (1|Participant.Public.ID) + (1|sentence1), family = "binomial", data = temp3)

summary(m_wm)

class(temp3$ASRT_score)
unique(temp3$ASRT_score)

temp4 <- filter(temp3, ASRT_score < 1000)

m_asrt <- glmer(correct ~ ASRT_score + (1|Participant.Public.ID)+ (1|sentence1), family = "binomial", data = temp4)

summary(m_asrt)

m_stroop <- glmer(correct ~ difference_reaction_time + (1|Participant.Public.ID)+ (1|sentence1), family = "binomial", data = temp3)

summary(m_stroop)

scores <- select(temp3, Participant.Public.ID,max_digits_recalled, ASRT_score, difference_reaction_time) %>% distinct()

temp3[temp3$Participant.Public.ID == "prmg0",]$ASRT_score = NA

mean_max_digits <- mean(scores$max_digits_recalled, na.rm = T)
mean_ASRT_score <- mean(scores$ASRT_score, na.rm = T)
mean_difference_RT<- mean(scores$difference_reaction_time, na.rm = T)

sd_max_digits <- sd(scores$max_digits_recalled, na.rm = T)
sd_ASRT_score <- sd(scores$ASRT_score, na.rm = T)
sd_difference_RT<- sd(scores$difference_reaction_time, na.rm = T)

temp3 <- mutate(temp3, max_digits_ST = (max_digits_recalled-mean_max_digits)/sd_max_digits,
                ASRT_score_ST = (ASRT_score-mean_ASRT_score)/sd_ASRT_score,
                difference_RT_ST = (difference_reaction_time-mean_difference_RT)/sd_difference_RT
                )

m_all <- glmer(correct ~ max_digits_ST + ASRT_score_ST+ difference_RT_ST + (1|Participant.Public.ID)+ (1|sentence1), family = "binomial", data = temp3)

summary(m_all)

temp <- select(temp, )

library(sjPlot)

# Plot 1: Working Memory (Digit Span) ---
p_wm <- plot_model(m_wm, type = "pred", terms = "max_digits_recalled[all]") +
  theme_sjplot() +
  labs(
    x = "Digit Span Score",               
    y = "Grammatical Accuracy (%)"         
  ) +
  theme(plot.title = element_blank())      

# Plot 2: Implicit Learning (ASRT) ---
p_asrt <- plot_model(m_asrt, type = "pred", terms = "ASRT_score[all]") +
  theme_sjplot() +
  labs(
    x = "ASRT Score",
    y = "Grammatical Accuracy (%)"
  ) +
  theme(plot.title = element_blank())

# Plot 3: Inhibitory Control (Stroop) ---
p_stroop <- plot_model(m_stroop, type = "pred", terms = "difference_reaction_time[all]") +
  theme_sjplot() +
  labs(
    x = "Stroop RT Difference",
    y = "Grammatical Accuracy (%)"
  ) +
  theme(plot.title = element_blank())
