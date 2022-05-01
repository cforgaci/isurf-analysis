# Evaluation of the I-SURF instruments according to workshop participants
# Claudiu Forgaci

# 1. Load packages ----
library(qualtRics)    # Load Qualtrics survey data
library(dplyr)        # Manipulate data
library(readr)        # Read rectangular data
library(tidyr)        # Make tidy data
library(stringr)      # Manipulate strings
library(ggplot2)      # Visualize data
library(crayon)       # Color terminal output
library(gridExtra)    # Lay out multiple plots
# library(ggpubr)

# 2. Read the raw data ----
## Read daily workshop evaluation data from paper-based questionnaire
eval_w <- tibble()
for (i in 1:4) {
  eval_day <- read_csv(paste0("data/eval-day", i, ".csv"))[,-1] %>%  # remove first row
    select(-contains("lecture")) %>%  # remove lectures, not relevant for analysis
    mutate(day = factor(i, levels = c(1:4), 
                        labels = c("Day 1", "Day 2", "Day 3", "Day 4")))  # add column with workshop day
  eval_w <- rbind(eval_w, eval_day)
}
# rm(list = c("eval_day", "i"))

## Read post-workshop evaluation data downloaded from Qualtrics (online questionnaire)
eval_pw <- read_survey("data/eval-post-workshop.csv")

# 3. Clean the data ----
## Remove variables that are not relevant for the analysis (evaluation of seminar lectures in this case)
# eval_days_list <- list(eval_day1, eval_day2, eval_day3, eval_day4)
# eval_days_list <- lapply(eval_days_list, function(x) {x <- x %>% select(!contains("lecture"))})

## Combine evaluation sheets in one data frame
### Add day number to each sheet in a new column 'day'
# for (i in 1:4) {
#   eval_days_list[[i]] <- eval_days_list[[i]] %>%
#     mutate(day = i)
# }
### Combine sheets into 'eval_all'
# eval_all <- rbind(eval_days_list[[1]], eval_days_list[[2]], eval_days_list[[3]], eval_days_list[[4]])

barplot(table(eval_w$day))
barplot(table(eval_w$workshop_workload))

# WORKSHOP - WORKLOAD: How was the workload perceived by the participants throughout the workshop? ----
eval_w$workshop_workload <- factor(eval_w$workshop_workload, 
                                     levels = c("Not enough",
                                                "The right amount of work",
                                                "Too much"))

eval_w$seminar_usefulness <- factor(eval_w$seminar_usefulness, 
                                      levels = c("Not at all",
                                                 "Somehow",
                                                 "Very useful"))

eval_w$day <- factor(eval_w$day, 
                       levels = c(1:4),
                       labels = c("Day 1: The Connector",
                                  "Day 2: The Sponge",
                                  "Day 3: The Integrator",
                                  "Day 4: The Scaler"))

ggplot(data = eval_w, mapping = aes(x = workshop_workload, fill = day)) + 
  geom_bar(position = "dodge2") + 
  labs(x = "Perceived workload", 
       y = "Count", 
       title = "Workload perceived by the workshop participants") +
  coord_flip() +
  facet_wrap(~ day, nrow = 1)

# table(eval_day1$workshop_workload)
# table(eval_day2$workshop_workload)
# table(eval_day3$workshop_workload)
# table(eval_day4$workshop_workload)
# table(eval_all$workshop_workload)

# INSTRUMENTS - EASE OF USE: How easy were the instruments to use according to the workshop participants? ----
## Select scores for The Connector from evaluation sheet and save the plot the results
# connector_easeOfUse <- eval_all %>% 
#   filter(day == 1) %>% 
#   select(instrument_easeOfUse)
# 
# conn_eou_plot <- ggplot(connector_easeOfUse, aes(instrument_easeOfUse)) + 
#   geom_bar(fill = "lightblue") +
#   labs(title = "Connector\n", x = "\nScore", y = "Frequency\n") +
#   theme_classic() +
#   scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1)) +
#   ylim(0,15)

## Select scores for The Sponge from evaluation sheet and save the plot the results
# sponge_easeOfUse <- eval_all %>% 
#   filter(day == 2) %>% 
#   select(instrument_easeOfUse)
# 
# spon_eou_plot <- ggplot(sponge_easeOfUse, aes(instrument_easeOfUse)) + 
#   geom_bar(fill = "lightblue") +
#   labs(title = "Sponge\n", x = "\nScore", y = "Frequency\n") +
#   theme_classic() +
#   scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1)) +
#   ylim(0,15)

## Select scores for The Integrator from evaluation sheet and save the plot the results
# integrator_easeOfUse <- eval_all %>% 
#   filter(day == 3) %>% 
#   select(instrument_easeOfUse)
# 
# intg_eou_plot <- ggplot(integrator_easeOfUse, aes(instrument_easeOfUse)) + 
#   geom_bar(fill = "lightblue") +
#   labs(title = "Integrator\n", x = "\nScore", y = "Frequency\n") +
#   theme_classic() +
#   scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1)) +
#   ylim(0,15)

## Select scores for The Scaler from evaluation sheet and save the plot results
# scaler_easeOfUse <- eval_all %>% 
#   filter(day == 4) %>% 
#   select(instrument_easeOfUse)
# 
# scal_eou_plot <- ggplot(scaler_easeOfUse, aes(instrument_easeOfUse)) + 
#   geom_bar(fill = "lightblue") +
#   labs(title = "Scaler\n", x = "\nScore", y = "Frequency\n") +
#   theme_classic() +
#   scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1)) +
#   ylim(0,15)

ggplot(eval_all, aes(instrument_easeOfUse)) + 
  geom_bar(fill = "lightblue") +
  labs(title = "Instruments ease of use", x = "\nScore", y = "Frequency\n") +
  theme_classic() +
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1)) +
  # ylim(0,15) +
  facet_wrap(~ day)

ggplot(eval_all, aes(x = instrument_easeOfUse, y = day)) +
  geom_boxplot() + 
  coord_flip() +
  scale_x_continuous(breaks = c(1:10), limits = c(1,10)) +
  labs(x = "Ease of Use", y = "Instrument")

## Combine the four plots
# grid.arrange(conn_eou_plot, spon_eou_plot, intg_eou_plot, scal_eou_plot, nrow = 2)
# 
# round(mean(connector_easeOfUse$instrument_easeOfUse, na.rm = T), 2)
# round(mean(sponge_easeOfUse$instrument_easeOfUse, na.rm = T), 2)
# round(mean(integrator_easeOfUse$instrument_easeOfUse, na.rm = T), 2)
# round(mean(scaler_easeOfUse$instrument_easeOfUse, na.rm = T), 2)

# INSTRUMENTS - EASE OF USE (post-workshop evaluation) ----
## Select variables related to ease of use
inst_eou_pw <- eval_pw[,c("Q3_1", "Q3_2", "Q4_1", "Q4_2", "Q4_3",
                          "Q10_1", "Q10_2", "Q11_1", "Q11_2", "Q11_3",
                          "Q17_1", "Q17_2", "Q18_1", "Q18_2", "Q18_3",
                          "Q24_1", "Q24_2", "Q25_1", "Q25_2", "Q25_3",
                          "Q31_1", "Q31_2", "Q31_3", "Q31_4",
                          "Q34_1", "Q34_2", "Q34_3", "Q34_4")]

## Remove incomplete entries
inst_eou_pw <- inst_eou_pw %>% 
  filter(!is.na(Q3_1))

glimpse(inst_eou_pw)

inst_eou_elem_1 <- c("Q4_1", "Q4_2", "Q4_3")
inst_eou_elem_2 <- c("Q11_1", "Q11_2", "Q11_3")
inst_eou_elem_3 <- c("Q18_1", "Q18_2", "Q18_3")
inst_eou_elem_4 <- c("Q25_1", "Q25_2", "Q25_3")

inst_eou_pw[,inst_eou_elem_1] <- lapply(inst_eou_pw[,inst_eou_elem_1], 
                                        factor, 
                                        levels = c("Difficult to work with",
                                                   "Neither difficult or easy to work with",
                                                   "Easy to work with"))
inst_eou_pw[,inst_eou_elem_2] <- lapply(inst_eou_pw[,inst_eou_elem_2], 
                                        factor, 
                                        levels = c("Difficult to use",
                                                   "Neither difficult or easy to use",
                                                   "Easy to use"))
inst_eou_pw[,inst_eou_elem_3] <- lapply(inst_eou_pw[,inst_eou_elem_3], 
                                        factor, 
                                        levels = c("Difficult",
                                                   "Neither difficult nor easy",
                                                   "Easy"))
inst_eou_pw[,inst_eou_elem_4] <- lapply(inst_eou_pw[,inst_eou_elem_4], 
                                        factor, 
                                        levels = c("Difficult",
                                                   "Neither difficult nor easy",
                                                   "Easy to use"))
## Ease of use - the elements of the Connector
eou_Q4_1 <- ggplot(inst_eou_pw, aes(Q4_1)) +
  geom_bar(fill = "grey") +
  ylim(0,15)
eou_Q4_2 <- ggplot(inst_eou_pw, aes(Q4_2)) +
  geom_bar(fill = "grey") +
  ylim(0,15)
eou_Q4_3 <- ggplot(inst_eou_pw, aes(Q4_3)) +
  geom_bar(fill = "grey") +
  ylim(0,15)

## Ease of use - the elements of the Sponge
eou_Q11_1 <- ggplot(inst_eou_pw, aes(Q11_1)) +
  geom_bar(fill = "grey") +
  ylim(0,15)
eou_Q11_2 <- ggplot(inst_eou_pw, aes(Q11_2)) +
  geom_bar(fill = "grey") +
  ylim(0,15)
eou_Q11_3 <- ggplot(inst_eou_pw, aes(Q11_3)) +
  geom_bar(fill = "grey") +
  ylim(0,15)

## Ease of use - the elements of the Integrator
eou_Q18_1 <- ggplot(inst_eou_pw, aes(Q18_1)) +
  geom_bar(fill = "grey") +
  ylim(0,15)
eou_Q18_2 <- ggplot(inst_eou_pw, aes(Q18_2)) +
  geom_bar(fill = "grey") +
  ylim(0,15)
eou_Q18_3 <- ggplot(inst_eou_pw, aes(Q18_3)) +
  geom_bar(fill = "grey") +
  ylim(0,15)

## Ease of use - the elements of the Scaler
eou_Q25_1 <- ggplot(inst_eou_pw, aes(Q25_1)) +
  geom_bar(fill = "grey") +
  ylim(0,15)
eou_Q25_2 <- ggplot(inst_eou_pw, aes(Q25_2)) +
  geom_bar(fill = "grey") +
  ylim(0,15)
eou_Q25_3 <- ggplot(inst_eou_pw, aes(Q25_3)) +
  geom_bar(fill = "grey") +
  ylim(0,15)

## Plot ease of use from post-workshop evaluation data
grid.arrange(eou_Q4_1, eou_Q4_2, eou_Q4_3, 
             eou_Q11_1, eou_Q11_2, eou_Q11_3, 
             eou_Q18_1, eou_Q18_2, eou_Q18_3, 
             eou_Q25_1, eou_Q25_2, eou_Q25_3,
             nrow = 4)

paste0("The Connector received a score of ", 
       round(mean(inst_eou_pw$Q3_1, na.rm = T), 2), 
       " out of 10 for ease of use.")
paste0("The Sponge received a score of ", 
       round(mean(inst_eou_pw$Q10_1, na.rm = T), 2), 
       " out of 10 for ease of use.")
paste0("The Integrator received a score of ", 
       round(mean(inst_eou_pw$Q17_1, na.rm = T), 2), 
       " out of 10 for ease of use.")
paste0("The Scaler received a score of ", 
       round(mean(inst_eou_pw$Q24_1, na.rm = T), 2), 
       " out of 10 for ease of use.")

inst_eou_pw_all <- data.frame(means = c(round(mean(inst_eou_pw$Q3_1, na.rm = T), 2),
                                round(mean(inst_eou_pw$Q10_1, na.rm = T), 2),
                                round(mean(inst_eou_pw$Q17_1, na.rm = T), 2),
                                round(mean(inst_eou_pw$Q24_1, na.rm = T), 2)))

# INSTRUMENTS - USEFULNESS: How useful were the instruments according to the workshop participants? ----
## Select scores for The Connector from evaluation sheet and save the plot the results
connector_usefulness <- eval_all %>% 
  filter(day == 1) %>% 
  select(instrument_usefulness) %>% 
  mutate(instrument = "connector_usefulness")

conn_use_plot <- ggplot(connector_usefulness, aes(instrument_usefulness)) + 
  geom_bar(fill = "lightblue") +
  labs(title = "The Connector (Day 1)\n", x = "Score", y = "Frequency") +
  theme_classic() + 
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1)) +
  ylim(0,15)

## Select scores for The Sponge from evaluation sheet and save the plot the results
sponge_usefulness <- eval_all %>% 
  filter(day == 2) %>%  
  select(instrument_usefulness) %>% 
  mutate(instrument = "sponge_usefulness")

spon_use_plot <- ggplot(sponge_usefulness, aes(instrument_usefulness)) + 
  geom_bar(fill = "lightblue") +
  labs(title = "The Sponge (Day 2)\n", x = "Score", y = "Frequency") +
  theme_classic() + 
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1)) +
  ylim(0,15)

## Select scores for The Integrator from evaluation sheet and save the plot the results
integrator_usefulness <- eval_all %>% 
  filter(day == 3) %>% 
  select(instrument_usefulness) %>% 
  mutate(instrument = "integrator_usefulness")

intg_use_plot <- ggplot(integrator_usefulness, aes(instrument_usefulness)) + 
  geom_bar(fill = "lightblue") +
  labs(title = "The Integrator (Day 3)\n", x = "Score", y = "Frequency") +
  theme_classic() + 
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1)) +
  ylim(0,15)

## Select scores for The Scaler from evaluation sheet and save the plot the results
scaler_usefulness <- eval_all %>% 
  filter(day == 4) %>% 
  select(instrument_usefulness) %>% 
  mutate(instrument = "scaler_usefulness")

scal_use_plot <- ggplot(scaler_usefulness, aes(instrument_usefulness)) + 
  geom_bar(fill = "lightblue") +
  labs(title = "The Scaler (Day 4)\n", x = "Score", y = "Frequency") +
  theme_classic() + 
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1)) +
  ylim(0,15)

instruments_usefulness <- rbind(connector_usefulness, 
                                sponge_usefulness,
                                integrator_usefulness,
                                scaler_usefulness)

inst_use_plot <- ggplot(instruments_usefulness, aes(x = instrument, y = instrument_usefulness)) +
  geom_boxplot() +
  theme_classic() 

## Combine the four plots
grid.arrange(conn_use_plot, spon_use_plot, intg_use_plot, scal_use_plot, inst_use_plot, nrow = 3)

round(mean(connector_usefulness$instrument_usefulness, na.rm = T), 2)
round(mean(sponge_usefulness$instrument_usefulness, na.rm = T), 2)
round(mean(integrator_usefulness$instrument_usefulness, na.rm = T), 2)
round(mean(scaler_usefulness$instrument_usefulness, na.rm = T), 2)

# INSTRUMENTS - USABILITY: ----

names(eval_all)

eval_long <- pivot_longer(eval_all, c(3:10,12:15), "activities") %>% select(-c(workshop_workload, seminar_usefulness))

eval_long %>%
  group_by(day, activities) %>% 
  summarise(sum(value, na.rm = TRUE))


ggplot(eval_long, aes(x = value)) +
  stat_count() +
  coord_flip() +
  # scale_x_discrete() +
  facet_wrap(~ day)


ggplot(eval_w, aes(x = team)) + 
  geom_bar() + 
  scale_x_continuous(breaks = c(1:10)) +
  coord_cartesian(xlim = c(1,10)) +
  facet_wrap(~ day)
