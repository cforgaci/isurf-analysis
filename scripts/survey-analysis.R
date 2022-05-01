# 1. Load packages ----
library(qualtRics)  # Load Qualtrics survey data
library(dplyr)      # Manipulate data
library(tidyr)      # Create tidy data
library(stringr)
library(ggplot2)    # Visualize data
library(crayon)     # Color terminal output
library(gridExtra)
# library(ggpubr)

# 2. Read raw data ----
## Read the latest raw survey data file
file_list <- file.info(list.files("data", full.names = T))
file_list <- file_list[grepl("I-SURF+-+expert+evaluation", rownames(file_list), fixed = T),]
survey_latest_raw <- rownames(file_list)[which.max(file_list$mtime)]
survey <- read_survey(survey_latest_raw)
## Display the number of responses in the raw data
cat("The raw data has", nrow(survey), "responses.")

# 3. Clean data ----
## Remove partial and test responses from raw data
survey_clean <- survey %>% 
  filter(Progress == 100 &  # filter only finished entries
         !Q6_25 %in% c("Claudiu", "Jasmijn", "Marina", "Bardia", "d") &  # exclude survey team
         !Q6_26 %in% c("Overtoom") &  # exclude survey team
         !is.na(Q6_25) &  # exclude entries without author
         !Q9 == "Not familiar at all")
## Display number of responses in total and per site 
cat("The cleaned data has", nrow(survey_clean), "responses, of which:\n",
    nrow(survey_clean[survey_clean$Q8 == "Hamerkwartier",]), "for Hamerkwartier\n",
    nrow(survey_clean[survey_clean$Q8 == "De Oeverlanden",]), "for De Oeverlanden\n",
    nrow(survey_clean[survey_clean$Q8 == "Amstelscheg",]), "for Kop Amstelscheg")

# 4. Get average time spent by each respondent on a pairwise comparison ----
mean_time_spent <- function() {
  ## select columns with time metadata for comparison questions
  survey_time_comparisons <- survey_clean %>% 
    select(contains("Page Submit")) %>% 
    select(!contains("Q10_")) %>% 
    select(!contains("Q365_")) %>% 
    select(!contains("Q372_")) %>% 
    select(!contains("Q2_")) %>% 
    select(!contains("Q4_"))
  
  ## print average time spent by each respondent
  time <- data.frame("First name"=character(),
                     # "Last name"=character(),
                     "Mean time"=double())
  for(i in 1:nrow(survey_clean)){
    temp_mean_spent <- round(mean(t(survey_time_comparisons)[,i], na.rm = T) / 60, 1)
    temp_msg <- paste(survey_clean$Q6_25[i],  # first name
                      survey_clean$Q6_26[i],  # last name
                      "spent an average of", 
                      temp_mean_spent,  # average time spent on a question
                      "minutes on a comparison.\n")
    if (temp_mean_spent >= 1){
      cat(green(temp_msg))
    } else if (temp_mean_spent >= 0.5){
      cat(yellow(temp_msg))
    } else {
      cat(red(temp_msg))
    }
    time <- rbind(time, 
                  c(survey_clean$Q6_25[i], 
                    # survey_clean$Q6_26[i], 
                    round(mean(t(survey_time_comparisons)[,i], na.rm = T), 0)))
    colnames(time) <- c("First name", "Average time spent (s)")
  }
  return(survey_time_comparisons)
}
x <- mean_time_spent()

# Verify that the number of timed questions is 16
for (i in 1:nrow(survey_clean)) {
  if (length(t(x[!is.na(x)[i,]])[,i]) == 16) {
    cat(green(paste("The number of comparisons (16) made by", survey_clean$Q6_25[i], "is correct.\n")))
  } else {
    cat(red(paste("The number of comparisons (16) made by", survey_clean$Q6_25[i], "is not correct.\n")))
  }
}

# Select all comparisons
selection <- c()
counter <- 0
for (i in 1:dim(survey_clean)[1]) {
  for (j in 1:dim(survey_clean)[2]) {
    if (grepl("stan", as.character(survey_clean[i,j])) & !is.na(survey_clean[i,j])) {
      selection <- c(selection, colnames(survey_clean)[j])
      counter <- counter + 1
    }
  }
}

# Check the numnber of comparisons made by each respondent
# print(counter/nrow(survey_clean))

selection <- unique(selection)
selection <- selection[!selection %in% c("Q14", "Q369", "Q376", "Q215", "Q37_12_TEXT")]
length(selection) == 16*3

# 5. Were the projects selected more often by the respondents than the sketches? ----
resp_sel <- data.frame()
for (i in 1:nrow(survey_clean[, selection])) {
  q_comp_freq <- table(t(survey_clean[i, selection]))
  print(q_comp_freq[order(q_comp_freq, decreasing = T)])
  # Count number of projects and sketches selected by each respondent
  count_proj <- 0
  count_sket <- 0
  for (j in 1:length(q_comp_freq)) {
    if (as.numeric(str_sub(names(q_comp_freq)[j], start = -2)) > 25) {
      count_proj <- count_proj + q_comp_freq[j]
    } else {
      count_sket <- count_sket + q_comp_freq[j]
    }
  }
  # Display percentage of projects and sketches selected by each respondent
  print(paste("Projects selected:", round(count_proj/(counter/nrow(survey_clean))*100, 0), "%"))
  print(paste("Sketches selected:", round(count_sket/(counter/nrow(survey_clean))*100, 0), "%"))
  resp_sel <- rbind(resp_sel, c(round(count_proj/(counter/nrow(survey_clean))*100, 0), round(count_sket/(counter/nrow(survey_clean))*100, 0)))
}

colnames(resp_sel) <- c("prc_proj_sel", "prc_sket_sel")
resp_sel <- cbind(resp_sel, "name_resp" = sprintf("Respondent%02d", c(1:length(survey_clean$Q6_25))))  # Anonymise respondents
# resp_sel <- as_data_frame(resp_sel)
resp_sel <- resp_sel[, c(3,1,2)]
resp_sel <- pivot_longer(
  data = resp_sel, 
  cols = c("prc_proj_sel", "prc_sket_sel"), 
  names_to = "cat_sel", values_to = "prc_sel")
ggplot(resp_sel, aes(fill=cat_sel, y=prc_sel, x=name_resp)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("lightgreen","tomato")) +
  xlab("Responses per site") + ylab("% type of project") +
  ggtitle("Percentage of types of projects selected by the respondents") +
  coord_flip()
  # geom_text(
  #   aes(label="stat(prop)*100", group=1), 
  #   nudge_y = 0.125,
  #   va="bottom",
  #   format_string="{:.1f}%"
  #   )

# 6. What is the level of agreement on pairwise comparisons? ----
## This question requires at least two responses on the same question


# 7. Site profiles ----
## Remove columns from a data frame where any value is NA
all_na <- function(x) any(!is.na(x))

site_profile_sel <- function(survey = survey_clean, 
                         first_name = "Q6_25", 
                         last_name = "Q6_26", 
                         col_sel = "Q12_", 
                         row_sel) {
  x <- survey %>%
    select(first_name, last_name, contains(col_sel)) %>% 
    filter(!is.na(Q12_66)) %>% 
    select_if(all_na)
  return(x)
}
site_profile_hamerkwartier_2 <- site_profile_sel()

site_profile <- function(x) {
  x[, 3:11] <- lapply(x[, 3:11], as.factor)
  x <- as.data.frame(x)
  for (i in 3:11) {
    x[, i] = factor(x[, i],
                    ordered = TRUE,
                    levels = c("Not at all important",
                               "Slightly important",
                               "Moderately important",
                               "Very important",
                               "Extremely important"))
  }
  return(x)
}

# Get site profile data for Hamerkwartier
site_profile_hamerkwartier <- survey_clean %>% 
  select(Q6_25, Q6_26, contains("Q12_")) %>% 
  filter(!is.na(Q12_66)) %>% 
  select_if(all_na) %>% 
  site_profile() %>% 
  mutate(name_respondent = paste(Q6_25, Q6_26)) %>% 
  select(!contains("Q6_")) %>% 
  select(name_respondent, everything()) %>% 
  pivot_longer(-name_respondent, names_to = "question", values_to = "evaluation")

# Get site profile data for De Oeverlanden
site_profile_oeverlanden <- survey_clean %>% 
  select(Q6_25, Q6_26, contains("Q12_")) %>% 
  filter(!is.na(Q12_66_1)) %>% 
  select_if(all_na) %>% 
  site_profile() %>% 
  mutate(name_respondent = paste(Q6_25, Q6_26)) %>% 
  select(!contains("Q6_")) %>% 
  select(name_respondent, everything()) %>% 
  pivot_longer(-name_respondent, names_to = "question", values_to = "evaluation")

# Get site profile data for Kop Amstelscheg
site_profile_amstelscheg <- survey_clean %>% 
  select(Q6_25, Q6_26, contains("Q12_")) %>% 
  filter(!is.na(Q12_66_2)) %>% 
  select_if(all_na) %>% 
  site_profile() %>% 
  mutate(name_respondent = paste(Q6_25, Q6_26)) %>% 
  select(!contains("Q6_")) %>% 
  select(name_respondent, everything()) %>% 
  pivot_longer(-name_respondent, names_to = "question", values_to = "evaluation")

# Plot site profile data 
site_profile_hamerkwartier$evaluation2 <- as.numeric(site_profile_hamerkwartier$evaluation)
bp_hamerkwartier <- ggplot(site_profile_hamerkwartier, aes(x = question, y = evaluation2)) +
  geom_boxplot() +
  coord_flip() +
  labs()

site_profile_oeverlanden$evaluation2 <- as.numeric(site_profile_oeverlanden$evaluation)
bp_oeverlanden <- ggplot(site_profile_oeverlanden, aes(x = question, y = evaluation2)) +
  geom_boxplot() +
  coord_flip() +
  labs()

site_profile_amstelscheg$evaluation2 <- as.numeric(site_profile_amstelscheg$evaluation)
bp_amstelscheg <- ggplot(site_profile_amstelscheg, aes(x = question, y = evaluation2)) +
  geom_boxplot() +
  coord_flip() +
  labs()

figure <- grid.arrange(bp_hamerkwartier, bp_oeverlanden, bp_amstelscheg, ncol = 3)
figure





par(mfrow=c(1,3))
boxplot(site_profile_amstelscheg[, 3:11], las = 1, horizontal = T)
boxplot(site_profile_oeverlanden[, 3:11], las = 1, horizontal = T)
boxplot(site_profile_hamerkwartier[, 3:11], las = 1, horizontal = T)
par(mfrow=c(1,1))







ggplot(t(as.numeric(site_profile_amstelscheg[, 3:11]))) +
  geom_boxplot()
  
site_profile_amstelscheg_long <- pivot_longer(
  data = site_profile_amstelscheg, 
  cols = 3:11, 
  names_to = "q_num", values_to = "imp")
site_profile_amstelscheg_long$imp <- as.numeric(site_profile_amstelscheg_long$imp)

site_profile_amstelscheg_sum <- site_profile_amstelscheg_long %>% 
  group_by(q_num) %>% 
  summarise("sum" = sum(imp))
ggplot(site_profile_amstelscheg_sum, (x = q_num, y = sum)) +
  geom_bar()


ggplot(site_profile_amstelscheg_long, aes(x = q_num, y = imp, fill = Q6_26)) +
  geom_boxplot() +
  ggtitle("Amstelscheg") +








for (i in 3:11) {
  site_profile_amstelscheg %>% 
    mutate("i" = factor(site_profile_amstelscheg[, i], levels = c("Not at all important", 
                                                                "Slightly important", 
                                                                "Moderately important", 
                                                                "Very important", 
                                                                "Extremely important"))
          )
}


# X. Scraps ----

survey_time <- as.data.frame(cbind(t(survey_first_click[1,]), t(survey_last_click[1,]), t(survey_click_count[1,]), t(survey_page_submit[1,])))  # add this to a loop and replace [1,] with [i,] to process multiple rows
colnames(survey_time) <- c("first_click", "last_click", "click_count", "page_submit")
rownames(survey_time) <- gsub("_.*", "", rownames(survey_time))  # remove string after question number
survey_time <- cbind(survey_time, rownames(survey_time))
colnames(survey_time)[5] <- "question_number"
dim(survey_time)
survey_time <- survey_time %>% 
  filter(!is.na(page_submit))

ggplot(survey_time, aes(question_number, page_submit)) +
  geom_point() + 
  geom_smooth(method = "lm")


