# Simulation for Hiring as Exploring by Li et al. 
# Author : Yiran Jia 
# Date: Sep 11th, 2023 
# Version: Natural imbalance with correct model. Here we simulate based on the assumption that the imbalance occur
# in the underlying distribution, and use logistic model to simulate the hiring outcome from the features 
# education, work history, and skill level. Later we compare the performance among Static SL model (logistic model), 
# USL, and UCB. Since this imbalance happens on the population level, we will simulate the test data the same way
# as the training data, i.e. making sure the racial composition remains same. 


# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(sn)
library(patchwork)
library(cowplot)
# library(caret)
# library(glmnet)

# We set seed for reproductive work. However keep in mind the result changes anyway whenever the number of 
# observation generated is changing. 
set.seed(12345) 

# SECTION ONE: DATA GENERATION FOR FIXED HIRING POTENTIAL SENARIO
# Description: In scenarios with fixed hiring potential, we set the underlying likelihood of 
# hiring for Black and Hispanic candidates HIGHER than for White and Asian candidates. 

#######################################################
# Set up which top percent 
which_top_percent = 0.25

# Set up mean of education, work_history, skill level 
black_edu_mean = 4.6
hispanic_edu_mean = 4.9
asian_edu_mean = 3.2
white_edu_mean = 3.1

black_wh_mean = 4.9
hispanic_wh_mean = 4.4
asian_wh_mean = 3.6
white_wh_mean = 3.8

black_sl_mean = 4.7
hispanic_sl_mean = 4.9
asian_sl_mean = 3.2
white_sl_mean = 3.1


# Set up variance of education, work_history, skill level 
black_edu_var = 1.5
hispanic_edu_var = 1.5
asian_edu_var = 0.5
white_edu_var = 0.5

black_wh_var = 1.5
hispanic_wh_var = 1.5
asian_wh_var = 0.5
white_wh_var = 0.5

black_sl_var = 1.5
hispanic_sl_var = 1.5
asian_sl_var = 0.5
white_sl_var = 0.5

# Generating training data set 

# Initialize number of samples for training 
n1617 <- 5000 

# Generate race
race_probs <- c(0.09, 0.04, 0.57, 0.30)
race <- sample(c("Black", "Hispanic", "Asian", "White"), n1617, replace = TRUE, prob = race_probs)

# Initialize empty data frame
fix_data_pre_mino_high_1617 <- data.frame(race = race)

# Generate other variables
fix_data_pre_mino_high_1617 <- fix_data_pre_mino_high_1617 %>%
  group_by(race) %>%
  mutate(
    education = case_when(
      race == "Black" ~ rnorm(n(), black_edu_mean, sqrt(black_edu_var)),
      race == "Hispanic" ~ rnorm(n(), hispanic_edu_mean, sqrt(hispanic_edu_var)),
      race == "Asian" ~ rnorm(n(), asian_edu_mean, sqrt(asian_edu_var)),
      race == "White" ~ rnorm(n(), white_edu_mean, sqrt(white_edu_var))
    ),
    work_history = case_when(
      race == "Black" ~ rnorm(n(), black_wh_mean, sqrt(black_wh_var)),
      race == "Hispanic" ~ rnorm(n(), hispanic_wh_mean, sqrt(hispanic_wh_var)),
      race == "Asian" ~ rnorm(n(), asian_wh_mean, sqrt(asian_wh_var)),
      race == "White" ~ rnorm(n(), white_wh_mean, sqrt(white_wh_var))
    ),
    skill_level = case_when(
      race == "Black" ~ rnorm(n(), black_sl_mean, sqrt(black_sl_var)),
      race == "Hispanic" ~ rnorm(n(), hispanic_sl_mean, sqrt(hispanic_sl_var)),
      race == "Asian" ~ rnorm(n(), asian_sl_mean, sqrt(asian_sl_var)),
      race == "White" ~ rnorm(n(), white_sl_mean, sqrt(white_sl_var))
    )
  ) %>%
  ungroup()

## Test
#fix_data_pre_mino_high_1617 <- fix_data_pre_mino_high_1617 %>%
#  mutate(is_black = ifelse(race == "Black", 1, 0),
#         is_hispanic = ifelse(race == "Hispanic", 1, 0),
#         is_asian = ifelse(race == "Asian", 1, 0),
#         is_white = ifelse(race == "White", 1, 0))

# Calculate probabilities using a linear function
#fix_data_pre_mino_high_1617$hiring_prob <- exp(-( 0.25 * fix_data_pre_mino_high_1617$is_black +
#  0.28 * fix_data_pre_mino_high_1617$is_hispanic + 
#  0.12 * fix_data_pre_mino_high_1617$is_asian +
#  0.13 * fix_data_pre_mino_high_1617$is_white + 
#  0.07 * fix_data_pre_mino_high_1617$education + 
#  0.05 * fix_data_pre_mino_high_1617$work_history +
#  0.06 * fix_data_pre_mino_high_1617$skill_level))
## Test

## Test
# # Update the data 
# fix_data_pre_mino_high_1617 <- fix_data_pre_mino_high_1617 %>% 
#   mutate(
#     above_below_edu_mean = case_when(
#       education > mean(fix_data_pre_mino_high_1617$education) ~ 1, 
#       education <= mean(fix_data_pre_mino_high_1617$education) ~ -1 
#     ), 
#     above_below_work_history_mean = case_when(
#       work_history > mean(fix_data_pre_mino_high_1617$work_history) ~ 1, 
#       work_history <= mean(fix_data_pre_mino_high_1617$work_history) ~ -1 
#     ), 
#     above_below_skill_level_mean = case_when(
#       skill_level > mean(fix_data_pre_mino_high_1617$skill_level) ~ 1, 
#       skill_level <= mean(fix_data_pre_mino_high_1617$skill_level) ~ -1 
#     )
#   )
# 
# # Custom function to calculate hiring probability
# calculate_hiring_prob <- function(race, above_below_edu_mean, above_below_work_history_mean, above_below_skill_level_mean) {
#   base_prob <- 0.13
#   if (race == "Black") {
#     base_prob <- 0.23
#   } else if (race == "Hispanic") {
#     base_prob <- 0.20
#   } else if (race == "Asian") {
#     base_prob <- 0.14
#   }
# 
#   # Weight
#   prob <- base_prob + 
#     0.03 * above_below_edu_mean + 
#     0.02 * above_below_work_history_mean + 
#     0.01 * above_below_skill_level_mean
#   
#   # Make sure the probability is between 0 and 1
#   prob <- min(1, max(0, prob))
#   
#   return(prob)
# }
## Test

# Custom function to calculate hiring probability
calculate_hiring_prob <- function(race, education, work_history, skill_level) {
  base_prob <- 0.13
  if (race == "Black") {
    base_prob <- 0.23
  } else if (race == "Hispanic") {
    base_prob <- 0.20
  } else if (race == "Asian") {
    base_prob <- 0.14
  }
  
  # Weight
  z <- base_prob + 
    0.03 * education + 
    0.02 * work_history + 
    0.01 * skill_level
    
  prob <- 1/(1+exp(-z))
  # Some background knowledge: here we are simulating using the logistic regression model, therefore 
  # you see this specific function - Sigmoid function. Since the features coefficients are all positive 
  # the probability will be always greater than 0.5. Usually, when one apply glm, the logistic regression
  # will output predicted probability, and one assign outcome 1 if the probability of greater than 0.5, 0 otherwise. 
  # At the beginning I was trying to simulate exactly the real world hiring potential, which is around 0.1.
  # (I was trying to make the hiring potential of Black to be 0.2, Hispanic to be 0.25, Asian to be 0.14, White 0.16). 
  # But that is not align with the convention Sigmoid function. With some effort I can do that, but I think go with 
  # Sigmoid function - staying with hiring potential to be above 0.5. After all I think it is just scale. 
  # Later when we run Static Sl model, it will output predicted probabilities, but we will not assign predicted hiring
  # outcome, we are using the predicted probabilities as score for ranking the top 25. So it's still fine. 
  
  # Also be aware that - if I only changing the information related to Black/Hispanic, the hiring potential 
  # of Asian or White will not change. It is obvious when you think about it. The group's hiring potential 
  # does not depend on each other. 
  return(prob)
}

# Update the data with hiring outcome 
fix_data_pre_mino_high_1617 <- fix_data_pre_mino_high_1617 %>%
  rowwise() %>%
  mutate(hiring_potential = calculate_hiring_prob(race, 
                                             education, 
                                             work_history, 
                                             skill_level)) %>% 
  mutate(hiring_outcome = rbinom(1, 1, hiring_potential)) %>%
  ungroup()

# Check the hiring likelihood and diversity composition of each race in the data. Suppose X1 is Black,  
# We generate 4000 * 0.09 Black observations, where each is a realization of Ber(p).
# Then mean of these observations, by LLN, converges to p. Therefore gives us an
# approximate of p. 
data_hiring_likelihood <- function(data){
  result <- c(mean(as.numeric(data[data$race=="Black",]$hiring_outcome)), 
              mean(as.numeric(data[data$race=="Hispanic",]$hiring_outcome)),
              mean(as.numeric(data[data$race=="Asian",]$hiring_outcome)),
              mean(as.numeric(data[data$race=="White",]$hiring_outcome)))
  return(result)
}

data_diversity <- function(data){
  result <- c(nrow(data[data$race=="Black",])/nrow(data),
              nrow(data[data$race=="Hispanic",])/nrow(data),
              nrow(data[data$race=="Asian",])/nrow(data),
              nrow(data[data$race=="White",])/nrow(data))
  return(result)
}

print(data_hiring_likelihood(fix_data_pre_mino_high_1617))
print(data_diversity(fix_data_pre_mino_high_1617))

# Generate finalized version of training base line dataset 
fix_data_pre_mino_high_1617 <- fix_data_pre_mino_high_1617[,c(1:4, 6)]

#######################################################
# Generating training data set 

# Initialize number of samples for training 
# Possible settings: 45000 with 85 round of 500 candidates and -5; 45000 with 215 round of 200 candidates and -10;
# 430000 with 818 round of 500 candidates and -42 
n1819 <- 430000
race <- sample(c("Black", "Hispanic", "Asian", "White"), n1819, replace = TRUE, prob = race_probs)

# Initialize empty data frame
fix_data_pos_mino_high_1819 <- data.frame(race = race)

# Generate other variables
# Generate race
fix_data_pos_mino_high_1819 <- fix_data_pos_mino_high_1819 %>%
  group_by(race) %>%
  mutate(
    education = case_when(
      race == "Black" ~ rnorm(n(), black_edu_mean, sqrt(black_edu_var)),
      race == "Hispanic" ~ rnorm(n(), hispanic_edu_mean, sqrt(hispanic_edu_var)),
      race == "Asian" ~ rnorm(n(), asian_edu_mean, sqrt(asian_edu_var)),
      race == "White" ~ rnorm(n(), white_edu_mean, sqrt(white_edu_var))
    ),
    work_history = case_when(
      race == "Black" ~ rnorm(n(), black_wh_mean, sqrt(black_wh_var)),
      race == "Hispanic" ~ rnorm(n(), hispanic_wh_mean, sqrt(hispanic_wh_var)),
      race == "Asian" ~ rnorm(n(), asian_wh_mean, sqrt(asian_wh_var)),
      race == "White" ~ rnorm(n(), white_wh_mean, sqrt(white_wh_var))
    ),
    skill_level = case_when(
      race == "Black" ~ rnorm(n(), black_sl_mean, sqrt(black_sl_var)),
      race == "Hispanic" ~ rnorm(n(), hispanic_sl_mean, sqrt(hispanic_sl_var)),
      race == "Asian" ~ rnorm(n(), asian_sl_mean, sqrt(asian_sl_var)),
      race == "White" ~ rnorm(n(), white_sl_mean, sqrt(white_sl_var))
    )
  ) %>%
  ungroup()

## Test
# # Update the data 
# fix_data_pos_mino_high_1819 <- fix_data_pos_mino_high_1819 %>% 
#   mutate(
#     above_below_edu_mean = case_when(
#       education > mean(fix_data_pos_mino_high_1819$education) ~ 1, 
#       education <= mean(fix_data_pos_mino_high_1819$education) ~ -1 
#     ), 
#     above_below_work_history_mean = case_when(
#       work_history > mean(fix_data_pos_mino_high_1819$work_history) ~ 1, 
#       work_history <= mean(fix_data_pos_mino_high_1819$work_history) ~ -1 
#     ), 
#     above_below_skill_level_mean = case_when(
#       skill_level > mean(fix_data_pos_mino_high_1819$skill_level) ~ 1, 
#       skill_level <= mean(fix_data_pos_mino_high_1819$skill_level) ~ -1 
#     )
#   )
#
# # Custom function to calculate hiring probability
# calculate_hiring_prob <- function(race, above_below_edu_mean, above_below_work_history_mean, above_below_skill_level_mean) {
#   base_prob <- 0.13
#   if (race == "Black") {
#     base_prob <- 0.15
#   } else if (race == "Hispanic") {
#     base_prob <- 0.16
#   } else if (race == "Asian") {
#     base_prob <- 0.13
#   }
# 
#   # Weight
#   prob <- base_prob + 
#     0.03 * above_below_edu_mean + 
#     0.02 * above_below_work_history_mean + 
#     0.01 * above_below_skill_level_mean
#   
#   # Make sure the probability is between 0 and 1
#   prob <- min(1, max(0, prob))
#   
#   return(prob)
# }
## Test

# Update the data with hiring outcome using the same calculate_hiring_prob as training above
# This is because we are consider that the test and training data are drawn from the same distribution  
fix_data_pos_mino_high_1819 <- fix_data_pos_mino_high_1819 %>%
  rowwise() %>%
  mutate(hiring_potential = calculate_hiring_prob(race, 
                                                  education, 
                                                  work_history, 
                                                  skill_level)) %>% 
  mutate(hiring_outcome = rbinom(1, 1, hiring_potential)) %>%
  ungroup()

# Check the hiring likelihood and diversity composition of each race in the data. 
print(data_hiring_likelihood(fix_data_pos_mino_high_1819))
print(data_diversity(fix_data_pos_mino_high_1819))

# Generate first version of testing dataset 
fix_data_pos_mino_high_1819 <- fix_data_pos_mino_high_1819[,c(1:4, 6)]

# Prepare for later shuffle the data 
round_size <- 500
n_black <- floor(round_size * race_probs[1])
n_hispanic <- floor(round_size * race_probs[2])  
n_asian <- floor(round_size * race_probs[3])
n_white <- round_size - n_black - n_hispanic - n_asian 

# Create empty data frame to store the results
result_data <- data.frame()

# Number of rounds, -5 because we ealier using floor 
n_rounds <- n1819/round_size - 42  

for(i in 1:n_rounds) {
  sample_black <- fix_data_pos_mino_high_1819 %>% filter(race == "Black") %>% sample_n(n_black)
  print("picked black")
  sample_hispanic <- fix_data_pos_mino_high_1819 %>% filter(race == "Hispanic") %>% sample_n(n_hispanic)
  print("picked Hispanic")
  sample_asian <- fix_data_pos_mino_high_1819 %>% filter(race == "Asian") %>% sample_n(n_asian)
  print("picked asian")
  sample_white <- fix_data_pos_mino_high_1819 %>% filter(race == "White") %>% sample_n(n_white)
  print("picked white")
  balanced_sample <- bind_rows(sample_black, sample_hispanic, sample_asian, sample_white)
  result_data <- bind_rows(result_data, balanced_sample)
  fix_data_pos_mino_high_1819 <- fix_data_pos_mino_high_1819 %>% anti_join(balanced_sample, 
                                               by = join_by(race, education, work_history, skill_level, hiring_outcome))
}

fix_data_pos_mino_high_1819 <- result_data

# Check the hiring likelihood and diversity composition of each race in the data. 
print(data_hiring_likelihood(fix_data_pos_mino_high_1819))
print(data_diversity(fix_data_pos_mino_high_1819))


# SECTION TWO: MAIN TASKS FOR FIXED HIRING POTENTIAL SENARIO

#######################################################
# Task 1: Train logistic regression model. Output results regards to diversity selection and hiring yield. 

# Fit the logistic regression model
logistic_model <- glm(hiring_outcome ~ race + education + work_history + skill_level, 
                      data = fix_data_pre_mino_high_1617, 
                      family = binomial)
# Summary of the model
summary(logistic_model)

# Generate information about the proportion of each race in the recommended list as well as 
# the corresponding hiring yield, along with the general one. 

temp <- function(which_top_percent, table) {
  # Select the top % of rows
  top_percent <- table[1:floor(nrow(table) * which_top_percent), ]
  
  # Compute the proportion of each race in the recommended candidates for interview by the SL model 
  recmd_black_prop <- nrow(top_percent[top_percent$Race=="Black",]) / nrow(top_percent)
  recmd_hispanic_prop <- nrow(top_percent[top_percent$Race=="Hispanic",]) / nrow(top_percent)
  recmd_asian_prop <- nrow(top_percent[top_percent$Race=="Asian",]) / nrow(top_percent)
  recmd_white_prop <- nrow(top_percent[top_percent$Race=="White",]) / nrow(top_percent)
  
  # Create a data frame to store these values
  recmd_race_prop_hiring_table <- data.frame(
    Race = c("Black", "Hispanic", "Asian", "White"),
    Proportion = c(recmd_black_prop, recmd_hispanic_prop, recmd_asian_prop, recmd_white_prop)
  )
  
  # Compute the hiring yield in general and for each race 
  gen_hiring_yield <- nrow(top_percent[top_percent$Actual_Hiring_Outcome==1,]) / nrow(top_percent)
  black_hiring_yield <- nrow(top_percent[top_percent$Actual_Hiring_Outcome==1 & top_percent$Race=="Black",]) / nrow(top_percent)
  hispanic_hiring_yield <- nrow(top_percent[top_percent$Actual_Hiring_Outcome==1 & top_percent$Race=="Hispanic",]) / nrow(top_percent)
  asian_hiring_yield <- nrow(top_percent[top_percent$Actual_Hiring_Outcome==1 & top_percent$Race=="Asian",]) / nrow(top_percent)
  white_hiring_yield <- nrow(top_percent[top_percent$Actual_Hiring_Outcome==1 & top_percent$Race=="White",]) / nrow(top_percent)
  
  # Complete the above recmd_race_prop_hiring_table
  recmd_race_prop_hiring_table$HiringYield <- c(black_hiring_yield, hispanic_hiring_yield, asian_hiring_yield, white_hiring_yield)
  recmd_race_prop_hiring_table$GenHiring <- gen_hiring_yield
  
  return(recmd_race_prop_hiring_table)
}

sl_model_result <- function(test_data, sl_model, start_from = 1, round_size, which_top_percent) {
  
  # Prediction result on the testing data set 
  predicted_probs <- predict(sl_model, newdata = test_data[start_from:(start_from + round_size - 1),], type = "response")

  # Create a data frame that combines the race, predicted probabilities, and actual hiring outcomes
  result_table <- data.frame(
    Race = test_data[start_from:(start_from + round_size - 1),]$race,
    Predicted_Probabilities = predicted_probs,
    Actual_Hiring_Outcome = test_data[start_from:(start_from + round_size - 1),]$hiring_outcome
  )
  
  # Sort the result table by the Predicted_Probabilities in descending order
  sorted_table <- result_table[order(-result_table$Predicted_Probabilities), ]
  
  return(list(round_info = sorted_table, other_info = temp(which_top_percent, sorted_table)))
  
}

result <- sl_model_result(fix_data_pos_mino_high_1819, logistic_model, 1, round_size, which_top_percent)
print("Sorted Table")
print(result$round_info)
print("Racial Composition of Recommendation, Hiring Yield of Each Race, and General Hiring Yield")
print(result$other_info)


#######################################################
# Task 2: Train USL model. 

# Set up for initial training dataset, models, and record for results
usl_base_line_training_data <- fix_data_pre_mino_high_1617
usl_base_line_model <- logistic_model
usl_summary_table <- data.frame()

for (i in 1: n_rounds) {

  # Focus on specific round data
  start <- 1 + round_size * (i - 1)
  end <- start + round_size - 1
  current_round <- fix_data_pos_mino_high_1819[start:end, ]
  
  # Apply last round base line model to predict using the new round data 
  pred <- predict(usl_base_line_model, newdata = current_round, type = "response")
  current_round$pred_hiring_potential <- pred
  
  # Pick the top percent data for interview recommendation based on predicted hiring potential
  current_top_percent_round_data <- current_round %>%
    arrange(desc(pred_hiring_potential)) %>%
    slice(1:(floor(round_size * which_top_percent)))
  
  # Prepare the data for temp function, which later produce result table (diversity, hiring yield, general hiring yield)
  current_short_top_percent_round_data <- current_top_percent_round_data %>%
    select(-education, -work_history, -skill_level) %>%
    rename(
      Race = race,
      Actual_Hiring_Outcome = hiring_outcome,
      Predicted_Probabilities = pred_hiring_potential
    )
  
  # Gather results from the current round 
  current_summary_table_round <- temp(1, current_short_top_percent_round_data)
  # Record the round time 
  current_summary_table_round$RoundTime <- i
  # Combine with the results from the last rounds
  usl_summary_table <- rbind(usl_summary_table, current_summary_table_round)
  
  # Prepare for combining the recommendations to the previous training data
  current_rbind_top_percent_round_data <- current_top_percent_round_data %>%
    select(-pred_hiring_potential)
  
  # Augment previous training data to get new one 
  usl_base_line_training_data <- rbind(usl_base_line_training_data, current_rbind_top_percent_round_data)
  # Train with the new training data to get a new logistic model, which will be used for the next iteration
  usl_base_line_model <- glm(hiring_outcome ~ race + education + work_history + skill_level, 
                             data = usl_base_line_training_data, 
                             family = binomial)
  
  print(paste0("USL finishing processing the ", i, "th round data, which contains ", start," - ", end, " observations from fix_data_pos_mino_high_1819"))
  
}

# Help us exam the situation of the updated training dataset for USL
print(data_hiring_likelihood(usl_base_line_training_data))
print(data_diversity(usl_base_line_training_data))


# Function that generates plot about diversity selection, hiring yield, and general hiring yield using model's summary table 
plot_result <- function(summary_table, model_name) {
  
  # Plot with proportion as y-axis using summary_table
  p1 <- ggplot(summary_table, aes(x=RoundTime, y=Proportion, color=Race)) + 
    geom_line() + 
    scale_y_continuous(limits = c(0, 1)) + 
    labs(title=paste0(model_name," Proportion by Round Time"), y="Proportion", x="Round Time")
  
  # Plot with hiring yield as y-axis using summary_table
  p2 <- ggplot(summary_table, aes(x=RoundTime, y=HiringYield, color=Race)) + 
    geom_line() + 
    scale_y_continuous(limits = c(0, 1)) +
    labs(title=paste0(model_name, " Hiring Yield by Round Time"), y="Hiring Yield", x="Round Time")
  
  # Plot with hiring yield as y-axis using summary_table
  p3 <- ggplot(summary_table, aes(x=RoundTime, y=GenHiring)) + 
    geom_line(color="black") +  # You can choose a color you prefer
    scale_y_continuous(limits = c(0, 1)) +
    labs(title=paste0(model_name, " General Hiring by Round Time"), y="General Hiring", x="Round Time")
  
  plots <- list(p1, p2, p3)
  return(plots)
}

# Generate plot about diversity selection, hiring yield, and general hiring yield using the USL summary table 
usl_plots <- plot_result(usl_summary_table, "USL")
print(usl_plots[[1]])
print(usl_plots[[2]])
print(usl_plots[[3]])

#######################################################
# Task 3: Train UCB model. 

# Set up for initial training dataset, models, and record for results
ucb_base_line_training_data <- fix_data_pre_mino_high_1617
ucb_base_line_model <- logistic_model
ucb_summary_table <- data.frame()
ucb_uncertainty_table <- data.frame()

for (i in 1: n_rounds) {
  
  # Focus on specific round data
  start <- 1 + round_size * (i - 1)
  end <- start + round_size - 1
  current_round <- fix_data_pos_mino_high_1819[start:end, ]
  
  # Apply last round base line model to predict using the new round data 
  pred_result <- predict(ucb_base_line_model, newdata = current_round, type = "response", se.fit = TRUE)
  
  # Record the uncertainty 
  current_uncertainty_table <- current_round %>% 
    mutate(Uncertainty = pred_result$se.fit,
           RoundTime = i ) %>% 
    select(-education, -work_history, -skill_level, -hiring_outcome) %>% 
    rename(Race = race)
  ucb_uncertainty_table <- rbind(ucb_uncertainty_table, current_uncertainty_table)
  
  # Add the bonus 
  pred <- pred_result$fit + pred_result$se.fit
  current_round$pred_hiring_potential <- pred
  
  # Pick the top percent data for interview recommendation based on predicted hiring potential
  current_top_percent_round_data <- current_round %>%
    arrange(desc(pred_hiring_potential)) %>%
    slice(1:(floor(round_size * which_top_percent)))
  
  # Prepare the data for temp function, which later produce result table (diversity, hiring yield, general hiring yield)
  current_short_top_percent_round_data <- current_top_percent_round_data %>%
    select(-education, -work_history, -skill_level) %>%
    rename(
      Race = race,
      Actual_Hiring_Outcome = hiring_outcome,
      Predicted_Probabilities = pred_hiring_potential
    )
  
  # Gather results from the current round 
  current_summary_table_round <- temp(1, current_short_top_percent_round_data)
  # Record the round time 
  current_summary_table_round$RoundTime <- i
  # Combine with the results from the last rounds
  ucb_summary_table <- rbind(ucb_summary_table, current_summary_table_round)
  
  # Prepare for combining the recommendations to the previous training data
  current_rbind_top_percent_round_data <- current_top_percent_round_data %>%
    select(-pred_hiring_potential)
  
  # Augment previous training data to get new one 
  ucb_base_line_training_data <- rbind(ucb_base_line_training_data, current_rbind_top_percent_round_data)
  # Train with the new training data to get a new logistic model, which will be used for the next iteration
  ucb_base_line_model <- glm(hiring_outcome ~ race + education + work_history + skill_level, 
                             data = ucb_base_line_training_data, 
                             family = binomial)
  
  print(paste0("UCB finishing processing the ", i, "th round data, which contains ", start," - ", end, " observations from fix_data_pos_mino_high_1819"))
  
}

# Help us exam the situation of the updated training dataset for UCB
print(data_hiring_likelihood(ucb_base_line_training_data))
print(data_diversity(ucb_base_line_training_data))


# Generate plot about diversity selection, hiring yield, and general hiring yield using the UCB summary table 
ucb_plots <- plot_result(ucb_summary_table, "UCB")
print(ucb_plots[[1]])
print(ucb_plots[[2]])
print(ucb_plots[[3]])

# Compare USL and UCB plots, which are generated from their summary table 
grid.arrange(
  usl_plots[[1]], ucb_plots[[1]], 
  usl_plots[[2]], ucb_plots[[2]],
  usl_plots[[3]], ucb_plots[[3]],
  ncol=2
)

# Analysis how much uncertainty in each group from the testing data 
# Try 1 - Boxplot for visual inspection, but using all testing data at once, no separation of rounds 
all_race_uncertainty_plots <- ggplot(ucb_uncertainty_table, aes(x = Race, y = Uncertainty)) +
  geom_boxplot() +
  labs(title = "Distribution of Prediction Standard Error by Race")
print(all_race_uncertainty_plots)
# Try 2 - generate table of summary statistics (mean, median, sd) about the prediction standard error of each race each round 
# After generate this summary table, use this to generate the following plot for median. 
 
summary_ucb_uncertainty_table <- ucb_uncertainty_table %>%
  group_by(Race, RoundTime) %>%
  summarize(
    mean = mean(Uncertainty),
    median = median(Uncertainty),
    sd = sd(Uncertainty),
    .groups = "drop"
  ) 
race_uncertainty_plots <- ggplot(summary_ucb_uncertainty_table, aes(x = RoundTime, y = median, color = Race)) +
  geom_line() +
  labs(title = "Median Prediction Standard Error by Round",
       x = "Round Number",
       y = "Median Prediction Standard Error") +
  theme_minimal()
print(race_uncertainty_plots)

# Combine two summary_ucb_uncertainty_table, plot, and compare the uncertainty between different level of variance given
# to the minority group
# inc_var <- function(data1, data2, which_race, fst_ver, snd_ver) {
#   data1$var <- fst_ver
#   data2$var <- snd_ver
#   data1_race <- data1[data1$Race == which_race,]
#   data2_race <- data2[data2$Race == which_race,]
#   ttl_race <- rbind(data1_race, data2_race)
#   combined <- ggplot(ttl_race, aes(x = RoundTime, y = median, color = ver)) +
#     geom_line() +
#     labs(title = paste0("Uncertainty of Race ", which_race)) +
#     scale_y_continuous(limits = c(0, 0.04))
#   return(combined)
# }
# 
# black_plot_combined <- inc_var(first_var_summary_ucb_uncertainty_table, fifth_var_summary_ucb_uncertainty_table, "Black", 1, 2)
# hispanic_plot_combined <- inc_var(first_var_summary_ucb_uncertainty_table, fifth_var_summary_ucb_uncertainty_table, "Hispanic", 1, 2)
# asian_plot_combined <- inc_var(first_var_summary_ucb_uncertainty_table, fifth_var_summary_ucb_uncertainty_table, "Asian", 1, 2)
# white_plot_combined <- inc_var(first_var_summary_ucb_uncertainty_table, fifth_var_summary_ucb_uncertainty_table, "White", 1, 2)
# combined_plot <- black_plot_combined + hispanic_plot_combined + asian_plot_combined + white_plot_combined + plot_layout(guides = "collect")
# print(combined_plot)


