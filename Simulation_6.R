# Simulation for Hiring as Exploring by Li et al. 
# Author : Yiran Jia 
# Date: Sep 11th, 2023 
# Version: Sample imbalance with incorrect model. Here we simulate based on the assumption that the imbalance occur
# in sampling, i.e. we have limited access or can only collect limited observations, and use logistic model to 
# simulate the hiring outcome from the features education, work history, and skill level. Later we compare the 
# performance among Static SL model (logistic model), USL, and UCB. Since this imbalance happens on the sampling 
# process, we will not grantee that the racial composition remains same in each round. 

# By incorrect model, I meant here we simulate the distribution of the test dataset different
# from the training dataset. We are not doing changing over time. The only thing changing 
# over time is the proportion of each racial group. 

# Here we model the way that in the training dataset, White and Asian appear to performance better 
# than Black and White, while in fact the truth is that Black and White perform better than 
# Asian and White. We remain how test round was generated, but now we change the data 
# generation process of training - focus on simulate in the way that the dominate group has 
# better hiring potential than the minority. 

# Different from Simulation_5, here we model the hiring potential more subtle - they are not piecewise constant 
# but random variable. 

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
set.seed(1234567)

# SECTION ONE: DATA GENERATION FOR FIXED HIRING POTENTIAL SENARIO
# Description: In scenarios with fixed hiring potential, we set the underlying likelihood of 
# hiring for Black and Hispanic candidates HIGHER than for White and Asian candidates. 

#######################################################

# Transform normal and beta 
trans <- function(mu, sigma){
  m <- mu
  s <- sigma
  alpha_beta_sum <- m * (1 - m) / s^2 - 1
  alpha <- m * alpha_beta_sum
  beta <- (1 - m) * alpha_beta_sum
  return(c(alpha, beta))
}

# Set up which top percent 
which_top_percent = 0.25

# Set up training mean of education, work_history, skill level 
train_black_edu_mean = 3.3
train_hispanic_edu_mean = 3.2
train_asian_edu_mean = 4.6
train_white_edu_mean = 4.9

train_black_wh_mean = 3.5
train_hispanic_wh_mean = 3.6
train_asian_wh_mean = 4.8
train_white_wh_mean = 4.7

train_black_sl_mean = 3.2
train_hispanic_sl_mean = 3.1
train_asian_sl_mean = 4.3
train_white_sl_mean = 4.4


# Set up training variance of education, work_history, skill level 
train_black_edu_var = 2.5
train_hispanic_edu_var = 2.5
train_asian_edu_var = 2
train_white_edu_var = 2

train_black_wh_var = 2.5
train_hispanic_wh_var = 2.5
train_asian_wh_var = 2
train_white_wh_var = 2

train_black_sl_var = 2.5
train_hispanic_sl_var = 2.5
train_asian_sl_var = 2
train_white_sl_var = 2

# Set up testing mean of education, work_history, skill level 
test_black_edu_mean = 4.9
test_hispanic_edu_mean = 4.8
test_asian_edu_mean = 3.8
test_white_edu_mean = 3.7

test_black_wh_mean = 4.7
test_hispanic_wh_mean = 4.8
test_asian_wh_mean = 3.6
test_white_wh_mean = 3.9

test_black_sl_mean = 4.4
test_hispanic_sl_mean = 4.35
test_asian_sl_mean = 3.7
test_white_sl_mean = 3.5


# Set up testing variance of education, work_history, skill level 
test_black_edu_var = 2.5
test_hispanic_edu_var = 2.5
test_asian_edu_var = 2
test_white_edu_var = 2

test_black_wh_var = 2.5
test_hispanic_wh_var = 2.5
test_asian_wh_var = 2
test_white_wh_var = 2

test_black_sl_var = 2.5
test_hispanic_sl_var = 2.5
test_asian_sl_var = 2
test_white_sl_var = 2

# Generating training data set 

# Initialize number of samples for training 
n1617 <- 5000 

# Generate race
train_race_probs <- c(0.09, 0.04, 0.57, 0.30)
race <- sample(c("Black", "Hispanic", "Asian", "White"), n1617, replace = TRUE, prob = train_race_probs)

# Initialize empty data frame
fix_data_pre_mino_high_1617 <- data.frame(race = race)

# Generate other variables
fix_data_pre_mino_high_1617 <- fix_data_pre_mino_high_1617 %>%
  group_by(race) %>%
  mutate(
    education = case_when(
      race == "Black" ~ rnorm(n(), train_black_edu_mean, sqrt(train_black_edu_var)),
      race == "Hispanic" ~ rnorm(n(), train_hispanic_edu_mean, sqrt(train_hispanic_edu_var)),
      race == "Asian" ~ rnorm(n(), train_asian_edu_mean, sqrt(train_asian_edu_var)),
      race == "White" ~ rnorm(n(), train_white_edu_mean, sqrt(train_white_edu_var))
    ),
    work_history = case_when(
      race == "Black" ~ rnorm(n(), train_black_wh_mean, sqrt(train_black_wh_var)),
      race == "Hispanic" ~ rnorm(n(), train_hispanic_wh_mean, sqrt(train_hispanic_wh_var)),
      race == "Asian" ~ rnorm(n(), train_asian_wh_mean, sqrt(train_asian_wh_var)),
      race == "White" ~ rnorm(n(), train_white_wh_mean, sqrt(train_white_wh_var))
    ),
    skill_level = case_when(
      race == "Black" ~ rnorm(n(), train_black_sl_mean, sqrt(train_black_sl_var)),
      race == "Hispanic" ~ rnorm(n(), train_hispanic_sl_mean, sqrt(train_hispanic_sl_var)),
      race == "Asian" ~ rnorm(n(), train_asian_sl_mean, sqrt(train_asian_sl_var)),
      race == "White" ~ rnorm(n(), train_white_sl_mean, sqrt(train_white_sl_var))
    )
  ) %>%
  ungroup()

# Custom function to calculate hiring probability
calculate_hiring_prob <- function(race, education, work_history, skill_level) {
  if (race == "Black") {
    base_prob <- rbeta(1, 13.96, 102.37)
  } else if (race == "Hispanic") {
    base_prob <- rbeta(1, 16.21, 108.46)
  } else if (race == "Asian") {
    base_prob <- rbeta(1, 101.6, 340.15)
  } else if (race == "White") {
    base_prob <- rbeta(1, 79.8, 319.2)
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



# # Generating population dataset for training dataset
# n1617 <- 5000
# pop_n1617 <- n1617 * 5
# 
# pop_race_probs <- c(0.25, 0.25, 0.25, 0.25)
# pop_race <- sample(c("Black", "Hispanic", "Asian", "White"), pop_n1617, replace = TRUE, prob = pop_race_probs)
# 
# # Initialize empty data frame
# pop_fix_data_pre_mino_high_1617 <- data.frame(race = pop_race)
# 
# # Generate other variables
# pop_fix_data_pre_mino_high_1617 <- pop_fix_data_pre_mino_high_1617 %>%
#   group_by(race) %>%
#   mutate(
#     education = case_when(
#       race == "Black" ~ rnorm(n(), train_black_edu_mean, sqrt(train_black_edu_var)),
#       race == "Hispanic" ~ rnorm(n(), train_hispanic_edu_mean, sqrt(train_hispanic_edu_var)),
#       race == "Asian" ~ rnorm(n(), train_asian_edu_mean, sqrt(train_asian_edu_var)),
#       race == "White" ~ rnorm(n(), train_white_edu_mean, sqrt(train_white_edu_var))
#     ),
#     work_history = case_when(
#       race == "Black" ~ rnorm(n(), train_black_wh_mean, sqrt(train_black_wh_var)),
#       race == "Hispanic" ~ rnorm(n(), train_hispanic_wh_mean, sqrt(train_hispanic_wh_var)),
#       race == "Asian" ~ rnorm(n(), train_asian_wh_mean, sqrt(train_asian_wh_var)),
#       race == "White" ~ rnorm(n(), train_white_wh_mean, sqrt(train_white_wh_var))
#     ),
#     skill_level = case_when(
#       race == "Black" ~ rnorm(n(), train_black_sl_mean, sqrt(train_black_sl_var)),
#       race == "Hispanic" ~ rnorm(n(), train_hispanic_sl_mean, sqrt(train_hispanic_sl_var)),
#       race == "Asian" ~ rnorm(n(), train_asian_sl_mean, sqrt(train_asian_sl_var)),
#       race == "White" ~ rnorm(n(), train_white_sl_mean, sqrt(train_white_sl_var))
#     )
#   ) %>%
#   ungroup()
# 
# # Custom function to calculate hiring probability
# train_calculate_hiring_prob <- function(race, education, work_history, skill_level) {
#   if (race == "Black") {
#     base_prob <- 0.25
#   } else if (race == "Hispanic") {
#     base_prob <- 0.26
#   } else if (race == "Asian") {
#     base_prob <- 0.14
#   } else if(race == "White") {
#     base_prob <- 0.10
#   }
#   
#   # Weight
#   z <- base_prob + 
#     0.03 * education + 
#     0.02 * work_history + 
#     0.01 * skill_level
#   
#   prob <- 1/(1+exp(-z))
#   # Some background knowledge: here we are simulating using the logistic regression model, therefore 
#   # you see this specific function - Sigmoid function. Since the features coefficients are all positive 
#   # the probability will be always greater than 0.5. Usually, when one apply glm, the logistic regression
#   # will output predicted probability, and one assign outcome 1 if the probability of greater than 0.5, 0 otherwise. 
#   # At the beginning I was trying to simulate exactly the real world hiring potential, which is around 0.1.
#   # (I was trying to make the hiring potential of Black to be 0.2, Hispanic to be 0.25, Asian to be 0.14, White 0.16). 
#   # But that is not align with the convention Sigmoid function. With some effort I can do that, but I think go with 
#   # Sigmoid function - staying with hiring potential to be above 0.5. After all I think it is just scale. 
#   # Later when we run Static Sl model, it will output predicted probabilities, but we will not assign predicted hiring
#   # outcome, we are using the predicted probabilities as score for ranking the top 25. So it's still fine. 
#   
#   # Also be aware that - if I only changing the information related to Black/Hispanic, the hiring potential 
#   # of Asian or White will not change. It is obvious when you think about it. The group's hiring potential 
#   # does not depend on each other. 
#   return(prob)
# }
# 
# # Update the data with hiring outcome 
# pop_fix_data_pre_mino_high_1617 <- pop_fix_data_pre_mino_high_1617 %>%
#   rowwise() %>%
#   mutate(hiring_potential = train_calculate_hiring_prob(race, 
#                                                   education, 
#                                                   work_history, 
#                                                   skill_level)) %>% 
#   mutate(hiring_outcome = rbinom(1, 1, hiring_potential)) %>%
#   ungroup()
# 
# # Check the hiring likelihood and diversity composition of each race in the data. Suppose X1 is Black,  
# # We generate 4000 * 0.09 Black observations, where each is a realization of Ber(p).
# # Then mean of these observations, by LLN, converges to p. Therefore gives us an
# # approximate of p. 
# data_hiring_likelihood <- function(data){
#   result <- c(mean(as.numeric(data[data$race=="Black",]$hiring_outcome)), 
#               mean(as.numeric(data[data$race=="Hispanic",]$hiring_outcome)),
#               mean(as.numeric(data[data$race=="Asian",]$hiring_outcome)),
#               mean(as.numeric(data[data$race=="White",]$hiring_outcome)))
#   return(result)
# }
# 
# data_diversity <- function(data){
#   result <- c(nrow(data[data$race=="Black",])/nrow(data),
#               nrow(data[data$race=="Hispanic",])/nrow(data),
#               nrow(data[data$race=="Asian",])/nrow(data),
#               nrow(data[data$race=="White",])/nrow(data))
#   return(result)
# }
# 
# # Check the hiring likelihood and diversity composition of each race in the data. 
# print(data_hiring_likelihood(pop_fix_data_pre_mino_high_1617))
# print(data_diversity(pop_fix_data_pre_mino_high_1617))
# 
# # Generate finalized version of training base line dataset 
# pop_fix_data_pre_mino_high_1617 <- pop_fix_data_pre_mino_high_1617[,c(1:4, 6)]
# 
# # Prepare for training dataset 
# train_race_probs <- c(0.09, 0.04, 0.57, 0.30)
# n_black <- floor(n1617 * train_race_probs[1])
# n_hispanic <- floor(n1617 * train_race_probs[2])  
# n_asian <- floor(n1617 * train_race_probs[3])
# n_white <- n1617 - n_black - n_hispanic - n_asian
# 
# # Initialize empty data frame
# fix_data_pre_mino_high_1617 <- data.frame()
# 
# sample_black <- pop_fix_data_pre_mino_high_1617 %>% filter(race == "Black") %>% sample_n(n_black)
# sample_hispanic <- pop_fix_data_pre_mino_high_1617 %>% filter(race == "Hispanic") %>% sample_n(n_hispanic)
# sample_asian <- pop_fix_data_pre_mino_high_1617 %>% filter(race == "Asian") %>% sample_n(n_asian)
# sample_white <- pop_fix_data_pre_mino_high_1617 %>% filter(race == "White") %>% sample_n(n_white)
# fix_data_pre_mino_high_1617 <- bind_rows(sample_black, sample_hispanic, sample_asian, sample_white)
# 
# # Check the hiring likelihood and diversity composition of each race in the data. 
# print(data_hiring_likelihood(fix_data_pre_mino_high_1617))
# print(data_diversity(fix_data_pre_mino_high_1617))

#######################################################
# Generating testing data set 
# The point of the paper is that in the (history) training dataset, the observations on the minority is limited, therefore 
# supervised model has a certain belief; however, that is not true on the testing dataset, i.e. there are more 
# minority applied therefore the observation on the minority is relative not limited anymore, and that is how 
# their intuition that UCB may come in help. Based on this, wewill simulate in a way that the proportion of minority 
# is increasing over rounds rather than stay fixed as the training. 

n1819 <- 53000 
pop_n1819 <- n1819 * 5
pop_race_probs <- c(0.25, 0.25, 0.25, 0.25)
pop_race <- sample(c("Black", "Hispanic", "Asian", "White"), pop_n1819, replace = TRUE, prob = pop_race_probs)

# Initialize empty data frame
pop_fix_data_pos_mino_high_1819 <- data.frame(race = pop_race)

# Generate other variables
pop_fix_data_pos_mino_high_1819 <- pop_fix_data_pos_mino_high_1819 %>%
  group_by(race) %>%
  mutate(
    education = case_when(
      race == "Black" ~ rnorm(n(), test_black_edu_mean, sqrt(test_black_edu_var)),
      race == "Hispanic" ~ rnorm(n(), test_hispanic_edu_mean, sqrt(test_hispanic_edu_var)),
      race == "Asian" ~ rnorm(n(), test_asian_edu_mean, sqrt(test_asian_edu_var)),
      race == "White" ~ rnorm(n(), test_white_edu_mean, sqrt(test_white_edu_var))
    ),
    work_history = case_when(
      race == "Black" ~ rnorm(n(), test_black_wh_mean, sqrt(test_black_wh_var)),
      race == "Hispanic" ~ rnorm(n(), test_hispanic_wh_mean, sqrt(test_hispanic_wh_var)),
      race == "Asian" ~ rnorm(n(), test_asian_wh_mean, sqrt(test_asian_wh_var)),
      race == "White" ~ rnorm(n(), test_white_wh_mean, sqrt(test_white_wh_var))
    ),
    skill_level = case_when(
      race == "Black" ~ rnorm(n(), test_black_sl_mean, sqrt(test_black_sl_var)),
      race == "Hispanic" ~ rnorm(n(), test_hispanic_sl_mean, sqrt(test_hispanic_sl_var)),
      race == "Asian" ~ rnorm(n(), test_asian_sl_mean, sqrt(test_asian_sl_var)),
      race == "White" ~ rnorm(n(), test_white_sl_mean, sqrt(test_white_sl_var))
    )
  ) %>%
  ungroup()

# Custom function to calculate hiring probability
test_calculate_hiring_prob <- function(race, education, work_history, skill_level) {
  if (race == "Black") {
    base_prob <- rbeta(1, 101.6025, 340.1475)
  } else if (race == "Hispanic") {
    base_prob <- rbeta(1, 109.2, 345.8)
  } else if (race == "Asian") {
    base_prob <- rbeta(1, 16.2, 108.5)
  } else if(race == "White") {
    base_prob <- rbeta(1, 14.0, 102.4)
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
pop_fix_data_pos_mino_high_1819 <- pop_fix_data_pos_mino_high_1819 %>%
  rowwise() %>%
  mutate(hiring_potential = test_calculate_hiring_prob(race, 
                                                  education, 
                                                  work_history, 
                                                  skill_level)) %>% 
  mutate(hiring_outcome = rbinom(1, 1, hiring_potential)) %>%
  ungroup()

# Check the hiring likelihood and diversity composition of each race in the data.
print(data_hiring_likelihood(pop_fix_data_pos_mino_high_1819))
print(data_diversity(pop_fix_data_pos_mino_high_1819))

# Generate finalized version of training base line dataset 
pop_fix_data_pos_mino_high_1819 <- pop_fix_data_pos_mino_high_1819[,c(1:4, 6)]

# Write a function of racial composition over rounds with the proportion of minority group increasing over time, and 
# the proportion of dominate group decreasing over time. 
round_racial_comp <- function(which_round) {
  
  convg_speed <- 0.0001
  
  # Starting racial composition
  if(which_round == 1) {
    prop <- train_race_probs
  } else {
    # For subsequent rounds, use the proportions from the previous round
    prop <- round_racial_comp(which_round - 1)
  }
  
  # The desired equal proportion
  desired_prop <- 0.25
  
  # Calculate adjustments based on the difference from the desired proportion
  black_adjustment <- (desired_prop - prop[1]) * convg_speed
  hispanic_adjustment <- (desired_prop - prop[2]) * convg_speed
  asian_adjustment <- (desired_prop - prop[3]) * convg_speed
  
  black_prop <- prop[1] + black_adjustment
  hispanic_prop <- prop[2] + hispanic_adjustment
  asian_prop <- prop[3] + asian_adjustment
  
  # Since white proportion is derived from the others, no need to calculate an adjustment
  white_prop <- 1 - black_prop - hispanic_prop - asian_prop
  
  prop <- c(black_prop, hispanic_prop, asian_prop, white_prop)
  
  if (all(prop >= 0 & prop <= 1)) {
    return(prop)
  } else {
    stop("Error: Not all components of the vector are between 0 and 1.")
  }
}


# Next create testing dataset come in rounds of applications where racial composition is changing over rounds. 
round_size = 300

sample_from_dataset <- function(df) {
  iteration <- 1
  samples_list <- list() # To store the sampled dataframes
  
  while(TRUE) {
    racial_counts <- round(round_size * round_racial_comp(iteration))
    # Adjust for the white group to make total samples equal to round_size
    racial_counts[4] <- racial_counts[4] + (round_size - sum(racial_counts))
    
    # Create datasets for each racial group
    black_data <- df[df$race == "Black", ]
    hispanic_data <- df[df$race == "Hispanic", ]
    asian_data <- df[df$race == "Asian", ]
    white_data <- df[df$race == "White", ]
    
    # Check if there's enough data to sample
    if (nrow(black_data) < racial_counts[1] || 
        nrow(hispanic_data) < racial_counts[2] ||
        nrow(asian_data) < racial_counts[3] ||
        nrow(white_data) < racial_counts[4]) {
      break
    }
    
    # Randomly sample from each dataset
    black_sample <- black_data[sample(nrow(black_data), racial_counts[1]), ]
    hispanic_sample <- hispanic_data[sample(nrow(hispanic_data), racial_counts[2]), ]
    asian_sample <- asian_data[sample(nrow(asian_data), racial_counts[3]), ]
    white_sample <- white_data[sample(nrow(white_data), racial_counts[4]), ]
    
    # Combine the samples into a single dataframe
    sampled_df <- rbind(black_sample, hispanic_sample, asian_sample, white_sample)
    samples_list[[iteration]] <- sampled_df
    
    # Remove the sampled data from the main dataframe
    df <- df[!rownames(df) %in% rownames(sampled_df), ]
    
    iteration <- iteration + 1
    print(paste0("This is the ", iteration, "th round of test data generation."))
  }
  
  return(samples_list)
}

list_fix_data_pos_mino_high_1819 <- sample_from_dataset(pop_fix_data_pos_mino_high_1819)

# Check the hiring likelihood and diversity composition of each race in the data.
print(data_hiring_likelihood(list_fix_data_pos_mino_high_1819[[1]]))
print(data_diversity(list_fix_data_pos_mino_high_1819[[1]]))


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

sl_model_result <- function(test_data, sl_model, which_top_percent) {
  # Prediction result on the testing data set 
  predicted_probs <- predict(sl_model, newdata = test_data, type = "response")
  
  # Create a data frame that combines the race, predicted probabilities, and actual hiring outcomes
  result_table <- data.frame(
    Race = test_data$race,
    Predicted_Probabilities = predicted_probs,
    Actual_Hiring_Outcome = test_data$hiring_outcome
  )
  
  # Sort the result table by the Predicted_Probabilities in descending order
  sorted_table <- result_table[order(-result_table$Predicted_Probabilities), ]
  
  return(list(round_info = sorted_table, other_info = temp(which_top_percent, sorted_table)))
}

# Pick random round and evaluate 
# one_test <- sample(1:length(list_fix_data_pos_mino_high_1819), 1)
# result <- sl_model_result(list_fix_data_pos_mino_high_1819[[one_test]], logistic_model, which_top_percent)
# print("Sorted Table")
# print(result$round_info)
# print("Racial Composition of Recommendation, Hiring Yield of Each Race, and General Hiring Yield")
# print(result$other_info)

# Generate Static SL model summary table
stasl_summary_table <- data.frame()
for (i in 1: length(list_fix_data_pos_mino_high_1819)) {
  result <- sl_model_result(list_fix_data_pos_mino_high_1819[[i]], logistic_model, which_top_percent)
  summary <- result$other_info
  summary$RoundTime <- i 
  stasl_summary_table <- rbind(stasl_summary_table, summary)
}

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
stasl_plots <- plot_result(stasl_summary_table, "Static SL")
print(stasl_plots[[1]])
print(stasl_plots[[2]])
print(stasl_plots[[3]])


#######################################################
# Task 2: Train USL model. 

# Set up for initial training dataset, models, and record for results
usl_base_line_training_data <- fix_data_pre_mino_high_1617
usl_base_line_model <- logistic_model
usl_summary_table <- data.frame()

for (i in 1: length(list_fix_data_pos_mino_high_1819)) {
  
  current_round <- list_fix_data_pos_mino_high_1819[[i]]
  
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
  
  print(paste0("USL finishing processing the ", i, "th round data from list_fix_data_pos_mino_high_1819"))
  
}

# Help us exam the situation of the updated training dataset for USL
print(data_hiring_likelihood(usl_base_line_training_data))
print(data_diversity(usl_base_line_training_data))


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

for (i in 1: length(list_fix_data_pos_mino_high_1819)) {
  
  current_round <- list_fix_data_pos_mino_high_1819[[i]]
  
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
  
  print(paste0("UCB finishing processing the ", i, "th round data, which contains observations from list_fix_data_pos_mino_high_1819"))
  
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
  stasl_plots[[1]], usl_plots[[1]], ucb_plots[[1]], 
  stasl_plots[[2]], usl_plots[[2]], ucb_plots[[2]],
  stasl_plots[[3]], usl_plots[[3]], ucb_plots[[3]],
  ncol=3
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









