library(tidyverse)
library(corrplot)


################################################################
####################### Data Preparation #######################
################################################################

# Import CSV file
team_data <- read.csv("Team Summaries.csv")

# Assuming team_data has columns 'w' for wins and 'l' for losses
team_data <- team_data %>%
  mutate(win_percent = w / (w + l))

# deselect all old entries (below 1973)
team_data <- team_data %>% 
  filter(season >= 1972)


# delete all 'League average' entries
team_data <- team_data %>%
  filter(team != "League Average")


# add missing values


# ======== replace NA in column 'x3p_ar' by averaging the values of ABA of each year =======

average_x3p_ar <- aggregate(x3p_ar ~ season, data = team_data, FUN = mean, na.rm = TRUE)

team_data_copy <- merge(team_data, average_x3p_ar, by = "season", all.x = TRUE, suffixes = c("", "_mean"))

team_data_copy$x3p_ar <- ifelse(is.na(team_data_copy$x3p_ar), team_data_copy$x3p_ar_mean, team_data_copy$x3p_ar)

team_data_copy <- subset(team_data_copy, select = -x3p_ar_mean)

avg_1976_1980 <- mean(team_data$x3p_ar[team_data$season %in% c(1976, 1980)], na.rm = TRUE)

team_data_copy$x3p_ar <- ifelse(is.na(team_data_copy$x3p_ar), avg_1976_1980, team_data_copy$x3p_ar)
# => reduce the number of NA from 153 to 66!
# 66 values are in the years where ABA was merged with NBA and no single value got stored during these years


# ======== same for 'tov_percent' ===================

average_tov_percent <- aggregate(tov_percent ~ season, data = team_data_copy, FUN = mean, na.rm = TRUE)

team_data_copy <- merge(team_data_copy, average_tov_percent, by = "season", all.x = TRUE, suffixes = c("", "_mean"))

team_data_copy$tov_percent <- ifelse(is.na(team_data_copy$tov_percent), team_data_copy$tov_percent_mean, team_data_copy$tov_percent)

team_data_copy <- subset(team_data_copy, select = -tov_percent_mean)

# => reduce the number of NA from 34 to 0!


# ======== same for 'opp_tov_percent' ===================

average_opp_tov_percent <- aggregate(opp_tov_percent ~ season, data = team_data_copy, FUN = mean, na.rm = TRUE)

team_data_copy <- merge(team_data_copy, average_opp_tov_percent, by = "season", all.x = TRUE, suffixes = c("", "_mean"))

team_data_copy$opp_tov_percent <- ifelse(is.na(team_data_copy$opp_tov_percent), team_data_copy$opp_tov_percent_mean, team_data_copy$opp_tov_percent)

team_data_copy <- subset(team_data_copy, select = -opp_tov_percent_mean)

# => reduce the number of NA from 34 to 0!




# ======== same for 'orb_percent' ===================

average_orb_percent <- aggregate(orb_percent ~ season, data = team_data_copy, FUN = mean, na.rm = TRUE)

team_data_copy <- merge(team_data_copy, average_orb_percent, by = "season", all.x = TRUE, suffixes = c("", "_mean"))

team_data_copy$orb_percent <- ifelse(is.na(team_data_copy$orb_percent), team_data_copy$orb_percent_mean, team_data_copy$orb_percent)

team_data_copy <- subset(team_data_copy, select = -orb_percent_mean)

# => reduce the number of NA from 34 to 0!


# ======== same for 'drb_percent' ===================

average_drb_percent <- aggregate(drb_percent ~ season, data = team_data_copy, FUN = mean, na.rm = TRUE)

team_data_copy <- merge(team_data_copy, average_drb_percent, by = "season", all.x = TRUE, suffixes = c("", "_mean"))

team_data_copy$drb_percent <- ifelse(is.na(team_data_copy$drb_percent), team_data_copy$drb_percent_mean, team_data_copy$drb_percent)

team_data_copy <- subset(team_data_copy, select = -drb_percent_mean)

# Exclude columns

team_data <- team_data_copy %>% select(-w, -l, -pw, -pl, -attend, -attend_g) %>% select_if(is.numeric)


################################################################
######################## Data Modeling #########################
################################################################
library(dplyr)
library(ggplot2)
library(caret)
library(glmnet)      # For Ridge Regression
library(ISLR)
library(tidyr)

#MultiLinear Regression and Decision Tree Regression?

# Step 1: Randomly split data into train/validation/test sets (stratified by win_percent)
set.seed(123) # For reproducibility
train_idx <- createDataPartition(team_data$win_percent, p = 0.7, list = FALSE) # 70% for training
train_data <- team_data[train_idx, ]
temp_data <- team_data[-train_idx, ]
val_idx <- createDataPartition(temp_data$win_percent, p = 0.5, list = FALSE) # 50% of remaining for validation
validation_data <- temp_data[val_idx, ]
test_data <- temp_data[-val_idx, ]


# Define the features and target variable
x_train <- as.matrix(train_data %>% select(-c(win_percent, season)))  # Exclude target and season column
y_train <- train_data$win_percent

# Use cross-validation to find the best lambda
#TODO is this correct?
cv_model <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 10)  # alpha = 0 for Ridge Regression

# Best lambda from cross-validation
best_lambda <- cv_model$lambda.min
cat("Best Lambda from CV: ", best_lambda, "\n")

# Plot cross-validation results
plot(cv_model)

# Now, train the Ridge model using the best lambda
ridge_model <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)

# Summary of the Ridge model
print(ridge_model)

# Prepare the validation data
x_validation <- as.matrix(validation_data %>% select(-c(win_percent, season)))
y_validation <- validation_data$win_percent

# Make predictions on the validation set
ridge_predictions <- predict(ridge_model, s = best_lambda, newx = x_validation)

# Evaluate the model performance
mse <- mean((ridge_predictions - y_validation)^2)
cat("Mean Squared Error on Validation Set: ", mse, "\n")

# Calculate R-squared
rss <- sum((ridge_predictions - y_validation)^2)
tss <- sum((y_validation - mean(y_validation))^2)
rsq <- 1 - (rss / tss)
cat("R-squared on Validation Set: ", rsq, "\n")

# Plot Actual vs Predicted for Validation Set
results_df <- data.frame(Actual = y_validation, Predicted = as.vector(ridge_predictions))
ggplot(results_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Win Percentage (Ridge Regression)",
    x = "Actual Win Percentage",
    y = "Predicted Win Percentage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
