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

# Separate features and target
##TODO test it without season. select(-c(win_percent, season)))
X_train <- as.matrix(train_data %>% select(-win_percent))
y_train <- train_data$win_percent
X_validation <- as.matrix(validation_data %>% select(-win_percent))
y_validation <- validation_data$win_percent
X_test <- as.matrix(test_data %>% select(-win_percent))
y_test <- test_data$win_percent


# Ridge Regression (using glmnet)
# Perform Ridge Regression with cross-validation to find the best lambda
#TODO is this correct? nfold 5 or 10?
ridge_model <- cv.glmnet(
  x = X_train,
  y = y_train,
  alpha = 0,  # Alpha = 0 for Ridge Regression
  nfolds = 5, # 5-fold cross-validation
  standardize = TRUE
)

#TODO  Best lambda value
best_lambda <- ridge_model$lambda.min
cat("Best lambda:", best_lambda, "\n")

# Plot cross-validation results
plot(ridge_model)

#TODO what is parameter alpha
# Train Ridge Regression model using the best lambda
final_ridge_model <- glmnet(
  x = X_train,
  y = y_train,
  alpha = 0,
  lambda = best_lambda
)

# Predictions and Performance Evaluation
# On validation set
predictions_validation <- predict(final_ridge_model, s = best_lambda, newx = X_validation)
validation_rmse <- sqrt(mean((y_validation - predictions_validation)^2))
cat("Validation RMSE:", validation_rmse, "\n")

# On test set
predictions_test <- predict(final_ridge_model, s = best_lambda, newx = X_test)
test_rmse <- sqrt(mean((y_test - predictions_test)^2))
cat("Test RMSE:", test_rmse, "\n")

# Visualizing Ridge Regression Coefficients
ridge_coefficients <- as.data.frame(as.matrix(coef(final_ridge_model)))
ridge_coefficients <- ridge_coefficients[-1, , drop = FALSE] # Remove intercept
names(ridge_coefficients) <- "Coefficient"

ggplot(ridge_coefficients, aes(x = reorder(rownames(ridge_coefficients), Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Ridge Regression Coefficients", x = "Feature", y = "Coefficient")

# Hyperparameter Tuning
# Try a range of lambda values and compare performance
lambda_values <- 10^seq(-4, 2, length = 100) # Generate a sequence of lambda values
ridge_tuning <- glmnet(
  x = X_train,
  y = y_train,
  alpha = 0,
  lambda = lambda_values
)

# Evaluate RMSE for each lambda on validation data
validation_rmse_values <- sapply(lambda_values, function(l) {
  predictions <- predict(ridge_tuning, s = l, newx = X_validation)
  sqrt(mean((y_validation - predictions)^2))
})

# Find the best lambda based on validation RMSE
optimal_lambda <- lambda_values[which.min(validation_rmse_values)]
cat("Optimal lambda from validation set:", optimal_lambda, "\n")

# Visualize RMSE vs Lambda
plot(lambda_values, validation_rmse_values, type = "b", log = "x", col = "blue",
     xlab = "Lambda (log scale)", ylab = "Validation RMSE",
     main = "Ridge Regression Hyperparameter Tuning")

colSums(is.na(team_data))

# Train the final model with the optimal lambda
final_model <- glmnet(X_train, y_train, alpha = 0, lambda = optimal_lambda)

# Test set performance
final_predictions <- predict(final_model, s = optimal_lambda, newx = X_test)
final_rmse <- sqrt(mean((y_test - final_predictions)^2))
cat("Final Test RMSE with optimal lambda:", final_rmse, "\n")

# Create a data frame for plotting
results_df <- data.frame(
  Actual = y_test,
  Predicted = as.vector(final_predictions)
)

# Plot Actual vs Predicted Win Percentage
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