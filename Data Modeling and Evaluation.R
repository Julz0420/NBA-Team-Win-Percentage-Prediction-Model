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

team_data <- team_data_copy %>% select(-w, -l, -pw, -pl, -attend, -attend_g, -season, -srs, -mov, -n_rtg, -sos) %>% select_if(is.numeric)


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

# Step 1: Randomly split data into train/validation/test sets stratified by win_percent
set.seed(123) # For reproducibility
train_idx <- createDataPartition(team_data$win_percent, p = 0.7, list = FALSE) # 70% for training
train_data <- team_data[train_idx, ]
temp_data <- team_data[-train_idx, ]
val_idx <- createDataPartition(temp_data$win_percent, p = 0.5, list = FALSE) # 50% of remaining for validation
validation_data <- temp_data[val_idx, ]
test_data <- temp_data[-val_idx, ]

# Separate features and target
X_train <- as.matrix(train_data %>% select(-win_percent))
y_train <- train_data$win_percent
X_validation <- as.matrix(validation_data %>% select(-win_percent))
y_validation <- validation_data$win_percent
X_test <- as.matrix(test_data %>% select(-win_percent))
y_test <- test_data$win_percent

# Train Ridge Regression models for each lambda and evaluate on the validation set

# Setting the range of lambda values
lambda_seq <- 10^seq(3, -3, by = -.1)

# Initialize a vector to store Mean Squared Error (MSE) for each lambda
validation_mse_list <- numeric(length(lambda_seq))

for (i in seq_along(lambda_seq)) {
  # Perform Ridge Regression
  ridge_model <- glmnet(
    x = X_train,
    y = y_train,
    alpha = 0,  # Alpha = 0 for Ridge Regression
    lambda = lambda_seq[i],
    standardize = TRUE, # standardizes variables
  )
  # Predict on the validation set
  validation_predictions <- predict(ridge_model, newx = X_validation)
  
  # Calculate Mean Squared Error (MSE) on the validation set
  validation_mse_list[i] <- mean((y_validation - validation_predictions)^2)
}

best_lambda_idx <- which.min(validation_mse_list)
best_lambda <- lambda_seq[best_lambda_idx]
best_validation_mse <- validation_mse_list[best_lambda_idx]
# Best lambda identified based on validation set: 0.002511886
# Validation MSE for best lambda: 0.00157269

# Plot for MSE
plot(
  x = log10(lambda_seq),
  y = validation_mse_list,
  type = "b",
  pch = 19,
  col = "blue",
  xlab = "log10(Lambda)",
  ylab = "Validation MSE",
  main = "Validation MSE for Different Lambda Values"
)
abline(v = log10(best_lambda), col = "red", lty = 2)

################################################################
######################## Data Evaluations ######################
################################################################


#https://www.r-bloggers.com/2020/05/simple-guide-to-ridge-regression-in-r/

final_ridge_model <- glmnet(
  x = X_train,
  y = y_train,
  alpha = 0,
  lambda = best_lambda,
  standardize = TRUE
)

# Test set performance
final_predictions <- predict(final_ridge_model, s = best_lambda, newx = X_test)
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



######################################################
# Re-train the model with identical hyper-parameters #
######################################################

x_train_validation <- rbind(X_train, X_validation)
y_train_validation <- c(y_train, y_validation)

ridge_model_retrained <- glmnet(
  x = x_train_validation,
  y = y_train_validation,
  alpha = 0,
  lambda = best_lambda,
  standardize = TRUE
)

retrained_predictions <- predict(ridge_model_retrained, newx = X_test, s = best_lambda)
rmse_retrained <- sqrt(mean((y_test - retrained_predictions)^2))
cat("Final Test RMSE with optimal lambda (retrained):", rmse_retrained, "\n")
# ein stück schlechter? ganz mininmal (0.0001 schlechter)



#############################################################################
# base-line performance of a trivial acceptor/rejecter or random classifier #
#############################################################################

trivial_acceptor_accuracy <- 0.5
trivial_rejector_accuracy <- 1 - trivial_acceptor_accuracy

cat(paste("trivial acceptor:", trivial_acceptor_accuracy, "\ntrivial rejecter:", trivial_rejector_accuracy))

# calculate rmse of trivial acceptor

rmse_trivial_acceptor <- sqrt(mean((team_data$win_percent - trivial_acceptor_accuracy)^2, na.rm = TRUE))

cat(paste("rmse of trivial acceptor:", rmse_trivial_acceptor))
cat(paste("difference to rmse of our model:", rmse_retrained - rmse_trivial_acceptor))

# compare performances according to the per-class level
# confusion matrix

team_data_classes <- cut(validation_data$win_percent, 
                         breaks = c(-Inf, 0.4, 0.6, Inf),
                         labels = c("low", "medium", "high"))

predicted_values_classes <- cut(predict(final_ridge_model, s = best_lambda, newx = X_validation),
                                breaks = c(-Inf, 0.4, 0.6, Inf),
                                labels = c("low", "medium", "high"))

conf_matrix <- table(team_data_classes, predicted_values_classes)

print(conf_matrix)

######################################################
#                 Feature Importance                 #
######################################################

# Extract feature names
feature_names <- colnames(X_train)

# Extract coefficients for the best lambda from the final Ridge Regression model
ridge_coefficients <- coef(final_ridge_model, s = best_lambda)

# Convert coefficients to a data frame for easier manipulation and visualization
coefficients_df <- data.frame(
  Feature = rownames(ridge_coefficients)[-1], # Exclude the intercept
  Coefficient = as.vector(ridge_coefficients[-1])
)

# Calculate the absolute values of coefficients for feature importance
coefficients_df <- coefficients_df %>%
  mutate(Importance = abs(Coefficient)) %>%
  arrange(desc(Importance))

# Plot Feature Importance
ggplot(coefficients_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(
    title = "Feature Importance (Ridge Regression)",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
