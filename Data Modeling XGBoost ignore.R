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
library(scorecard)
library(xgboost)
library(ggplot2)
library(caret)
#MultiLinear Regression and Decision Tree Regression?

# Step 1: Randomly split data into train/validation/test sets (stratified by win_percent)
set.seed(123) # For reproducibility
train_idx <- createDataPartition(team_data$win_percent, p = 0.7, list = FALSE) # 70% for training
train_data <- team_data[train_idx, ]
temp_data <- team_data[-train_idx, ]

val_idx <- createDataPartition(temp_data$win_percent, p = 0.5, list = FALSE) # 50% of remaining for validation
val_data <- temp_data[val_idx, ]
test_data <- temp_data[-val_idx, ]

# Step 2: Convert data to DMatrix format for XGBoost
train_matrix <- xgb.DMatrix(data = as.matrix(train_data %>% select(-c(win_percent, season))),
                            label = train_data$win_percent)
val_matrix <- xgb.DMatrix(data = as.matrix(val_data %>% select(-c(win_percent, season))),
                          label = val_data$win_percent)
test_matrix <- xgb.DMatrix(data = as.matrix(test_data %>% select(-c(win_percent, season))),
                           label = test_data$win_percent)


# Step 3: Hyper-parameter tuning for learning_rate
learning_rates <- c(0.001, 0.005, 0.01, 0.05, 0.1, 0.2, 0.25)
results <- list()

for (lr in learning_rates) {
  params <- list(
    objective = "reg:squarederror",
    eta = lr, # Learning rate
    max_depth = 6, # Reasonable depth
    nthread = 2 # Number of threads
  )
  
  # Train the model
  model <- xgb.train(
    params = params,
    data = train_matrix,
    nrounds = 100,
    watchlist = list(train = train_matrix, val = val_matrix),
    print_every_n = 10,
    early_stopping_rounds = 10
  )
  
  # Predict on validation set
  val_predictions <- predict(model, val_matrix)
  mse <- mean((val_data$win_percent - val_predictions)^2) # Calculate Mean Squared Error
  
  results[[paste("lr", lr, sep = "_")]] <- mse
}

# Step 4: Select the best learning rate
# lr 0.05 has the lowest mean squared error which is why we chose it as our hyperparameter.
best_lr <- names(which.min(unlist(results)))
best_lr_value <- as.numeric(sub("lr_", "", best_lr))
print(paste("Best learning rate:", best_lr_value))

################################################################
###################### Data Evaluation##########################
################################################################

#steps gehören umgekehrt vielleicht?

# Step 5: Retrain model with the best learning rate using train + validation data
combined_train_val <- bind_rows(train_data, val_data)
combined_matrix <- xgb.DMatrix(data = as.matrix(combined_train_val %>% select(-c(win_percent, season))),
                               label = combined_train_val$win_percent)

final_model <- xgb.train(
  params = list(
    objective = "reg:squarederror",
    eta = best_lr_value,
    max_depth = 6,
    nthread = 2
  ),
  data = combined_matrix,
  nrounds = 100
)

# Step 6: Evaluate on the test set
test_predictions <- predict(final_model, test_matrix)
test_mse <- mean((test_data$win_percent - test_predictions)^2)
test_r2 <- 1 - sum((test_data$win_percent - test_predictions)^2) / 
  sum((test_data$win_percent - mean(test_data$win_percent))^2)

cat("Test MSE:", test_mse, "\n")
cat("Test R²:", test_r2, "\n")

# Step 7: Visualize results
# Plot predicted vs actual win percentage
test_data <- test_data %>%
  mutate(predicted_win_percent = test_predictions)

ggplot(test_data, aes(x = win_percent, y = predicted_win_percent)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual Win Percentage",
       x = "Actual Win Percentage",
       y = "Predicted Win Percentage") +
  theme_minimal()
