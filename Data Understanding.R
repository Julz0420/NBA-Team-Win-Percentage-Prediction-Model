# Load necessary libraries
library(tidyverse)
library(corrplot)
library(tidyr)

# Import CSV file
team_data <- read.csv("Team Summaries.csv")

# Assuming team_data has columns 'w' for wins and 'l' for losses
team_data <- team_data %>%
  mutate(win_percent = w / (w + l))

# Display the first few rows of the data
head(team_data)

# Data summary to get an overview
summary(team_data)

# Correlation matrix
# Select only numeric columns for correlation analysis
numeric_cols <- team_data %>%select_if(is.numeric)

# Select numeric columns, excluding large attributes
numeric_cols_excl <- team_data %>% select(-w, -l, -pw, -pl, -season, -attend, -attend_g) %>% select_if(is.numeric)

# Reshape data for plotting
numeric_data <- gather(numeric_cols_excl, key = "Variable", value = "Value")

# Plot boxplots for each numeric column
ggplot(numeric_data, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  coord_flip() + # Rotate for readability
  labs(title = "Boxplot of Numeric Variables (Excluding Attendance, Season, Wins, Losses)", x = "Variable", y = "Value")

# Calculate the correlation matrix
cor_matrix <- cor(numeric_cols, use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, 
         mar=c(0,0,1,0))

# Visual exploration: scatter plot of offensive rating vs. defensive rating
ggplot(team_data, aes(x = o_rtg, y = d_rtg, color = playoffs)) +
  geom_point() +
  labs(title = "Offensive vs Defensive Rating by Playoffs Status",
       x = "Offensive Rating (o_rtg)",
       y = "Defensive Rating (d_rtg)")

# Hypothesis test example: Are playoff teams older on average?
playoff_age <- team_data %>%
  filter(playoffs == 1) %>%
  pull(age)

non_playoff_age <- team_data %>%
  filter(playoffs == 0) %>%
  pull(age)

t.test(playoff_age, non_playoff_age, alternative = "two.sided")


# Save the summary table to a CSV
summary_table <- summary(team_data)
write.csv(summary_table, "Team_Summaries_Summary.csv")

library(ggplot2)
library(dplyr)

# Calculate the average age for each season
age_by_season <- team_data %>%
  group_by(season) %>%
  summarise(avg_age = mean(age, na.rm = TRUE))

# Plot the average age over time
ggplot(age_by_season, aes(x = season, y = avg_age)) +
  geom_line(color = "green") +
  geom_point(color = "darkgreen") +
  labs(title = "Average Team Age Over Seasons",
       x = "Season",
       y = "Average Age") +
  theme_minimal()


# Calculate average offensive and defensive ratings for each season
ratings_by_season <- team_data %>%
  group_by(season) %>%
  summarise(avg_o_rtg = mean(o_rtg, na.rm = TRUE),
            avg_d_rtg = mean(d_rtg, na.rm = TRUE))

# Plot offensive and defensive ratings over time
ggplot(ratings_by_season, aes(x = season)) +
  geom_line(aes(y = avg_o_rtg, color = "Offensive Rating")) +
  geom_line(aes(y = avg_d_rtg, color = "Defensive Rating")) +
  labs(title = "Offensive and Defensive Ratings Over Seasons",
       x = "Season",
       y = "Rating",
       color = "Metric") +
  theme_minimal()

# Select attributes to visualize over time
attributes <- team_data %>%
  group_by(season) %>%
  summarise(avg_age = mean(age, na.rm = TRUE),
            avg_o_rtg = mean(o_rtg, na.rm = TRUE),
            avg_d_rtg = mean(d_rtg, na.rm = TRUE),
            avg_pace = mean(pace, na.rm = TRUE),
            avg_ts_percent = mean(ts_percent, na.rm = TRUE),
            tov_percent = mean(tov_percent, na.rm = TRUE))

# Reshape data for faceting
attributes_long <- attributes %>%
  pivot_longer(cols = -season, names_to = "Attribute", values_to = "Value")

# Plot with faceting
ggplot(attributes_long, aes(x = season, y = Value)) +
  geom_line() +
  facet_wrap(~ Attribute, scales = "free_y") +
  labs(x = "Season",
       y = "Value") +
  theme_minimal()


library(tidyr)
# Convert the data to long format
team_data_long <- team_data %>%
  pivot_longer(cols = -c(season, lg, team, abbreviation, playoffs, arena),  # Exclude categorical columns
               names_to = "attribute", values_to = "value") %>%
  drop_na(value)  # Remove NAs for plotting

# 1. Histogram for each attribute
ggplot(team_data_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~ attribute, scales = "free") +
  theme_minimal() +
  labs(x = "Value", y = "Frequency")

#######################TO show if data is approximately linear ##############
####Test for suitability of Linear Regression ################
# Load necessary libraries
library(ggplot2)
library(corrplot)

# Plot each feature against win_percent to visually check linearity
# Selecting a subset of features for visualization
features_to_plot <- c("age", "srs", "o_rtg", "pace", "f_tr")

# Create a list of plots
plots <- lapply(features_to_plot, function(f) {
  ggplot(team_data, aes_string(x = f, y = "win_percent")) + 
    geom_point() + 
    geom_smooth(method = "lm", col = "red") + 
    theme_minimal() +
    ggtitle(paste("Scatter plot of", f, "vs Win Percent"))
})

# Display all plots together
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2)

# Now, fit a linear regression model and plot residuals
lm_model <- lm(win_percent ~ age + srs + o_rtg + pace + f_tr, data = team_data)

# Residual vs Fitted plot
ggplot(data.frame(fitted = fitted(lm_model), residuals = residuals(lm_model)), 
       aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  ggtitle("Residuals vs Fitted values (Linear Regression Model)")
## indicates linearity it seems like. 
# residual doesnt change with fitted values, and the spread is even
#https://stats.stackexchange.com/questions/76226/interpreting-the-residuals-vs-fitted-values-plot-for-verifying-the-assumptions

# QQ plot of residuals to check for normality
qqnorm(residuals(lm_model))
qqline(residuals(lm_model), col = "red")

# Histogram of residuals
ggplot(data.frame(residuals = residuals(lm_model)), aes(x = residuals)) + 
  geom_histogram(binwidth = 0.02, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() + 
  ggtitle("Histogram of Residuals")

####Test for suitability of Linear Regression ################