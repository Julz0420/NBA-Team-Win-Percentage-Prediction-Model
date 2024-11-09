# Load necessary libraries
library(tidyverse)
library(corrplot)

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
         title = "Correlation Matrix of Numeric Variables", mar=c(0,0,1,0))

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
