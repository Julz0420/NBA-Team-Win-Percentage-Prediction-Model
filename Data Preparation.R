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



