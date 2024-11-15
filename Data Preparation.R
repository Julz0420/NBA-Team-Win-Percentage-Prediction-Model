# Load necessary libraries
library(tidyverse)
library(corrplot)

# Import CSV file
team_data <- read.csv("Team Summaries.csv")

# Assuming team_data has columns 'w' for wins and 'l' for losses
team_data <- team_data %>%
  mutate(win_percent = w / (w + l))

# Display the first few rows of the data
#head(team_data)

# Data summary to get an overview
#summary(team_data)


# deselect all old entries (below 1973)
team_data <- team_data %>% 
  filter(season >= 1972)


# delete all 'League average' entries
team_data <- team_data %>%
  filter(team != "League Average")

# look at the numeric variables
numeric_cols <- team_data %>%select_if(is.numeric)

numeric_cols_excl <- team_data %>% select(-attend, -attend_g, -season) %>% select_if(is.numeric)

numeric_data <- gather(numeric_cols_excl, key = "Variable", value = "Value")



ggplot(numeric_data, aes(x = Variable, y = Value)) +
  geom_boxplot() +
  coord_flip() + # Rotate for readability
  labs(x = "Variable", y = "Value")


# add missing values


# ======== replace NA in column 'x3p_ar' by averaging the values of ABA of each year =======

average_x3p_ar <- aggregate(x3p_ar ~ season, data = team_data, FUN = mean, na.rm = TRUE)

team_data_copy <- merge(team_data, average_x3p_ar, by = "season", all.x = TRUE, suffixes = c("", "_mean"))

team_data_copy$x3p_ar <- ifelse(is.na(team_data_copy$x3p_ar), team_data_copy$x3p_ar_mean, team_data_copy$x3p_ar)

team_data_copy <- subset(team_data_copy, select = -x3p_ar_mean)


# => reduce the number of NA from 153 to 66!!!!


# ======== same for 'tov_percent' ===================

average_tov_percent <- aggregate(tov_percent ~ season, data = team_data_copy, FUN = mean, na.rm = TRUE)

team_data_copy <- merge(team_data_copy, average_tov_percent, by = "season", all.x = TRUE, suffixes = c("", "_mean"))

team_data_copy$tov_percent <- ifelse(is.na(team_data_copy$tov_percent), team_data_copy$tov_percent_mean, team_data_copy$tov_percent)

team_data_copy <- subset(team_data_copy, select = -tov_percent_mean)

# => reduce the number of NA from 34 to 0!!!!
# => do we need that? what about the other columns, they also only have 34 NA values (except ATTEND and ATTEND_G)????


# ======== same for 'tov_percent' ===================


#TODO: WHAT IS ATTEND AND ATTENG_G??????
