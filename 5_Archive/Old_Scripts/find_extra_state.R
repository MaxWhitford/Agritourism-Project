# Find the extra state
library(tidyverse)

# Get all .txt files in statute directory
statute_files <- list.files("State Agritourism Laws", pattern = "\\.txt$")
states_in_folder <- str_remove(statute_files, "\\.txt$")

cat("States in folder (N =", length(states_in_folder), "):\n")
print(sort(states_in_folder))
cat("\n")

# Get states from regression data
regression_data <- read_csv("agritourism_regression_data_FINAL.csv", show_col_types = FALSE)
states_in_data <- regression_data$state

cat("States in regression data (N =", length(states_in_data), "):\n")
print(sort(states_in_data))
cat("\n")

# Find which state is in folder but NOT in regression data
extra_state <- setdiff(states_in_folder, states_in_data)
cat("State(s) in folder but NOT in regression data:\n")
print(extra_state)
cat("\n")

# Find which state is in regression data but NOT in folder
missing_state <- setdiff(states_in_data, states_in_folder)
cat("State(s) in regression data but NOT in folder:\n")
print(missing_state)
cat("\n")
