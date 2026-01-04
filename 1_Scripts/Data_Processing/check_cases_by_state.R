# Quick check of cases by state
library(tidyverse)

cases <- read_csv("../../2_Data/Raw/agritourism_cases_scraped.csv")

# Our 43 states with statutes
statute_states <- c("Alabama", "Arkansas", "Colorado", "Florida", "Georgia", "Hawaii", 
                    "Idaho", "Illinois", "Indiana", "Kansas", "Kentucky", "Louisiana",
                    "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                    "Mississippi", "Missouri", "Montana", "Nebraska", "North Carolina",
                    "North Dakota", "Ohio", "Oklahoma", "Oregon", "South Carolina",
                    "South Dakota", "Tennessee", "Texas", "Utah", "Virginia", 
                    "Washington", "West Virginia", "Wisconsin", "Alaska", "Arizona",
                    "Delaware", "Iowa", "New Hampshire", "New York", "Pennsylvania", "Vermont")

# Count cases per state (excluding Federal cases)
state_cases <- cases %>%
  filter(state != "Federal") %>%
  count(state, name = "n_cases") %>%
  arrange(desc(n_cases))

print("All states with cases:")
print(state_cases, n = 50)

# Check which of our 43 states have cases
our_states_cases <- state_cases %>%
  filter(state %in% statute_states)

print("\nOur 43 statute states with cases:")
print(our_states_cases, n = 50)

# Which of our 43 states have NO cases?
missing <- setdiff(statute_states, our_states_cases$state)
cat("\nStatute states with NO cases in database:", length(missing), "\n")
print(missing)
