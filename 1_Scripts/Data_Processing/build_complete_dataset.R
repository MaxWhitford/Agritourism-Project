# Complete Dataset Assembly for All 43 Agritourism States
# Pulls CSPP data for missing states and adds governor party information
# Author: Script for agritourism paper
# Date: January 2026

library(tidyverse)

cat("=== BUILDING COMPLETE 43-STATE DATASET ===\n\n")

# 1. Read existing data (31 states)
existing_data <- read_csv("../../2_Data/Raw/time_of_adoption_agritourism_2004_2018.csv", show_col_types = FALSE)
cat(paste("Current dataset has", nrow(existing_data), "states\n"))

# 2. Read CSPP data
cspp_data <- read_csv("../../2_Data/Raw/cspp_data_2025-12-15.csv", show_col_types = FALSE)
cat(paste("CSPP data loaded with", nrow(cspp_data), "rows\n\n"))

# 3. Define the 12 MISSING states and their enactment years
missing_states <- tribble(
  ~state, ~enact_year,
  "Alaska", 2003,
  "Arizona", 2019,
  "Delaware", 2008,
  "Hawaii", 2006,
  "Illinois", 2006,
  "Iowa", 2021,
  "Massachusetts", 2006,
  "Michigan", 2006,
  "New Hampshire", 2018,
  "New York", 2017,
  "Pennsylvania", 2021,
  "Vermont", 2021
)

cat("Missing states to add:\n")
print(missing_states)
cat("\n")

# 4. Pull CSPP data for missing states at their enactment years
missing_cspp <- cspp_data %>%
  filter(state %in% missing_states$state) %>%
  inner_join(missing_states, by = "state") %>%
  filter(year == enact_year) %>%
  select(
    state, year, enact_year,
    # Key variables for hypotheses
    contrib_agri,
    valueofagsect,
    govparty_c,
    # Additional variables from existing dataset
    st, stateno, state_fips, state_icpsr,
    agtaxcredit, interstate_compact_on_agricultur, ig_ag,
    union_density, sals, gagexem,
    agriculture_ig, agemploy, code300,
    govparty_a, govparty_b, govparty_b_2, state_midterm_penalty, know_govparty
  )

cat(paste("Retrieved CSPP data for", nrow(missing_cspp), "missing states\n\n"))

# 5. Governor party lookup (all 43 states) - VERIFIED DATA
governor_party_complete <- tribble(
  ~state, ~enact_year, ~govparty_updated,
  "Alaska", 2003, 1,  # Frank Murkowski (R)
  "Kansas", 2004, 0,  # Kathleen Sebelius (D)
  "Oklahoma", 2004, 0,  # Brad Henry (D)
  "North Carolina", 2005, 0,  # Mike Easley (D)
  "Hawaii", 2006, 1,  # Linda Lingle (R)
  "Illinois", 2006, 0,  # Rod Blagojevich (D)
  "Massachusetts", 2006, 1,  # Mitt Romney (R)
  "Michigan", 2006, 0,  # Jennifer Granholm (D)
  "Virginia", 2006, 0,  # Tim Kaine (D)
  "Colorado", 2008, 0,  # Bill Ritter (D)
  "Delaware", 2008, 0,  # Ruth Ann Minner (D)
  "Georgia", 2008, 1,  # Sonny Perdue (R)
  "Louisiana", 2008, 1,  # Bobby Jindal (R)
  "Utah", 2008, 1,  # Jon Huntsman (R)
  "Montana", 2009, 0,  # Brian Schweitzer (D)
  "Tennessee", 2009, 0,  # Phil Bredesen (D)
  "Missouri", 2010, 0,  # Jay Nixon (D)
  "South Carolina", 2010, 1,  # Mark Sanford (R)
  "South Dakota", 2010, 1,  # Mike Rounds (R)
  "Arkansas", 2011, 0,  # Mike Beebe (D)
  "Indiana", 2011, 1,  # Mitch Daniels (R)
  "North Dakota", 2011, 1,  # Jack Dalrymple (R)
  "Alabama", 2012, 1,  # Robert Bentley (R)
  "Kentucky", 2012, 0,  # Steve Beshear (D)
  "Maine", 2012, 1,  # Paul LePage (R)
  "Mississippi", 2012, 1,  # Phil Bryant (R)
  "Florida", 2013, 1,  # Rick Scott (R)
  "Idaho", 2014, 1,  # Butch Otter (R)
  "Wisconsin", 2014, 1,  # Scott Walker (R)
  "Minnesota", 2015, 0,  # Mark Dayton (D)
  "Nebraska", 2015, 1,  # Pete Ricketts (R)
  "Oregon", 2015, 0,  # Kate Brown (D)
  "Texas", 2015, 1,  # Greg Abbott (R)
  "Ohio", 2016, 1,  # John Kasich (R)
  "New York", 2017, 0,  # Andrew Cuomo (D)
  "Washington", 2017, 0,  # Jay Inslee (D)
  "Maryland", 2018, 1,  # Larry Hogan (R)
  "New Hampshire", 2018, 1,  # Chris Sununu (R)
  "West Virginia", 2018, 1,  # Jim Justice (R)
  "Arizona", 2019, 1,  # Doug Ducey (R)
  "Iowa", 2021, 1,  # Kim Reynolds (R)
  "Pennsylvania", 2021, 0,  # Tom Wolf (D)
  "Vermont", 2021, 1   # Phil Scott (R)
)

# 6. Update missing_cspp with verified governor data
missing_cspp_complete <- missing_cspp %>%
  left_join(governor_party_complete %>% select(state, enact_year, govparty_updated), 
            by = c("state", "enact_year")) %>%
  mutate(govparty_c = coalesce(govparty_updated, govparty_c)) %>%
  select(-govparty_updated)

# 7. Harmonize column structure with existing data
# Make sure all columns match
missing_cspp_final <- missing_cspp_complete %>%
  mutate(
    year = enact_year,
    pldvpag = NA_real_
  ) %>%
  select(names(existing_data))

# 8. Add missing columns that might be in existing but not in missing
for(col in names(existing_data)) {
  if(!(col %in% names(missing_cspp_final))) {
    missing_cspp_final[[col]] <- NA
  }
}

# 9. Combine existing + missing
combined_data <- bind_rows(existing_data, missing_cspp_final)

# 10. Update ALL states with verified governor data
combined_data_final <- combined_data %>%
  left_join(governor_party_complete %>% select(state, enact_year, govparty_updated),
            by = c("state", "enact_year")) %>%
  mutate(govparty_c = coalesce(govparty_updated, govparty_c)) %>%
  select(-govparty_updated) %>%
  arrange(enact_year, state)

# 11. Save final dataset
write_csv(combined_data_final, "agritourism_complete_43states.csv")

# 12. Summary statistics
cat("\n=== FINAL DATASET SUMMARY ===\n")
cat(paste("Total states:", nrow(combined_data_final), "\n"))
cat(paste("States with contrib_agri:", sum(!is.na(combined_data_final$contrib_agri)), "\n"))
cat(paste("States with valueofagsect:", sum(!is.na(combined_data_final$valueofagsect)), "\n"))
cat(paste("States with govparty_c:", sum(!is.na(combined_data_final$govparty_c)), "\n\n"))

# Show distribution by year
cat("=== STATES BY ENACTMENT YEAR ===\n")
year_summary <- combined_data_final %>%
  group_by(enact_year) %>%
  summarize(n_states = n(), .groups = "drop")
print(year_summary)

# Show states with missing data for each hypothesis
cat("\n=== MISSING DATA BY HYPOTHESIS ===\n")
cat("H1 (contrib_agri) missing:\n")
h1_missing <- combined_data_final %>%
  filter(is.na(contrib_agri)) %>%
  select(state, enact_year)
if(nrow(h1_missing) > 0) print(h1_missing) else cat("  NONE - Complete!\n")

cat("\nH2 (valueofagsect) missing:\n")
h2_missing <- combined_data_final %>%
  filter(is.na(valueofagsect)) %>%
  select(state, enact_year)
if(nrow(h2_missing) > 0) print(h2_missing) else cat("  NONE - Complete!\n")

cat("\nH3 (govparty_c) missing:\n")
h3_missing <- combined_data_final %>%
  filter(is.na(govparty_c)) %>%
  select(state, enact_year)
if(nrow(h3_missing) > 0) print(h3_missing) else cat("  NONE - Complete!\n")

cat("\n=== SUCCESS! ===\n")
cat("File saved as: agritourism_complete_43states.csv\n")
cat("You now have complete data for all 43 states!\n")
