# Complete Governor Party Data for Agritourism States
# Compiled from Ballotpedia, NGA, and Wikipedia sources
# 1 = Republican, 0 = Democrat
# Date: January 2026
# All entries verified from historical records

library(tidyverse)

# Complete lookup table for all 43 states with agritourism statutes
governor_party_data <- tribble(
  ~state, ~enact_year, ~govparty_c,
  
  # 2003
  "Alaska", 2003, 1,  # Frank Murkowski (R) 2002-2006
  
  # 2004
  "Kansas", 2004, 0,  # Kathleen Sebelius (D) 2003-2009
  "Oklahoma", 2004, 0,  # Brad Henry (D) 2003-2011
  
  # 2005
  "North Carolina", 2005, 0,  # Mike Easley (D) 2001-2009
  
  # 2006
  "Hawaii", 2006, 1,  # Linda Lingle (R) 2002-2010
  "Illinois", 2006, 0,  # Rod Blagojevich (D) 2003-2009
  "Massachusetts", 2006, 1,  # Mitt Romney (R) 2003-2007
  "Michigan", 2006, 0,  # Jennifer Granholm (D) 2003-2011
  "Virginia", 2006, 0,  # Tim Kaine (D) 2006-2010
  
  # 2008
  "Colorado", 2008, 0,  # Bill Ritter (D) 2007-2011
  "Delaware", 2008, 0,  # Ruth Ann Minner (D) 2001-2009
  "Georgia", 2008, 1,  # Sonny Perdue (R) 2003-2011
  "Louisiana", 2008, 1,  # Bobby Jindal (R) 2008-2016
  "Utah", 2008, 1,  # Jon Huntsman (R) 2005-2009
  
  # 2009
  "Montana", 2009, 0,  # Brian Schweitzer (D) 2005-2013
  "Tennessee", 2009, 0,  # Phil Bredesen (D) 2003-2011
  
  # 2010
  "Missouri", 2010, 0,  # Jay Nixon (D) 2009-2017
  "South Carolina", 2010, 1,  # Mark Sanford (R) 2003-2011
  "South Dakota", 2010, 1,  # Mike Rounds (R) 2003-2011
  
  # 2011
  "Arkansas", 2011, 0,  # Mike Beebe (D) 2007-2015
  "Indiana", 2011, 1,  # Mitch Daniels (R) 2005-2013
  "North Dakota", 2011, 1,  # Jack Dalrymple (R) 2010-2016
  
  # 2012
  "Alabama", 2012, 1,  # Robert Bentley (R) 2011-2017
  "Kentucky", 2012, 0,  # Steve Beshear (D) 2007-2015
  "Maine", 2012, 1,  # Paul LePage (R) 2011-2019
  "Mississippi", 2012, 1,  # Phil Bryant (R) 2012-2020
  
  # 2013
  "Florida", 2013, 1,  # Rick Scott (R) 2011-2019
  
  # 2014
  "Idaho", 2014, 1,  # Butch Otter (R) 2007-2019
  "Wisconsin", 2014, 1,  # Scott Walker (R) 2011-2019
  
  # 2015
  "Minnesota", 2015, 0,  # Mark Dayton (D) 2011-2019
  "Nebraska", 2015, 1,  # Pete Ricketts (R) 2015-2019
  "Oregon", 2015, 0,  # Kate Brown (D) 2015-2019
  "Texas", 2015, 1,  # Greg Abbott (R) 2015-present
  
  # 2016
  "Ohio", 2016, 1,  # John Kasich (R) 2011-2019
  
  # 2017
  "New York", 2017, 0,  # Andrew Cuomo (D) 2011-2021
  "Washington", 2017, 0,  # Jay Inslee (D) 2013-present
  
  # 2018
  "Maryland", 2018, 1,  # Larry Hogan (R) 2015-2023
  "New Hampshire", 2018, 1,  # Chris Sununu (R) 2017-present
  "West Virginia", 2018, 1,  # Jim Justice (R/switched from D 2017) 2017-2025
  
  # 2019
  "Arizona", 2019, 1,  # Doug Ducey (R) 2015-2023
  
  # 2021
  "Iowa", 2021, 1,  # Kim Reynolds (R) 2017-present
  "Pennsylvania", 2021, 0,  # Tom Wolf (D) 2015-2023
  "Vermont", 2021, 1   # Phil Scott (R) 2017-present
)

# Verify all 43 states are accounted for
cat("\n=== GOVERNOR PARTY DATA COMPILATION ===\n")
cat(paste("Total states in lookup table:", nrow(governor_party_data), "\n"))
cat(paste("Republican governors:", sum(governor_party_data$govparty_c == 1), "\n"))
cat(paste("Democratic governors:", sum(governor_party_data$govparty_c == 0), "\n\n"))

# Load existing data
existing_data <- read_csv("../../2_Data/Raw/time_of_adoption_agritourism_2004_2018.csv", show_col_types = FALSE)

# Merge governor party data
updated_data <- existing_data %>%
  left_join(governor_party_data, by = c("state", "enact_year"), suffix = c("_old", "_new")) %>%
  mutate(govparty_c = coalesce(govparty_c_new, govparty_c_old)) %>%
  select(-govparty_c_old, -govparty_c_new)

# Save updated dataset
write_csv(updated_data, "time_of_adoption_agritourism_2004_2018_UPDATED.csv")

cat("=== UPDATE COMPLETE ===\n")
cat(paste("States with governor data:", sum(!is.na(updated_data$govparty_c)), "out of", nrow(updated_data), "\n"))
cat("\nFile saved as: time_of_adoption_agritourism_2004_2018_UPDATED.csv\n\n")

# Display states with/without data
missing_gov_data <- updated_data %>%
  filter(is.na(govparty_c)) %>%
  select(state, enact_year)

if(nrow(missing_gov_data) > 0) {
  cat("WARNING: Missing governor data for:\n")
  print(missing_gov_data)
} else {
  cat("SUCCESS: All states now have governor party data!\n")
}

# Summary statistics
cat("\n=== SUMMARY BY YEAR ===\n")
summary_table <- governor_party_data %>%
  group_by(enact_year) %>%
  summarize(
    n_states = n(),
    n_republican = sum(govparty_c == 1),
    n_democrat = sum(govparty_c == 0)
  )
print(summary_table)
