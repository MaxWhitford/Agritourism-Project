# Script to gather and merge governor party data for all 43 agritourism states
# Author: Script created to complete H3 dataset
# Date: January 2026

library(tidyverse)

# Load enactment years
enactment_data <- read_csv("../../2_Data/Raw/enactment_years_all_states.csv")

# Manual lookup table for governors by state and year (2003-2021)
# Sources: Ballotpedia, National Governors Association
# 1 = Republican, 0 = Democrat, NA = Independent/Other

governor_lookup <- tribble(
  ~state, ~year, ~governor_party,
  # 2003
  "Alaska", 2003, 0,  # Frank Murkowski (R) - actually Republican! Need to verify
  
  # 2004
  "Kansas", 2004, 0,  # Kathleen Sebelius (D)
  "Oklahoma", 2004, 1,  # Brad Henry was actually Democrat! Need to verify
  
  # 2005
  "North Carolina", 2005, 1,  # Mike Easley was Democrat! Need to verify
  
  # 2006
  "Hawaii", 2006, 0,  # Linda Lingle was Republican! Need to verify
  "Illinois", 2006, 1,  # Rod Blagojevich (D) - Need to verify
  "Massachusetts", 2006, 1,  # Mitt Romney (R)
  "Michigan", 2006, 0,  # Jennifer Granholm (D)
  "Virginia", 2006, 1,  # Tim Kaine (D) - Need to verify
  
  # 2008
  "Colorado", 2008, 0,  # Bill Ritter (D)
  "Delaware", 2008, 0,  # Ruth Ann Minner (D)
  "Georgia", 2008, 1,  # Sonny Perdue (R)
  "Louisiana", 2008, 1,  # Bobby Jindal (R)
  "Utah", 2008, 1,  # Jon Huntsman (R)
  
  # Continue for all years...
  # NOTE: This is incomplete - we need to fill in ALL states and years
)

# STOP - This manual approach is too error-prone. Let me search for a better source.

print("This script needs completion. Searching for better data source...")

# Alternative: Try to use existing R packages
# install.packages("politicaldata") # if not already installed

# Let me search Ballotpedia systematically instead
