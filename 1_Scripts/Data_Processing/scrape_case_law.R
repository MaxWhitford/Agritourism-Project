# ==============================================================================
# Scrape National Agricultural Law Center - Agritourism Case Law Index
# ==============================================================================

library(tidyverse)
library(rvest)
library(lubridate)

# ------------------------------------------------------------------------------
# 1. SCRAPE THE PAGE
# ------------------------------------------------------------------------------

url <- "https://nationalaglawcenter.org/aglaw-reporter/case-law-index/agritourism/"

# Read the HTML
page <- read_html(url)

# Get all the text content
page_text <- page %>% 
  html_text2()

# ------------------------------------------------------------------------------
# 2. PARSE BY STATE
# ------------------------------------------------------------------------------

# The page is organized with state headers like "### ALABAMA" or just "ALABAMA"
# followed by case entries

# First, let's extract the main content area
content <- page %>%
  html_nodes("body") %>%
  html_text2()

# Split by state headers - states appear as headers in the HTML
# We'll parse the raw text and look for state patterns

# Define all US states
states <- c("ALABAMA", "ALASKA", "ARIZONA", "ARKANSAS", "CALIFORNIA", "COLORADO",
            "CONNECTICUT", "DELAWARE", "FLORIDA", "GEORGIA", "HAWAII", "IDAHO",
            "ILLINOIS", "INDIANA", "IOWA", "KANSAS", "KENTUCKY", "LOUISIANA",
            "MAINE", "MARYLAND", "MASSACHUSETTS", "MICHIGAN", "MINNESOTA",
            "MISSISSIPPI", "MISSOURI", "MONTANA", "NEBRASKA", "NEVADA",
            "NEW HAMPSHIRE", "NEW JERSEY", "NEW MEXICO", "NEW YORK",
            "NORTH CAROLINA", "NORTH DAKOTA", "OHIO", "OKLAHOMA", "OREGON",
            "PENNSYLVANIA", "RHODE ISLAND", "SOUTH CAROLINA", "SOUTH DAKOTA",
            "TENNESSEE", "TEXAS", "UTAH", "VERMONT", "VIRGINIA", "WASHINGTON",
            "WEST VIRGINIA", "WISCONSIN", "WYOMING")

# Also include federal circuits
circuits <- c("FIRST CIRCUIT", "SECOND CIRCUIT", "THIRD CIRCUIT", "FOURTH CIRCUIT",
              "FIFTH CIRCUIT", "SIXTH CIRCUIT", "SEVENTH CIRCUIT", "EIGHTH CIRCUIT",
              "NINTH CIRCUIT", "TENTH CIRCUIT", "ELEVENTH CIRCUIT", "TAX COURT")

# ------------------------------------------------------------------------------
# 3. ALTERNATIVE APPROACH: Parse the HTML structure directly
# ------------------------------------------------------------------------------

# Get all paragraph and text elements
all_text <- page %>%
  html_nodes("p, h3, hr") %>%
  html_text2()

# The cases appear after state name headers
# Let's get the raw HTML and parse more carefully

raw_html <- page %>% html_nodes("body") %>% as.character()

# Function to extract year from case citation
extract_year <- function(text) {
  # Look for 4-digit years in parentheses or after common patterns
  years <- str_extract_all(text, "\\b(19[89][0-9]|20[0-2][0-9])\\b")[[1]]
  if (length(years) > 0) {
    # Return the most recent year (usually the decision year)
    return(max(as.numeric(years)))
  }
  return(NA)
}

# Function to extract state from case citation
extract_state_from_citation <- function(text) {
  # Common patterns: (Ala. 2012), (Tex. App. 2009), (N.Y. App. Div. 2010)
  state_abbrevs <- c(
    "Ala\\." = "Alabama", "Alaska" = "Alaska", "Ariz\\." = "Arizona", 
    "Ark\\." = "Arkansas", "Cal\\." = "California", "Colo\\." = "Colorado",
    "Conn\\." = "Connecticut", "Del\\." = "Delaware", "Fla\\." = "Florida",
    "Ga\\." = "Georgia", "Haw\\." = "Hawaii", "Idaho" = "Idaho",
    "Ill\\." = "Illinois", "Ind\\." = "Indiana", "Iowa" = "Iowa",
    "Kan\\." = "Kansas", "Ky\\." = "Kentucky", "La\\." = "Louisiana",
    "Me\\." = "Maine", "Md\\." = "Maryland", "Mass\\." = "Massachusetts",
    "Mich\\." = "Michigan", "Minn\\." = "Minnesota", "Miss\\." = "Mississippi",
    "Mo\\." = "Missouri", "Mont\\." = "Montana", "Neb\\." = "Nebraska",
    "Nev\\." = "Nevada", "N\\.H\\." = "New Hampshire", "N\\.J\\." = "New Jersey",
    "N\\.M\\." = "New Mexico", "N\\.Y\\." = "New York", "N\\.C\\." = "North Carolina",
    "N\\.D\\." = "North Dakota", "Ohio" = "Ohio", "Okla\\." = "Oklahoma",
    "Or\\." = "Oregon", "Pa\\." = "Pennsylvania", "R\\.I\\." = "Rhode Island",
    "S\\.C\\." = "South Carolina", "S\\.D\\." = "South Dakota", "Tenn\\." = "Tennessee",
    "Tex\\." = "Texas", "Utah" = "Utah", "Vt\\." = "Vermont", "Va\\." = "Virginia",
    "Wash\\." = "Washington", "W\\.Va\\." = "West Virginia", "Wis\\." = "Wisconsin",
    "Wyo\\." = "Wyoming"
  )
  
  for (pattern in names(state_abbrevs)) {
    if (str_detect(text, pattern)) {
      return(state_abbrevs[pattern])
    }
  }
  return(NA)
}

# Function to classify case type based on description
classify_case <- function(text) {
  text_lower <- tolower(text)
  
  if (str_detect(text_lower, "negligence|liability|injury|injuries|personal injury|death")) {
    return("Negligence/Liability")
  } else if (str_detect(text_lower, "zoning|land use|agricultural use|permit")) {
    return("Zoning/Land Use")
  } else if (str_detect(text_lower, "equine|horse|riding|stable|horseback")) {
    return("Equine Activity")
  } else if (str_detect(text_lower, "recreational use|recreation")) {
    return("Recreational Use")
  } else if (str_detect(text_lower, "winery|wine|vineyard|alcohol")) {
    return("Winery/Alcohol")
  } else if (str_detect(text_lower, "insurance|coverage")) {
    return("Insurance")
  } else if (str_detect(text_lower, "tax|taxation")) {
    return("Taxation")
  } else {
    return("Other")
  }
}

# ------------------------------------------------------------------------------
# 4. PARSE CASES LINE BY LINE
# ------------------------------------------------------------------------------

# Split the page text into lines
lines <- str_split(page_text, "\n")[[1]]
lines <- lines[lines != ""]  # Remove empty lines

# Initialize variables
cases <- tibble(
  case_name = character(),
  citation = character(),
  year = integer(),
  state = character(),
  jurisdiction = character(),
  description = character(),
  case_type = character()
)

current_jurisdiction <- NA
current_state <- NA

for (line in lines) {
  line <- str_trim(line)
  
  # Skip empty or very short lines
  if (nchar(line) < 5) next
  
  # Check if this is a jurisdiction header (federal circuit)
  if (line %in% circuits) {
    current_jurisdiction <- line
    current_state <- "Federal"
    next
  }
  
  # Check if this is a state header
  if (toupper(line) %in% states) {
    current_state <- str_to_title(line)
    current_jurisdiction <- "State"
    next
  }
  
  # Check if this looks like a case entry (contains "v." and a year)
  if (str_detect(line, " v\\.? ") && str_detect(line, "\\b(19[89][0-9]|20[0-2][0-9])\\b")) {
    
    # Extract case name (everything before the citation)
    case_name <- str_extract(line, "^[^,]+v\\.[^,]+")
    if (is.na(case_name)) {
      case_name <- str_extract(line, "^.+?(?=\\d+\\s+[A-Z])")
    }
    
    # Extract year
    year <- extract_year(line)
    
    # Extract description (text in parentheses at the end, before [Text])
    description <- str_extract(line, "\\([^)]+\\)(?=\\s*\\[Text\\]|$)")
    if (is.na(description)) {
      description <- str_extract(line, "\\([^)]+\\)$")
    }
    description <- str_remove_all(description, "^\\(|\\)$")
    
    # Determine state from current context or citation
    if (current_jurisdiction == "State" && !is.na(current_state)) {
      state <- current_state
    } else {
      state <- extract_state_from_citation(line)
      if (is.na(state)) state <- "Federal"
    }
    
    # Classify case type
    case_type <- classify_case(line)
    
    # Add to cases
    cases <- cases %>% add_row(
      case_name = str_trim(case_name),
      citation = line,
      year = year,
      state = state,
      jurisdiction = current_jurisdiction,
      description = description,
      case_type = case_type
    )
  }
}

# ------------------------------------------------------------------------------
# 5. CLEAN AND SUMMARIZE
# ------------------------------------------------------------------------------

cat("=== SCRAPING RESULTS ===\n")
cat("Total cases extracted:", nrow(cases), "\n\n")

# Cases by state
cat("Cases by state:\n")
cases %>%
  filter(state != "Federal") %>%
  count(state, sort = TRUE) %>%
  print(n = 50)

# Cases by type
cat("\nCases by type:\n")
cases %>%
  count(case_type, sort = TRUE) %>%
  print()

# Cases by year
cat("\nCases by decade:\n")
cases %>%
  filter(!is.na(year)) %>%
  mutate(decade = floor(year / 10) * 10) %>%
  count(decade) %>%
  print()

# ------------------------------------------------------------------------------
# 6. CREATE ANALYSIS VARIABLE: Pre-statute case counts
# ------------------------------------------------------------------------------

# Agritourism statute enactment years (from our earlier research)
enactment_years <- tribble(
  ~state, ~enact_year,
  "Alabama", 2012,
  "Arkansas", 2011,
  "Colorado", 2008,
  "Florida", 2013,
  "Georgia", 2008,
  "Hawaii", 2006,
  "Idaho", 2014,
  "Illinois", 2006,
  "Indiana", 2011,
  "Kansas", 2004,
  "Kentucky", 2012,
  "Louisiana", 2008,
  "Maine", 2012,
  "Maryland", 2018,
  "Massachusetts", 2006,
  "Michigan", 2006,
  "Minnesota", 2015,
  "Mississippi", 2012,
  "Missouri", 2010,
  "Montana", 2009,
  "Nebraska", 2015,
  "North Carolina", 2005,
  "North Dakota", 2011,
  "Ohio", 2016,
  "Oklahoma", 2004,
  "Oregon", 2015,
  "South Carolina", 2010,
  "South Dakota", 2010,
  "Tennessee", 2009,
  "Texas", 2015,
  "Utah", 2008,
  "Virginia", 2006,
  "Washington", 2017,
  "West Virginia", 2018,
  "Wisconsin", 2014
)

# Count pre-statute cases for each state
pre_statute_cases <- cases %>%
  filter(state != "Federal") %>%
  left_join(enactment_years, by = "state") %>%
  filter(!is.na(enact_year) & !is.na(year)) %>%
  mutate(pre_statute = year < enact_year) %>%
  group_by(state) %>%
  summarize(
    total_cases = n(),
    pre_statute_cases = sum(pre_statute, na.rm = TRUE),
    post_statute_cases = sum(!pre_statute, na.rm = TRUE),
    pre_statute_liability = sum(pre_statute & case_type == "Negligence/Liability", na.rm = TRUE),
    pre_statute_equine = sum(pre_statute & case_type == "Equine Activity", na.rm = TRUE),
    enact_year = first(enact_year)
  )

cat("\n=== PRE-STATUTE CASE COUNTS ===\n")
print(pre_statute_cases, n = 40)

# ------------------------------------------------------------------------------
# 7. SAVE DATA
# ------------------------------------------------------------------------------

# Save all cases
write_csv(cases, here::"../../2_Data/Raw/agritourism_cases_scraped.csv")
cat("\nSaved all cases to: agritourism_cases_scraped.csv\n")

# Save pre-statute summary
write_csv(pre_statute_cases, here::"../../4_Analysis_Results/pre_statute_case_counts.csv")
cat("Saved pre-statute counts to: pre_statute_case_counts.csv\n")

# ------------------------------------------------------------------------------
# 8. MERGE WITH ANALYSIS DATA (if available)
# ------------------------------------------------------------------------------

if (file.exists(here::"../../2_Data/Processed/agritourism_analysis_final.csv")) {
  analysis_data <- read_csv(here::"../../2_Data/Processed/agritourism_analysis_final.csv")
  
  # Merge pre-statute case counts
  analysis_with_cases <- analysis_data %>%
    left_join(pre_statute_cases, by = "state")
  
  # Replace NAs with 0 for states without cases
  analysis_with_cases <- analysis_with_cases %>%
    mutate(across(c(total_cases, pre_statute_cases, post_statute_cases, 
                    pre_statute_liability, pre_statute_equine),
                  ~replace_na(., 0)))
  
  write_csv(analysis_with_cases, "../../2_Data/Processed/agritourism_analysis_with_cases.csv"))
  cat("Saved merged data to: agritourism_analysis_with_cases.csv\n")
  
  cat("\n=== MERGED DATA SUMMARY ===\n")
  cat("States with case data:", sum(analysis_with_cases$total_cases > 0), "\n")
  cat("Total pre-statute liability cases:", sum(analysis_with_cases$pre_statute_liability), "\n")
}
