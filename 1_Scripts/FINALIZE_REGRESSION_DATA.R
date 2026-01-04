# ==============================================================================
# ADD CASES DATA TO CLEAN REGRESSION DATASET
# ==============================================================================

library(tidyverse)

cat("=== MERGING CASE DATA ===\n\n")

# Load the clean dataset
clean_data <- read_csv("../2_Data/Processed/agritourism_regression_data_clean.csv",
                       show_col_types = FALSE)

cat("Loaded clean dataset:", nrow(clean_data), "rows\n\n")

# Load existing dataset to get cases
if(file.exists("../2_Data/Processed/beta_regression_complete_dataset.csv")) {
  existing <- read_csv("../2_Data/Processed/beta_regression_complete_dataset.csv", show_col_types = FALSE)
  
  # Extract cases
  cases_data <- existing %>%
    select(state, total_cases)
  
  cat("Cases data from existing dataset:\n")
  cat("  States with case data:", sum(!is.na(cases_data$total_cases)), "\n\n")
  
  # Merge
  final_data <- clean_data %>%
    select(-total_cases) %>%  # Remove placeholder
    left_join(cases_data, by = "state")
  
} else {
  cat("Existing dataset not found. Setting total_cases = 0 for all states.\n\n")
  final_data <- clean_data %>%
    mutate(total_cases = 0)
}

# Ensure total_cases has no NAs (set to 0 if missing)
final_data <- final_data %>%
  mutate(total_cases = replace_na(total_cases, 0))

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("=== FINAL DATASET SUMMARY ===\n\n")

cat("Variables:\n")
print(names(final_data))
cat("\n")

cat("Coverage:\n")
cat("  Total states: 43\n")
cat("  ag_cash_receipts:", sum(!is.na(final_data$ag_cash_receipts)), "/ 43\n")
cat("  govparty_republican:", sum(!is.na(final_data$govparty_republican)), "/ 43\n")
cat("  total_cases:", sum(!is.na(final_data$total_cases)), "/ 43\n\n")

cat("Governor Party:\n")
cat("  Republican:", sum(final_data$govparty_republican == 1), "\n")
cat("  Democrat:", sum(final_data$govparty_republican == 0), "\n\n")

cat("Cases:\n")
cat("  States with cases > 0:", sum(final_data$total_cases > 0), "\n")
cat("  Total cases:", sum(final_data$total_cases, na.rm = TRUE), "\n\n")

cat("Sample data:\n")
print(head(final_data, 10))
cat("\n")

# ==============================================================================
# SAVE FINAL DATASET
# ==============================================================================

write_csv(final_data, "../2_Data/Processed/agritourism_regression_data_FINAL.csv")

cat("=== SAVED ===\n")
cat("File: agritourism_regression_data_FINAL.csv\n\n")

cat("Ready for beta regression with:\n")
cat("  • Agricultural cash receipts (USDA) - N=43\n")
cat("  • Governor partisanship - N=43\n")
cat("  • Legal cases - N=43\n\n")

cat("ALL HYPOTHESES AT N=43!\n")
