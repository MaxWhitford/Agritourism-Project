# ==============================================================================
# Agritourism Analysis: Pre-Statute Litigation → Statutory Content
# ==============================================================================
# Hypothesis: States with more pre-statute ag/liability litigation adopted
#             statutes with greater emphasis on liability protections (Topic 1)
# ==============================================================================

library(tidyverse)
library(stm)
library(stargazer)
library(ggplot2)
library(corrplot)

# ------------------------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------------------------

# Pre-statute case counts (from scraping)
case_counts <- read_csv("../../4_Analysis_Results/pre_statute_case_counts.csv")

# Your existing analysis data with topic proportions
# (Assumes you've run STM and have topic proportions for each state)

# If you have the STM output saved, load it:
# load("stm_model.RData")  # or whatever you named it

# For now, let's create the merged dataset structure
# You'll need to add your topic proportions from the STM output

cat("=== PRE-STATUTE LITIGATION DATA ===\n\n")

# Summary statistics
cat("States with case data:", nrow(case_counts), "\n")
cat("Total cases:", sum(case_counts$total_cases), "\n")
cat("Pre-statute cases:", sum(case_counts$pre_statute_cases), "\n")
cat("Pre-statute liability cases:", sum(case_counts$pre_statute_liability), "\n\n")

# Distribution of pre-statute cases
cat("Distribution of pre-statute case counts:\n")
summary(case_counts$pre_statute_cases)

cat("\n\nStates with most pre-statute litigation:\n")
case_counts %>%
  arrange(desc(pre_statute_cases)) %>%
  select(state, pre_statute_cases, pre_statute_liability, enact_year) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# 2. VISUALIZE PRE-STATUTE LITIGATION
# ------------------------------------------------------------------------------

# Bar chart of pre-statute cases by state
p1 <- ggplot(case_counts, aes(x = reorder(state, pre_statute_cases), 
                               y = pre_statute_cases)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Pre-Statute Agricultural Litigation by State",
       subtitle = "Cases filed before agritourism statute enactment",
       x = NULL,
       y = "Number of Cases") +
  theme_minimal()

ggsave("pre_statute_cases_by_state.png", p1, width = 10, height = 12)
cat("\nSaved: pre_statute_cases_by_state.png\n")

# Timeline: When did states enact statutes relative to litigation?
p2 <- ggplot(case_counts, aes(x = enact_year, y = pre_statute_cases)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = state), hjust = -0.1, vjust = 0, size = 2.5, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  labs(title = "Enactment Year vs. Pre-Statute Litigation",
       subtitle = "Do states with more litigation enact statutes earlier or later?",
       x = "Year of Statute Enactment",
       y = "Pre-Statute Cases") +
  theme_minimal() +
  xlim(2003, 2020)

ggsave("enactment_vs_litigation.png", p2, width = 10, height = 8)
cat("Saved: enactment_vs_litigation.png\n")

# ------------------------------------------------------------------------------
# 3. MERGE WITH TOPIC PROPORTIONS
# ------------------------------------------------------------------------------

# This section requires your STM topic proportions
# I'll create the structure - you'll need to add your actual theta values

# Example structure (you'll replace with actual data):
# topic_proportions <- data.frame(
#   state = c("Alabama", "Arkansas", ...),
#   topic1_liability = theta[,1],  # from your STM model
#   topic2_regulatory = theta[,2],
#   topic3_tourism = theta[,3],
#   topic4_landuse = theta[,4]
# )

# For demonstration, let's assume you have this data and test the hypothesis

# If you have topic proportions in a CSV:
# topic_props <- read_csv("topic_proportions.csv")

# Merge:
# analysis_data <- case_counts %>%
#   left_join(topic_props, by = "state")

# ------------------------------------------------------------------------------
# 4. KEY HYPOTHESIS TEST
# ------------------------------------------------------------------------------

# Model: Topic 1 (Liability) = f(pre_statute_liability_cases)

# Simple correlation first:
# cor.test(analysis_data$pre_statute_liability, analysis_data$topic1_liability)

# Beta regression (for proportional DV):
# library(betareg)
# model1 <- betareg(topic1_liability ~ pre_statute_liability + pre_statute_cases, 
#                   data = analysis_data)
# summary(model1)

# Alternative: OLS with robust SEs
# model2 <- lm(topic1_liability ~ pre_statute_liability + log(pre_statute_cases + 1) + 
#              enact_year, data = analysis_data)
# summary(model2)

# ------------------------------------------------------------------------------
# 5. CREATE PUBLISHABLE OUTPUT
# ------------------------------------------------------------------------------

# For now, let's create a summary table
summary_table <- case_counts %>%
  mutate(
    any_liability = pre_statute_liability > 0,
    litigation_level = case_when(
      pre_statute_cases == 0 ~ "None",
      pre_statute_cases <= 3 ~ "Low (1-3)",
      pre_statute_cases <= 7 ~ "Medium (4-7)",
      TRUE ~ "High (8+)"
    )
  ) %>%
  group_by(litigation_level) %>%
  summarize(
    n_states = n(),
    states = paste(state, collapse = ", "),
    .groups = "drop"
  )

cat("\n\n=== LITIGATION LEVELS ===\n")
print(summary_table)

# Save for paper
write_csv(case_counts, "analysis_case_counts_final.csv")
cat("\nSaved: analysis_case_counts_final.csv\n")

# ------------------------------------------------------------------------------
# 6. INSTRUCTIONS FOR COMPLETING ANALYSIS
# ------------------------------------------------------------------------------

cat("\n\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("NEXT STEPS TO COMPLETE ANALYSIS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

cat("1. Run your STM model (or load existing results)\n")
cat("2. Extract theta (topic proportions) for each state\n")
cat("3. Merge topic proportions with case_counts data\n")
cat("4. Run the hypothesis tests:\n\n")

cat("   # Key test: Does pre-statute liability litigation predict Topic 1?\n")
cat("   library(betareg)\n")
cat("   model <- betareg(topic1_liability ~ pre_statute_liability + \n")
cat("                    log(pre_statute_cases + 1), data = merged_data)\n")
cat("   summary(model)\n\n")

cat("5. Create publication-ready tables with stargazer\n\n")

cat("THEORETICAL FRAMING:\n")
cat("- 'Reactive policymaking': States respond to litigation pressure\n")
cat("- States experiencing farm injury lawsuits craft statutes to limit liability\n")
cat("- This explains Topic 1 (Liability & Injury) geographic concentration\n\n")

cat("KEY STATES TO DISCUSS:\n")
cat("- Louisiana (13 pre-statute cases): Heavy litigation -> liability focus?\n")
cat("- Texas (9 cases), Wisconsin (9 cases): Compare statutory content\n")
cat("- Washington, West Virginia (0 pre-statute cases): Different framing?\n")
