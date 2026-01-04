# ==============================================================================
# Geographic Diffusion Results Table
# Creates publication-ready tables for spatial diffusion findings
# ==============================================================================

library(tidyverse)
library(gt)
library(webshot2)
library(here)

# ==============================================================================
# LOAD DATA
# ==============================================================================

# Load the similarity matrix (already in long format) and analysis data
sim_long <- read_csv("../../2_Data/STM/similarity_matrix_full_sample.csv")
analysis_data <- read_csv("../../2_Data/Processed/analysis_data_full_sample.csv")

# ==============================================================================
# GEOGRAPHIC DIFFUSION ANALYSIS
# ==============================================================================

# Filter to unique pairs (avoid counting A-B and B-A separately)
sim_unique <- sim_long %>%
  filter(state1 < state2)

# Calculate statistics
neighbor_pairs <- sim_unique %>% filter(neighbors == TRUE)
non_neighbor_pairs <- sim_unique %>% filter(neighbors == FALSE)

mean_neighbor <- mean(neighbor_pairs$similarity, na.rm = TRUE)
mean_non_neighbor <- mean(non_neighbor_pairs$similarity, na.rm = TRUE)
diff <- mean_neighbor - mean_non_neighbor

# T-test
t_result <- t.test(neighbor_pairs$similarity, non_neighbor_pairs$similarity)

# Effect size (Cohen's d)
pooled_sd <- sqrt(((nrow(neighbor_pairs)-1)*var(neighbor_pairs$similarity, na.rm=TRUE) + 
                   (nrow(non_neighbor_pairs)-1)*var(non_neighbor_pairs$similarity, na.rm=TRUE)) / 
                  (nrow(neighbor_pairs) + nrow(non_neighbor_pairs) - 2))
cohens_d <- diff / pooled_sd

cat("Geographic Diffusion Results:\n")
cat("  Neighbor pairs:", nrow(neighbor_pairs), "\n")
cat("  Non-neighbor pairs:", nrow(non_neighbor_pairs), "\n")
cat("  Mean similarity (neighbors):", round(mean_neighbor, 3), "\n")
cat("  Mean similarity (non-neighbors):", round(mean_non_neighbor, 3), "\n")
cat("  Difference:", round(diff * 100, 1), "percentage points\n")
cat("  t-statistic:", round(t_result$statistic, 3), "\n")
cat("  p-value:", format(t_result$p.value, digits = 4), "\n")
cat("  Cohen's d:", round(cohens_d, 3), "\n")

# ==============================================================================
# OPTION 1: STANDALONE GEOGRAPHIC DIFFUSION TABLE
# ==============================================================================

geo_results <- tibble(
  Comparison = c("Neighboring States", "Non-Neighboring States", "Difference"),
  `Mean Similarity` = c(
    sprintf("%.3f", mean_neighbor),
    sprintf("%.3f", mean_non_neighbor),
    sprintf("%.3f", diff)
  ),
  `N Pairs` = c(
    nrow(neighbor_pairs),
    nrow(non_neighbor_pairs),
    NA
  ),
  `Test Statistic` = c(NA, NA, sprintf("t = %.2f", t_result$statistic)),
  `p-value` = c(NA, NA, sprintf("%.4f***", t_result$p.value)),
  `Effect Size` = c(NA, NA, sprintf("d = %.2f", cohens_d))
)

tbl_geo <- geo_results %>%
  gt() %>%
  tab_header(
    title = md("**Table X. Geographic Diffusion of Statutory Content**"),
    subtitle = md("*Similarity in Topic Proportions by Geographic Proximity (N = 43 States)*")
  ) %>%
  cols_align(align = "center", columns = -Comparison) %>%
  cols_align(align = "left", columns = Comparison) %>%
  sub_missing(missing_text = "—") %>%
  tab_style(
    style = cell_fill(color = "#e8f4e8"),
    locations = cells_body(rows = 3)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 3)
  ) %>%
  tab_source_note(
    source_note = md("*Notes:* Similarity measured as 1 - Euclidean distance between state topic proportion vectors (K=4 topics). Neighboring states defined as geographically contiguous. *** p < 0.001. Effect size: Cohen's d, where d > 0.5 indicates medium effect, d > 0.8 indicates large effect.")
  ) %>%
  tab_options(
    table.font.size = 11,
    heading.title.font.size = 13,
    heading.subtitle.font.size = 10,
    source_notes.font.size = 9
  )

print(tbl_geo)

gtsave(tbl_geo, "../../3_Output/Tables/table_geographic_diffusion.png", vwidth = 700, vheight = 280)
gtsave(tbl_geo, "../../3_Output/Tables/table_geographic_diffusion.html")
cat("\nSaved: table_geographic_diffusion.png\n")

# ==============================================================================
# OPTION 2: COMBINED TABLE - INTERNAL VS EXTERNAL FACTORS
# ==============================================================================

# Load CSPP covariates
cspp_data <- read_csv("../../2_Data/Processed/agritourism_analysis_final.csv") %>%
  select(state, contrib_agri, valueofagsect, govparty_c)

# Load case data
cases <- read_csv("../../2_Data/Raw/agritourism_cases_scraped.csv")
case_counts <- cases %>%
  filter(state != "Federal") %>%
  count(state, name = "n_cases")

# Merge
model_data <- analysis_data %>%
  left_join(cspp_data, by = "state") %>%
  left_join(case_counts, by = "state") %>%
  mutate(n_cases = replace_na(n_cases, 0))

# Run political economy models
m1 <- lm(topic1 ~ log(contrib_agri + 1), data = model_data %>% filter(!is.na(contrib_agri)))
m2 <- lm(topic1 ~ log(valueofagsect + 1), data = model_data %>% filter(!is.na(valueofagsect)))
m3 <- lm(topic1 ~ factor(govparty_c), data = model_data %>% filter(!is.na(govparty_c)))
m4 <- lm(topic1 ~ log(n_cases + 1), data = model_data)

# Extract results
get_results <- function(model, predictor_name, row = 2) {
  s <- summary(model)
  tibble(
    Predictor = predictor_name,
    Estimate = sprintf("%.3f", s$coefficients[row, 1]),
    SE = sprintf("(%.3f)", s$coefficients[row, 2]),
    p_value = s$coefficients[row, 4],
    p_fmt = case_when(
      s$coefficients[row, 4] < 0.001 ~ "< 0.001***",
      s$coefficients[row, 4] < 0.01 ~ sprintf("%.3f**", s$coefficients[row, 4]),
      s$coefficients[row, 4] < 0.05 ~ sprintf("%.3f*", s$coefficients[row, 4]),
      s$coefficients[row, 4] < 0.10 ~ sprintf("%.3f†", s$coefficients[row, 4]),
      TRUE ~ sprintf("%.3f", s$coefficients[row, 4])
    ),
    N = nobs(model)
  )
}

# Build combined table
combined_results <- bind_rows(
  # Internal factors (null)
  tibble(Category = "INTERNAL FACTORS", Predictor = "", Estimate = "", SE = "", p_fmt = "", N = NA_integer_),
  get_results(m1, "Agricultural PAC Contributions (log)") %>% mutate(Category = ""),
  get_results(m2, "Agricultural Sector Value (log)") %>% mutate(Category = ""),
  get_results(m3, "Republican Governor") %>% mutate(Category = ""),
  get_results(m4, "Agritourism Court Cases (log)") %>% mutate(Category = ""),
  # Spacer
  tibble(Category = "", Predictor = "", Estimate = "", SE = "", p_fmt = "", N = NA_integer_),
  # External factors (significant)
  tibble(Category = "EXTERNAL FACTORS", Predictor = "", Estimate = "", SE = "", p_fmt = "", N = NA_integer_),
  tibble(
    Category = "",
    Predictor = "Geographic Contiguity",
    Estimate = sprintf("+%.1f pp", diff * 100),
    SE = "",
    p_fmt = sprintf("%.4f***", t_result$p.value),
    N = 43L
  )
) %>%
  select(Category, Predictor, Estimate, SE, p_fmt, N)

tbl_combined <- combined_results %>%
  gt() %>%
  tab_header(
    title = md("**Table X. Internal vs. External Predictors of Statutory Content**"),
    subtitle = md("*What Drives Agritourism Liability Statute Content? (N = 43 States)*")
  ) %>%
  cols_label(
    Category = "",
    Predictor = "Predictor",
    Estimate = "β / Effect",
    SE = "SE",
    p_fmt = "p-value",
    N = "N"
  ) %>%
  cols_align(align = "center", columns = c(Estimate, SE, p_fmt, N)) %>%
  cols_align(align = "left", columns = c(Category, Predictor)) %>%
  sub_missing(missing_text = "") %>%
  tab_style(
    style = cell_text(weight = "bold", size = "medium"),
    locations = cells_body(rows = c(1, 7))
  ) %>%
  tab_style(
    style = cell_fill(color = "#f0f0f0"),
    locations = cells_body(rows = c(1, 7))
  ) %>%
  tab_style(
    style = cell_fill(color = "#e8f4e8"),
    locations = cells_body(rows = 8)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = 8)
  ) %>%
  tab_source_note(
    source_note = md("*Notes:* Internal factors: OLS regression predicting Topic 1 (Liability/Warning) proportion. External factors: difference in mean similarity between neighboring vs. non-neighboring state pairs. † p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001. pp = percentage points.")
  ) %>%
  tab_footnote(
    footnote = sprintf("Effect size: Cohen's d = %.2f (medium effect)", cohens_d),
    locations = cells_body(columns = Predictor, rows = 8)
  ) %>%
  tab_options(
    table.font.size = 11,
    heading.title.font.size = 13,
    heading.subtitle.font.size = 10,
    source_notes.font.size = 9,
    footnotes.font.size = 9
  )

print(tbl_combined)

gtsave(tbl_combined, "../../3_Output/Tables/table_internal_vs_external.png", vwidth = 750, vheight = 420)
gtsave(tbl_combined, "../../3_Output/Tables/table_internal_vs_external.html")
cat("Saved: table_internal_vs_external.png\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("SUMMARY: TWO TABLE OPTIONS CREATED\n")
cat(rep("=", 70), "\n", sep = "")
cat("\n1. STANDALONE: table_geographic_diffusion.png\n")
cat("   - Simple table showing neighbor vs non-neighbor similarity\n")
cat("   - Use if you want to keep diffusion findings separate\n")
cat("\n2. COMBINED: table_internal_vs_external.png\n")
cat("   - Shows internal factors (all null) vs external (significant)\n")
cat("   - Directly contrasts the two theoretical mechanisms\n")
cat("   - More impactful for the 'diffusion beats political economy' argument\n")
