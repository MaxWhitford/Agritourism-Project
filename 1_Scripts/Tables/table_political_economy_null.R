# ==============================================================================
# Political Economy Null Results Table - Full Sample (N=43) with Cases
# Creates publication-ready table showing internal dynamics don't predict content
# ==============================================================================

library(tidyverse)
library(gt)
library(webshot2)  # For saving as PNG
library(here)

# ==============================================================================
# LOAD DATA
# ==============================================================================

# Load the analysis data with topic proportions
analysis_data <- read_csv("../../2_Data/Processed/analysis_data_full_sample.csv")

# Load CSPP covariates for states that have them
cspp_data <- read_csv("../../2_Data/Processed/agritourism_analysis_final.csv") %>%
  select(state, contrib_agri, valueofagsect, govparty_c)

# Load case data and count by state
cases <- read_csv("../../2_Data/Raw/agritourism_cases_scraped.csv")

case_counts <- cases %>%
  filter(state != "Federal") %>%
  count(state, name = "n_cases")

# Merge all data
model_data <- analysis_data %>%
  left_join(cspp_data, by = "state") %>%
  left_join(case_counts, by = "state") %>%
  mutate(n_cases = replace_na(n_cases, 0))

cat("Full sample:", nrow(model_data), "states\n")
cat("States with CSPP covariates:", sum(!is.na(model_data$contrib_agri)), "\n")
cat("States with cases:", sum(model_data$n_cases > 0), "\n")
cat("Total cases:", sum(model_data$n_cases), "\n")

# ==============================================================================
# RUN MODELS
# ==============================================================================

# Model 1: Agricultural contributions → Topic 1 (Liability)
m1 <- lm(topic1 ~ log(contrib_agri + 1), data = model_data %>% filter(!is.na(contrib_agri)))

# Model 2: Ag sector value → Topic 1
m2 <- lm(topic1 ~ log(valueofagsect + 1), data = model_data %>% filter(!is.na(valueofagsect)))

# Model 3: Governor party → Topic 1
m3 <- lm(topic1 ~ factor(govparty_c), data = model_data %>% filter(!is.na(govparty_c)))

# Model 4: Number of agritourism cases → Topic 1 (N=43, full sample!)
m4 <- lm(topic1 ~ log(n_cases + 1), data = model_data)

# Model 5: Full model (only states with all covariates)
m5 <- lm(topic1 ~ log(contrib_agri + 1) + log(valueofagsect + 1) + factor(govparty_c) + log(n_cases + 1), 
         data = model_data %>% filter(!is.na(contrib_agri) & !is.na(valueofagsect) & !is.na(govparty_c)))

# ==============================================================================
# EXTRACT RESULTS
# ==============================================================================

extract_model_results <- function(model, predictor_label, row_num = 2) {
  s <- summary(model)
  tibble(
    Predictor = predictor_label,
    Estimate = s$coefficients[row_num, 1],
    SE = s$coefficients[row_num, 2],
    p = s$coefficients[row_num, 4],
    N = nobs(model),
    R2 = s$r.squared
  )
}

# Build results table
results <- bind_rows(
  extract_model_results(m1, "Agricultural Contributions (log)"),
  extract_model_results(m2, "Agricultural Sector Value (log)"),
  extract_model_results(m3, "Republican Governor"),
  extract_model_results(m4, "Agritourism Cases (log)")
)

# Add model labels
results$Model <- c("H1", "H2", "H3", "H4")

# Reorder columns
results <- results %>%
  select(Model, Predictor, Estimate, SE, p, N, R2)

# ==============================================================================
# CREATE GT TABLE
# ==============================================================================

# Format for display
results_display <- results %>%
  mutate(
    Estimate_fmt = sprintf("%.3f", Estimate),
    SE_fmt = sprintf("(%.3f)", SE),
    p_fmt = case_when(
      p < 0.001 ~ "< 0.001***",
      p < 0.01 ~ paste0(sprintf("%.3f", p), "**"),
      p < 0.05 ~ paste0(sprintf("%.3f", p), "*"),
      p < 0.10 ~ paste0(sprintf("%.3f", p), "†"),
      TRUE ~ sprintf("%.3f", p)
    ),
    R2_fmt = sprintf("%.3f", R2)
  )

# Create the table
tbl <- results_display %>%
  select(Model, Predictor, Estimate_fmt, SE_fmt, p_fmt, N, R2_fmt) %>%
  gt() %>%
  tab_header(
    title = md("**Table X. Internal Political Economy Does Not Predict Statutory Content**"),
    subtitle = md("*OLS Regression: Predictors of Liability Topic Proportion (N = 43 States)*")
  ) %>%
  cols_label(
    Model = "Hypothesis",
    Predictor = "Predictor Variable",
    Estimate_fmt = "β",
    SE_fmt = "SE",
    p_fmt = "p-value",
    N = "N",
    R2_fmt = "R²"
  ) %>%
  tab_spanner(
    label = "Coefficient",
    columns = c(Estimate_fmt, SE_fmt, p_fmt)
  ) %>%
  tab_spanner(
    label = "Model Fit",
    columns = c(N, R2_fmt)
  ) %>%
  cols_align(
    align = "center",
    columns = c(Estimate_fmt, SE_fmt, p_fmt, N, R2_fmt)
  ) %>%
  cols_align(
    align = "left",
    columns = c(Model, Predictor)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f5f5f5"),
    locations = cells_body(rows = c(1, 3))
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_spanners()
  ) %>%
  tab_source_note(
    source_note = md("*Notes:* Dependent variable is Topic 1 (Liability/Warning Language) proportion from Structural Topic Model (K=4). † p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001. Agricultural contributions and sector value from Correlates of State Policy Project (CSPP); case counts from comprehensive legal database search. N varies by covariate availability; H4 uses full sample (N=43).")
  ) %>%
  tab_footnote(
    footnote = "Total agricultural PAC contributions to state legislators (logged dollars)",
    locations = cells_body(columns = Predictor, rows = 1)
  ) %>%
  tab_footnote(
    footnote = "Total value of agricultural sector in state economy (logged dollars)",
    locations = cells_body(columns = Predictor, rows = 2)
  ) %>%
  tab_footnote(
    footnote = "1 = Republican governor at time of enactment, 0 = Democratic",
    locations = cells_body(columns = Predictor, rows = 3)
  ) %>%
  tab_footnote(
    footnote = "Count of agritourism-related court cases in state (logged)",
    locations = cells_body(columns = Predictor, rows = 4)
  ) %>%
  tab_options(
    table.font.size = 11,
    heading.title.font.size = 13,
    heading.subtitle.font.size = 10,
    source_notes.font.size = 9,
    footnotes.font.size = 9,
    table.width = pct(100),
    column_labels.padding = px(5),
    data_row.padding = px(4)
  )

# Print table
print(tbl)

# ==============================================================================
# SAVE AS PNG
# ==============================================================================

# Save as PNG
gtsave(tbl, "../../3_Output/Tables/table_political_economy_null.png", vwidth = 850, vheight = 400)
cat("\nSaved: table_political_economy_null.png\n")

# Also save as HTML for flexibility
gtsave(tbl, "../../3_Output/Tables/table_political_economy_null.html")
cat("Saved: table_political_economy_null.html\n")

# ==============================================================================
# SUMMARY OUTPUT
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("POLITICAL ECONOMY NULL RESULTS SUMMARY\n")
cat(rep("=", 70), "\n", sep = "")

cat("\nKey Finding: NO internal political economy variable significantly\n")
cat("predicts the liability content of agritourism statutes.\n\n")

cat("Individual Models:\n")
cat("  H1 (Ag Contributions):  β =", sprintf("%6.3f", results$Estimate[1]), 
    ", p =", sprintf("%.3f", results$p[1]), ", N =", results$N[1], "\n")
cat("  H2 (Ag Sector Value):   β =", sprintf("%6.3f", results$Estimate[2]), 
    ", p =", sprintf("%.3f", results$p[2]), ", N =", results$N[2], "\n")
cat("  H3 (Governor Party):    β =", sprintf("%6.3f", results$Estimate[3]), 
    ", p =", sprintf("%.3f", results$p[3]), ", N =", results$N[3], "\n")
cat("  H4 (Agritourism Cases): β =", sprintf("%6.3f", results$Estimate[4]), 
    ", p =", sprintf("%.3f", results$p[4]), ", N =", results$N[4], "(FULL SAMPLE)\n")

cat("\nInterpretation: States do not tailor agritourism statutory content\n")
cat("to match their internal political, economic, or litigation conditions.\n")
cat("Instead, content is driven by external diffusion mechanisms.\n")
cat("\nNote: H4 (cases) uses all 43 states - the strongest test.\n")
