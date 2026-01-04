# ==============================================================================
# Agritourism Policy Diffusion Analysis
# Part 1: Callaway-Sant'Anna DiD for Treatment Effects
# Part 2: Topic Vector Equilibration Over Time
# ==============================================================================

library(tidyverse)
library(did)        # Callaway-Sant'Anna
library(fixest)     # Alternative DiD
library(ggplot2)
library(here)

set.seed(2024)

# ==============================================================================
# LOAD DATA
# ==============================================================================

cat("=== LOADING DATA ===\n\n")

# Case data from scrape
cases <- read_csv("../../2_Data/Raw/agritourism_cases_scraped.csv")

# Enactment years
enactment <- read_csv("../../2_Data/Processed/agritourism_analysis_final.csv") %>%
  select(state, enact_year)

# Topic proportions from STM
topic_data <- read_csv("../../2_Data/Processed/final_analysis_data.csv")

cat("Cases loaded:", nrow(cases), "\n")
cat("States with enactment data:", nrow(enactment), "\n")

# ==============================================================================
# PART 1: CALLAWAY-SANT'ANNA DiD
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("PART 1: CALLAWAY-SANT'ANNA DiD\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

# ------------------------------------------------------------------------------
# 1A. Build State-Year Panel of Case Counts
# ------------------------------------------------------------------------------

# Define panel years (1985-2025 to capture pre/post for all states)
years <- 1985:2025

# Get all states with agritourism statutes
states_with_statutes <- enactment$state

# Filter cases to relevant states and case types
# Focus on Negligence/Liability cases as primary outcome
liability_cases <- cases %>%
  filter(case_type == "Negligence/Liability") %>%
  filter(state %in% states_with_statutes | state == "Federal") %>%
  filter(!is.na(year))

cat("\nLiability/Negligence cases for analysis:", nrow(liability_cases), "\n")

# Create state-year panel
panel <- expand_grid(
  state = states_with_statutes,
  year = years
) %>%
  left_join(enactment, by = "state")

# Count cases per state-year
case_counts <- liability_cases %>%
  filter(state != "Federal") %>%  # Exclude federal cases for now
  group_by(state, year) %>%
  summarize(n_cases = n(), .groups = "drop")

# Merge counts into panel
panel <- panel %>%
  left_join(case_counts, by = c("state", "year")) %>%
  mutate(
    n_cases = replace_na(n_cases, 0),
    # Treatment indicator
    treated = year >= enact_year,
    # Cohort (year of treatment)
    cohort = enact_year,
    # Time relative to treatment
    rel_time = year - enact_year
  )

cat("\nPanel dimensions:", nrow(panel), "state-years\n")
cat("States:", n_distinct(panel$state), "\n")
cat("Years:", min(panel$year), "-", max(panel$year), "\n")

# ------------------------------------------------------------------------------
# 1B. Descriptive: Case Trends Pre/Post
# ------------------------------------------------------------------------------

cat("\n--- Descriptive: Case Counts by Treatment Status ---\n")

# Average cases pre vs post treatment
prepost_summary <- panel %>%
  filter(year >= 1990 & year <= 2023) %>%  # Trim to reasonable window
  mutate(period = case_when(
    rel_time < -5 ~ "Pre (>5 years before)",
    rel_time >= -5 & rel_time < 0 ~ "Pre (0-5 years before)",
    rel_time >= 0 & rel_time <= 5 ~ "Post (0-5 years after)",
    rel_time > 5 ~ "Post (>5 years after)"
  )) %>%
  filter(!is.na(period)) %>%
  group_by(period) %>%
  summarize(
    n_state_years = n(),
    total_cases = sum(n_cases),
    mean_cases = mean(n_cases),
    .groups = "drop"
  )

print(prepost_summary)

# Event study plot (raw means)
event_study_raw <- panel %>%
  filter(rel_time >= -10 & rel_time <= 10) %>%
  group_by(rel_time) %>%
  summarize(
    mean_cases = mean(n_cases),
    se = sd(n_cases) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

p_event_raw <- ggplot(event_study_raw, aes(x = rel_time, y = mean_cases)) +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "red") +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = mean_cases - 1.96*se, ymax = mean_cases + 1.96*se), 
              alpha = 0.2) +
  labs(title = "Raw Event Study: Liability Cases Around Statute Enactment",
       subtitle = "Mean cases per state-year, relative to enactment",
       x = "Years Relative to Enactment",
       y = "Mean Liability Cases") +
  theme_minimal()

ggsave("../../3_Output/Figures/event_study_raw.png"), p_event_raw, width = 10, height = 6)
cat("\nSaved: event_study_raw.png\n")

# ------------------------------------------------------------------------------
# 1C. Callaway-Sant'Anna DiD
# ------------------------------------------------------------------------------

cat("\n--- Callaway-Sant'Anna DiD Estimation ---\n")

# Prepare data for did package
# Need: id (state), time (year), cohort (first treatment year), outcome
did_data <- panel %>%
  filter(year >= 1995 & year <= 2023) %>%  # Reasonable window
  mutate(
    state_id = as.numeric(factor(state)),
    # For never-treated, set cohort to 0
    G = ifelse(is.na(enact_year), 0, enact_year)
  ) %>%
  filter(!is.na(n_cases))

# Check cohort distribution
cat("\nCohort distribution:\n")
table(did_data$G) %>% print()

# Run Callaway-Sant'Anna
# Note: This requires variation in treatment timing and some never-treated or 
# not-yet-treated units for comparison

tryCatch({
  cs_out <- att_gt(
    yname = "n_cases",
    tname = "year", 
    idname = "state_id",
    gname = "G",
    data = did_data,
    control_group = "notyettreated",  # Use not-yet-treated as control
    anticipation = 0,
    est_method = "reg",
    base_period = "universal"
  )
  
  cat("\n--- Group-Time ATT Estimates ---\n")
  summary(cs_out)
  
  # Aggregate to overall ATT
  agg_simple <- aggte(cs_out, type = "simple")
  cat("\n--- Simple Aggregation (Overall ATT) ---\n")
  summary(agg_simple)
  
  # Dynamic effects (event study)
  agg_dynamic <- aggte(cs_out, type = "dynamic")
  cat("\n--- Dynamic Effects (Event Study) ---\n")
  summary(agg_dynamic)
  
  # Plot event study
  p_cs <- ggdid(agg_dynamic) +
    labs(title = "Callaway-Sant'Anna Event Study",
         subtitle = "Effect of Agritourism Statute on Liability Cases") +
    theme_minimal()
  
  ggsave("../../3_Output/Figures/callaway_santanna_event_study.png", p_cs, width = 10, height = 6)
  cat("\nSaved: callaway_santanna_event_study.png\n")
  
}, error = function(e) {
  cat("\nCallaway-Sant'Anna estimation failed:", e$message, "\n")
  cat("This may be due to limited variation in the outcome or cohort structure.\n")
  cat("Proceeding with alternative approach...\n")
})

# ------------------------------------------------------------------------------
# 1D. Alternative: TWFE with Heterogeneity-Robust Estimator (Sun & Abraham)
# ------------------------------------------------------------------------------

cat("\n--- Alternative: fixest TWFE ---\n")

# Simple TWFE (for comparison, knowing its limitations)
twfe_simple <- feols(n_cases ~ treated | state_id + year, 
                      data = did_data)
cat("\nSimple TWFE:\n")
print(summary(twfe_simple))

# Sun & Abraham event study
did_data <- did_data %>%
  mutate(
    rel_time_factor = factor(rel_time),
    # Bin extreme values
    rel_time_binned = case_when(
      rel_time < -5 ~ -5,
      rel_time > 10 ~ 10,
      TRUE ~ rel_time
    )
  )

# Event study with fixest
tryCatch({
  es_fixest <- feols(n_cases ~ i(rel_time_binned, ref = -1) | state_id + year,
                      data = did_data %>% filter(rel_time_binned >= -5 & rel_time_binned <= 10))
  
  cat("\nEvent Study (fixest):\n")
  print(summary(es_fixest))
  
  # Plot
  iplot(es_fixest, main = "Event Study: Effect on Liability Cases")
  
}, error = function(e) {
  cat("fixest event study failed:", e$message, "\n")
})

# ==============================================================================
# PART 2: TOPIC VECTOR EQUILIBRATION
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("PART 2: TOPIC VECTOR EQUILIBRATION\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

# Load topic data
topics <- topic_data %>%
  select(state, enact_year, topic1_liability, topic2_tax, topic3_tourism, topic4_admin,
         adoption_wave, dominant_topic)

# ------------------------------------------------------------------------------
# 2A. Variance Decomposition Over Time
# ------------------------------------------------------------------------------

cat("\n--- Variance in Topic Proportions by Adoption Wave ---\n")

variance_by_wave <- topics %>%
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    # Variance in each topic
    var_liability = var(topic1_liability),
    var_tax = var(topic2_tax),
    var_tourism = var(topic3_tourism),
    var_admin = var(topic4_admin),
    # Total variance (sum across topics)
    total_var = var_liability + var_tax + var_tourism + var_admin,
    # Standard deviation of dominant topic assignment
    sd_liability = sd(topic1_liability),
    .groups = "drop"
  ) %>%
  arrange(adoption_wave)

print(variance_by_wave)

# Plot variance over time
var_long <- variance_by_wave %>%
  select(adoption_wave, var_liability, var_tax, var_tourism, var_admin) %>%
  pivot_longer(cols = starts_with("var_"), 
               names_to = "topic", 
               values_to = "variance",
               names_prefix = "var_")

p_variance <- ggplot(var_long, aes(x = adoption_wave, y = variance, fill = topic)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set2", 
                    labels = c("Administrative", "Liability", "Tax/Regulatory", "Tourism")) +
  labs(title = "Topic Variance Decreases Over Adoption Waves",
       subtitle = "Evidence of policy convergence",
       x = "Adoption Wave",
       y = "Variance in Topic Proportion",
       fill = "Topic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../../3_Output/Figures/topic_variance_by_wave.png", p_variance, width = 10, height = 6)
cat("\nSaved: topic_variance_by_wave.png\n")

# ------------------------------------------------------------------------------
# 2B. Distance to Centroid (Mean Statute)
# ------------------------------------------------------------------------------

cat("\n--- Distance to Centroid by Adoption Wave ---\n")

# Calculate overall centroid (mean topic vector)
centroid <- topics %>%
  summarize(
    c1 = mean(topic1_liability),
    c2 = mean(topic2_tax),
    c3 = mean(topic3_tourism),
    c4 = mean(topic4_admin)
  )

cat("Centroid (mean statute):\n")
cat("  Liability:", round(centroid$c1, 3), "\n")
cat("  Tax:", round(centroid$c2, 3), "\n")
cat("  Tourism:", round(centroid$c3, 3), "\n")
cat("  Admin:", round(centroid$c4, 3), "\n")

# Calculate Euclidean distance to centroid for each state
topics <- topics %>%
  mutate(
    dist_to_centroid = sqrt(
      (topic1_liability - centroid$c1)^2 +
      (topic2_tax - centroid$c2)^2 +
      (topic3_tourism - centroid$c3)^2 +
      (topic4_admin - centroid$c4)^2
    )
  )

# Mean distance by wave
dist_by_wave <- topics %>%
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    mean_dist = mean(dist_to_centroid),
    sd_dist = sd(dist_to_centroid),
    .groups = "drop"
  )

print(dist_by_wave)

# Plot
p_dist <- ggplot(dist_by_wave, aes(x = adoption_wave, y = mean_dist)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = mean_dist - sd_dist, ymax = mean_dist + sd_dist), 
                width = 0.2) +
  labs(title = "Distance to Mean Statute by Adoption Wave",
       subtitle = "Lower distance = more convergence toward common template",
       x = "Adoption Wave",
       y = "Mean Euclidean Distance to Centroid") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../../3_Output/Figures/distance_to_centroid.png", p_dist, width = 10, height = 6)
cat("\nSaved: distance_to_centroid.png\n")

# ------------------------------------------------------------------------------
# 2C. Entropy / Concentration of Dominant Topics
# ------------------------------------------------------------------------------

cat("\n--- Topic Concentration (Entropy) by Wave ---\n")

# Calculate Shannon entropy of topic distribution within each wave
entropy_by_wave <- topics %>%
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    # Proportion in each dominant topic category
    p_liability = mean(dominant_topic == "Liability"),
    p_tax = mean(dominant_topic == "Tax/Regulatory"),
    p_tourism = mean(dominant_topic == "Tourism"),
    p_admin = mean(dominant_topic == "Administrative"),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    # Shannon entropy (higher = more dispersed, lower = more concentrated)
    entropy = {
      probs <- c(p_liability, p_tax, p_tourism, p_admin)
      probs <- probs[probs > 0]  # Remove zeros
      -sum(probs * log(probs))
    },
    # Herfindahl index (higher = more concentrated)
    herfindahl = p_liability^2 + p_tax^2 + p_tourism^2 + p_admin^2
  ) %>%
  ungroup()

print(entropy_by_wave)

# Plot entropy
p_entropy <- ggplot(entropy_by_wave, aes(x = adoption_wave, y = entropy)) +
  geom_col(fill = "coral") +
  geom_hline(yintercept = log(4), linetype = "dashed", color = "gray50") +
  annotate("text", x = 3.5, y = log(4) + 0.05, label = "Maximum entropy (uniform)", 
           size = 3, color = "gray50") +
  labs(title = "Topic Entropy Decreases Over Adoption Waves",
       subtitle = "Lower entropy = states converging on fewer dominant topics",
       x = "Adoption Wave",
       y = "Shannon Entropy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../../3_Output/Figures/topic_entropy_by_wave.png", p_entropy, width = 10, height = 6)
cat("\nSaved: topic_entropy_by_wave.png\n")

# ------------------------------------------------------------------------------
# 2D. Pairwise Similarity Over Time
# ------------------------------------------------------------------------------

cat("\n--- Within-Wave vs Across-Wave Similarity ---\n")

# Load similarity matrix
sim_matrix <- read_csv("../../2_Data/STM/similarity_matrix.csv")

# Add wave information
sim_with_waves <- sim_matrix %>%
  left_join(topics %>% select(state, adoption_wave), by = c("state1" = "state")) %>%
  rename(wave1 = adoption_wave) %>%
  left_join(topics %>% select(state, adoption_wave), by = c("state2" = "state")) %>%
  rename(wave2 = adoption_wave) %>%
  mutate(same_wave = wave1 == wave2)

# Compare within-wave vs across-wave similarity
similarity_comparison <- sim_with_waves %>%
  group_by(same_wave) %>%
  summarize(
    n_pairs = n(),
    mean_similarity = mean(similarity),
    sd_similarity = sd(similarity),
    .groups = "drop"
  )

print(similarity_comparison)

# Within-wave similarity by wave
within_wave_sim <- sim_with_waves %>%
  filter(same_wave) %>%
  group_by(wave1) %>%
  summarize(
    n_pairs = n(),
    mean_similarity = mean(similarity),
    .groups = "drop"
  )

cat("\nWithin-wave similarity:\n")
print(within_wave_sim)

# ------------------------------------------------------------------------------
# 2E. Cumulative Convergence Plot
# ------------------------------------------------------------------------------

cat("\n--- Cumulative Convergence ---\n")

# Order states by enactment year
topics_ordered <- topics %>%
  arrange(enact_year) %>%
  mutate(order = row_number())

# Calculate cumulative centroid and distance for each state
cumulative_convergence <- topics_ordered %>%
  mutate(
    # Running mean of topic proportions (centroid up to that point)
    cum_mean_t1 = cummean(topic1_liability),
    cum_mean_t2 = cummean(topic2_tax),
    cum_mean_t3 = cummean(topic3_tourism),
    cum_mean_t4 = cummean(topic4_admin),
    # Distance of this state to cumulative centroid
    dist_to_cum_centroid = sqrt(
      (topic1_liability - lag(cum_mean_t1, default = 0))^2 +
      (topic2_tax - lag(cum_mean_t2, default = 0))^2 +
      (topic3_tourism - lag(cum_mean_t3, default = 0))^2 +
      (topic4_admin - lag(cum_mean_t4, default = 0))^2
    )
  )

# Plot cumulative convergence
p_cumulative <- ggplot(cumulative_convergence %>% filter(order > 1), 
                        aes(x = enact_year, y = dist_to_cum_centroid)) +
  geom_point(aes(color = adoption_wave), size = 3) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed") +
  geom_text(aes(label = state), hjust = -0.1, vjust = 0, size = 2, check_overlap = TRUE) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Policy Convergence Over Time",
       subtitle = "Distance of each state to cumulative mean of prior adopters",
       x = "Year of Enactment",
       y = "Distance to Prior Adopters' Centroid",
       color = "Wave") +
  theme_minimal()

ggsave("../../3_Output/Figures/cumulative_convergence.png", p_cumulative, width = 12, height = 8)
cat("\nSaved: cumulative_convergence.png\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("ANALYSIS SUMMARY\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

cat("\nPART 1: Treatment Effects (Callaway-Sant'Anna)\n")
cat("  - Panel: ", nrow(did_data), " state-years\n")
cat("  - Outcome: Liability/negligence case counts\n")
cat("  - See event study plots for dynamic effects\n")

cat("\nPART 2: Topic Equilibration\n")
cat("  - Variance by wave: ", 
    round(variance_by_wave$total_var[1], 4), " (Wave 1) → ",
    round(variance_by_wave$total_var[nrow(variance_by_wave)], 4), " (Wave 4)\n")
cat("  - Distance to centroid: ",
    round(dist_by_wave$mean_dist[1], 3), " (Wave 1) → ",
    round(dist_by_wave$mean_dist[nrow(dist_by_wave)], 3), " (Wave 4)\n")
cat("  - Entropy: ",
    round(entropy_by_wave$entropy[1], 3), " (Wave 1) → ",
    round(entropy_by_wave$entropy[nrow(entropy_by_wave)], 3), " (Wave 4)\n")

cat("\n=== OUTPUT FILES ===\n")
cat("  event_study_raw.png\n")
cat("  callaway_santanna_event_study.png (if estimation succeeded)\n")
cat("  topic_variance_by_wave.png\n")
cat("  distance_to_centroid.png\n")
cat("  topic_entropy_by_wave.png\n")
cat("  cumulative_convergence.png\n")

# Save data
write_csv(panel, "../../4_Analysis_Results/did_panel_data.csv")
write_csv(convergence_data, "../../4_Analysis_Results/convergence_analysis.csv"))
cat("\n  did_panel_data.csv\n")
cat("  convergence_analysis.csv\n")
