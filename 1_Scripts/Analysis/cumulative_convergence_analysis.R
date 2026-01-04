# ==============================================================================
# Cumulative Convergence Plot - Full Sample (N=43)
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(here)

# Load the analysis data (assuming it's already been created)
# If not, run the main analysis script first

# This should be run AFTER the main SPPQ_Full_Sample_Analysis.R script

# ------------------------------------------------------------------------------
# CUMULATIVE CONVERGENCE ANALYSIS
# ------------------------------------------------------------------------------

cat("=== CUMULATIVE CONVERGENCE ANALYSIS ===\n\n")

# Order states by enactment year
analysis_ordered <- analysis_data %>%
  arrange(enact_year) %>%
  mutate(order = row_number())

# Calculate cumulative centroid and distance for each state
cumulative_convergence <- analysis_ordered %>%
  mutate(
    # Running mean of topic proportions (centroid up to that point)
    cum_mean_t1 = cummean(topic1),
    cum_mean_t2 = cummean(topic2),
    cum_mean_t3 = cummean(topic3),
    cum_mean_t4 = cummean(topic4),
    # Distance of this state to cumulative centroid of PRIOR adopters
    # Use lag() to get the centroid BEFORE this state was added
    dist_to_cum_centroid = sqrt(
      (topic1 - lag(cum_mean_t1, default = topic1[1]))^2 +
      (topic2 - lag(cum_mean_t2, default = topic2[1]))^2 +
      (topic3 - lag(cum_mean_t3, default = topic3[1]))^2 +
      (topic4 - lag(cum_mean_t4, default = topic4[1]))^2
    )
  )

# Print summary
cat("Cumulative convergence by wave:\n")
cumulative_convergence %>%
  filter(order > 1) %>%  # Exclude first state (no prior centroid)
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    mean_dist = mean(dist_to_cum_centroid),
    sd_dist = sd(dist_to_cum_centroid),
    min_dist = min(dist_to_cum_centroid),
    max_dist = max(dist_to_cum_centroid),
    .groups = "drop"
  ) %>%
  print()

# ------------------------------------------------------------------------------
# MAIN CUMULATIVE CONVERGENCE PLOT
# ------------------------------------------------------------------------------

p_cumulative <- ggplot(cumulative_convergence %>% filter(order > 1), 
                        aes(x = enact_year, y = dist_to_cum_centroid)) +
  geom_point(aes(color = adoption_wave), size = 3, alpha = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "black", linetype = "dashed", 
              span = 0.75) +
  geom_text(aes(label = state), hjust = -0.1, vjust = 0.5, size = 2.5, 
            check_overlap = TRUE, alpha = 0.7) +
  scale_color_brewer(palette = "Set1", name = "Adoption Wave") +
  scale_x_continuous(breaks = seq(2004, 2021, 2)) +
  labs(title = "Policy Convergence Over Time (N=43)",
       subtitle = "Distance of each state's statute to the cumulative centroid of prior adopters",
       x = "Year of Enactment",
       y = "Distance to Prior Adopters' Centroid",
       caption = "Lower distance = more similar to established template") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave("../../3_Output/Figures/figure_cumulative_convergence_full.png"), p_cumulative, 
       width = 12, height = 8, dpi = 300)
cat("\nSaved: figure_cumulative_convergence_full.png\n")

# ------------------------------------------------------------------------------
# ALTERNATIVE: FACETED BY WAVE
# ------------------------------------------------------------------------------

p_faceted <- ggplot(cumulative_convergence %>% filter(order > 1), 
                     aes(x = order, y = dist_to_cum_centroid)) +
  geom_line(color = "gray50", alpha = 0.5) +
  geom_point(aes(color = adoption_wave), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgreen", alpha = 0.5) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Cumulative Convergence: State-by-State",
       subtitle = "Each point shows how similar a state is to all prior adopters",
       x = "Order of Adoption (1 = earliest)",
       y = "Distance to Cumulative Centroid",
       color = "Wave") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("../../3_Output/Figures/figure_cumulative_convergence_sequential.png"), p_faceted, 
       width = 12, height = 6, dpi = 300)
cat("Saved: figure_cumulative_convergence_sequential.png\n")

# ------------------------------------------------------------------------------
# IDENTIFY OUTLIERS (states that diverge from template)
# ------------------------------------------------------------------------------

cat("\n--- States Most Different from Template ---\n")
cumulative_convergence %>%
  filter(order > 5) %>%  # After template established
  arrange(desc(dist_to_cum_centroid)) %>%
  select(state, enact_year, adoption_wave, dist_to_cum_centroid, 
         topic1, topic2, topic3, topic4) %>%
  head(10) %>%
  print()

cat("\n--- States Most Similar to Template ---\n")
cumulative_convergence %>%
  filter(order > 5) %>%
  arrange(dist_to_cum_centroid) %>%
  select(state, enact_year, adoption_wave, dist_to_cum_centroid,
         topic1, topic2, topic3, topic4) %>%
  head(10) %>%
  print()

# ------------------------------------------------------------------------------
# TREND LINE: Is convergence increasing over time?
# ------------------------------------------------------------------------------

cat("\n--- Convergence Trend Analysis ---\n")

# Correlation between adoption order and distance to centroid
trend_test <- cor.test(
  cumulative_convergence$order[cumulative_convergence$order > 1],
  cumulative_convergence$dist_to_cum_centroid[cumulative_convergence$order > 1]
)

cat("Correlation (order vs distance):", round(trend_test$estimate, 3), "\n")
cat("p-value:", round(trend_test$p.value, 4), "\n")

if (trend_test$estimate < 0 & trend_test$p.value < 0.05) {
  cat("\n✓ SIGNIFICANT CONVERGENCE: Later adopters are MORE similar to the template\n")
} else if (trend_test$estimate > 0 & trend_test$p.value < 0.05) {
  cat("\n✗ DIVERGENCE: Later adopters are LESS similar to the template\n")
} else {
  cat("\n○ No significant trend in convergence over time\n")
}

# ------------------------------------------------------------------------------
# SAVE DATA
# ------------------------------------------------------------------------------

write_csv(cumulative_convergence, "../../4_Analysis_Results/cumulative_convergence_full.csv")
cat("\nSaved: cumulative_convergence_full.csv\n")
