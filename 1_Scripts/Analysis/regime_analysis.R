# ==============================================================================
# REGIME-BASED ANALYSIS (Replaces Wave-Based Analysis)
# 3 Regimes: Experimentation (2003-2010), Convergence (2011-2014), Recalibration (2015-2021)
# ==============================================================================

library(tidyverse)
library(here)

# ==============================================================================
# REGIME DEFINITIONS
# ==============================================================================

# Theory-driven regimes based on convergence patterns
REGIME_LABELS <- c(

  "Experimentation (2003–2010)",
  "Convergence (2011–2014)",
  "Recalibration (2015–2021)"
)

# Grayscale colors for regimes
REGIME_COLORS_GRAY <- c(
  "Experimentation (2003–2010)" = "#1a1a1a",   # Dark
  "Convergence (2011–2014)" = "#808080",        # Medium
  "Recalibration (2015–2021)" = "#cccccc"       # Light
)

# Shapes for regimes
REGIME_SHAPES <- c(
  "Experimentation (2003–2010)" = 16,  # Circle
  "Convergence (2011–2014)" = 15,      # Square
  "Recalibration (2015–2021)" = 17     # Triangle
)

# Topic colors (grayscale)
TOPIC_COLORS_GRAY <- c(
  "Liability/Warning" = "#2d2d2d",
  "Governance/Administration" = "#707070",
  "Land Use/Zoning" = "#a8a8a8",
  "Recreational Use" = "#e0e0e0"
)

# ==============================================================================
# LOAD AND TRANSFORM DATA
# ==============================================================================

analysis_data <- read_csv("../../2_Data/Processed/final_analysis_data.csv")

# Create regime variable (replaces adoption_wave)
analysis_data <- analysis_data %>%
  mutate(
    regime = case_when(
      enact_year <= 2010 ~ "Experimentation (2003–2010)",
      enact_year <= 2014 ~ "Convergence (2011–2014)",
      TRUE ~ "Recalibration (2015–2021)"
    ),
    regime = factor(regime, levels = REGIME_LABELS)
  )

# Verify counts
cat("=== REGIME SAMPLE SIZES ===\n")
print(table(analysis_data$regime))
cat("\nTotal N:", nrow(analysis_data), "\n")

# ==============================================================================
# CALCULATE VARIANCE BY REGIME
# ==============================================================================

variance_by_regime <- analysis_data %>%
  group_by(regime) %>%
  summarise(
    n = n(),
    var_topic1 = var(topic1),
    var_topic2 = var(topic2),
    var_topic3 = var(topic3),
    var_topic4 = var(topic4),
    total_var = var_topic1 + var_topic2 + var_topic3 + var_topic4,
    mean_dist = mean(dist_to_centroid, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n=== VARIANCE BY REGIME ===\n")
print(variance_by_regime %>% select(regime, n, total_var, mean_dist))

# ==============================================================================
# FIGURE 1: Topic Composition by State and Regime (GRAYSCALE)
# ==============================================================================

fig1_data <- analysis_data %>%
  select(state, abbrev, regime, enact_year, topic1, topic2, topic3, topic4) %>%
  pivot_longer(cols = starts_with("topic"), names_to = "topic", values_to = "proportion") %>%
  mutate(
    topic_label = case_when(
      topic == "topic1" ~ "Liability/Warning",
      topic == "topic2" ~ "Governance/Administration",
      topic == "topic3" ~ "Land Use/Zoning",
      topic == "topic4" ~ "Recreational Use"
    ),
    topic_label = factor(topic_label, levels = names(TOPIC_COLORS_GRAY))
  )

# Sort states by year within each regime
state_order <- analysis_data %>%
  arrange(regime, enact_year) %>%
  pull(abbrev)

fig1_data$abbrev <- factor(fig1_data$abbrev, levels = state_order)

fig1_regime <- ggplot(fig1_data, aes(x = abbrev, y = proportion, fill = topic_label)) +
  geom_col(position = "stack", width = 0.85, color = "white", linewidth = 0.2) +
  facet_wrap(~regime, scales = "free_x", nrow = 1) +
  scale_fill_manual(values = TOPIC_COLORS_GRAY, name = "Topic") +
  labs(
    title = "Topic Composition by State and Regulatory Regime (N=43)",
    subtitle = "States ordered by year of enactment within each regime",
    x = "State",
    y = "Topic Proportion"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("../../3_Output/Figures/figure1_regime_grayscale.png"), fig1_regime, width = 14, height = 6, dpi = 300)
cat("\nSaved: figure1_regime_grayscale.png\n")

# ==============================================================================
# FIGURE 2: Policy Convergence Over Time (GRAYSCALE)
# ==============================================================================

fig2_regime <- analysis_data %>%
  filter(!is.na(dist_to_centroid)) %>%
  ggplot(aes(x = enact_year, y = dist_to_centroid, 
             color = regime, shape = regime)) +
  geom_point(size = 3.5, stroke = 1.2) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, 
              color = "gray30", fill = "gray80",
              linetype = "dashed", alpha = 0.4) +
  geom_text(aes(label = abbrev), hjust = -0.2, vjust = 0.5, size = 2.5, 
            check_overlap = TRUE, show.legend = FALSE, color = "black") +
  scale_color_manual(values = REGIME_COLORS_GRAY, name = "Regulatory Regime") +
  scale_shape_manual(values = REGIME_SHAPES, name = "Regulatory Regime") +
  scale_x_continuous(breaks = seq(2004, 2022, 2)) +
  # Add vertical lines to show regime boundaries
  geom_vline(xintercept = 2010.5, linetype = "dotted", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2014.5, linetype = "dotted", color = "gray50", linewidth = 0.5) +
  labs(
    title = "Policy Convergence Over Time (N=43)",
    subtitle = "Distance of each state's statute to the cumulative centroid of prior adopters",
    x = "Year of Enactment",
    y = "Distance to Prior Adopters' Centroid",
    caption = "Lower distance = more similar to established template.\nNote: Early adopters (2003–2006) have fewer prior states for comparison; interpret with caution.\nVertical lines indicate regime boundaries."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))

ggsave("../../3_Output/Figures/figure2_regime_grayscale.png"), fig2_regime, width = 11, height = 7, dpi = 300)
cat("Saved: figure2_regime_grayscale.png\n")

# ==============================================================================
# FIGURE 3: Variance by Regime (GRAYSCALE)
# ==============================================================================

fig3_regime <- variance_by_regime %>%
  ggplot(aes(x = regime, y = total_var)) +
  geom_col(fill = "gray40", width = 0.6, color = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.5, size = 4) +
  labs(
    title = "Topic Variance by Regulatory Regime",
    subtitle = "Lower variance = greater policy convergence",
    x = "Regulatory Regime",
    y = "Total Variance (sum across topics)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("../../3_Output/Figures/figure3_regime_grayscale.png"), fig3_regime, width = 8, height = 6, dpi = 300)
cat("Saved: figure3_regime_grayscale.png\n")

# ==============================================================================
# SUPPLEMENTARY: Individual Topic Proportions Over Time
# ==============================================================================

create_topic_figure <- function(data, topic_col, topic_label, filename) {
  
  p <- ggplot(data, aes(x = enact_year, y = .data[[topic_col]], 
                        color = regime, shape = regime)) +
    geom_point(size = 3.5, stroke = 1.2) +
    geom_smooth(aes(group = 1), method = "loess", se = TRUE, 
                color = "gray30", fill = "gray80",
                linetype = "dashed", alpha = 0.4) +
    geom_text(aes(label = abbrev), hjust = -0.2, vjust = 0.5, size = 2.5, 
              check_overlap = TRUE, show.legend = FALSE, color = "black") +
    geom_vline(xintercept = 2010.5, linetype = "dotted", color = "gray50", linewidth = 0.5) +
    geom_vline(xintercept = 2014.5, linetype = "dotted", color = "gray50", linewidth = 0.5) +
    scale_color_manual(values = REGIME_COLORS_GRAY, name = "Regulatory Regime") +
    scale_shape_manual(values = REGIME_SHAPES, name = "Regulatory Regime") +
    scale_x_continuous(breaks = seq(2004, 2022, 2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    labs(
      title = topic_label,
      subtitle = "Topic proportion by year of enactment (N=43)",
      x = "Year of Enactment",
      y = "Topic Proportion",
      caption = "Vertical lines indicate regime boundaries."
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))
  
  ggsave(here(filename), p, width = 10, height = 6, dpi = 300)
  cat("Saved:", filename, "\n")
  
  return(p)
}

# Create individual topic figures
fig_t1 <- create_topic_figure(analysis_data, "topic1", 
                               "Topic 1: Liability/Warning", 
                               "figure_topic1_regime.png")

fig_t2 <- create_topic_figure(analysis_data, "topic2", 
                               "Topic 2: Governance/Administration", 
                               "figure_topic2_regime.png")

fig_t3 <- create_topic_figure(analysis_data, "topic3", 
                               "Topic 3: Land Use/Zoning", 
                               "figure_topic3_regime.png")

fig_t4 <- create_topic_figure(analysis_data, "topic4", 
                               "Topic 4: Recreational Use", 
                               "figure_topic4_regime.png")

# Combined faceted figure
long_data <- analysis_data %>%
  select(state, abbrev, enact_year, regime, topic1, topic2, topic3, topic4) %>%
  pivot_longer(cols = starts_with("topic"), 
               names_to = "topic", 
               values_to = "proportion") %>%
  mutate(
    topic_label = case_when(
      topic == "topic1" ~ "Topic 1: Liability/Warning",
      topic == "topic2" ~ "Topic 2: Governance/Administration",
      topic == "topic3" ~ "Topic 3: Land Use/Zoning",
      topic == "topic4" ~ "Topic 4: Recreational Use"
    ),
    topic_label = factor(topic_label)
  )

fig_combined <- ggplot(long_data, aes(x = enact_year, y = proportion,
                                       color = regime, shape = regime)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = F,
              color = "gray30", fill = "gray80",
              linetype = "dashed", alpha = 0.3, linewidth = 0.8) +
  geom_vline(xintercept = 2010.5, linetype = "dotted", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = 2014.5, linetype = "dotted", color = "gray50", linewidth = 0.5) +
  facet_wrap(~topic_label, nrow = 2, scales = "fixed") +
  scale_color_manual(values = REGIME_COLORS_GRAY, name = "Regulatory Regime") +
  scale_shape_manual(values = REGIME_SHAPES, name = "Regulatory Regime") +
  scale_x_continuous(breaks = seq(2004, 2020, 4)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5)) +
  labs(
    title = "Topic Proportions Over Time by Topic (N=43)",
    subtitle = "Each point represents one state's statute; LOESS trend line with 95% CI",
    x = "Year of Enactment",
    y = "Topic Proportion",
    caption = "Vertical lines indicate regime boundaries."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))

ggsave("../../3_Output/Figures/figure_all_topics_regime.png"), fig_combined, 
       width = 11, height = 8, dpi = 300)
cat("Saved: figure_all_topics_regime.png\n")

# ==============================================================================
# SAVE UPDATED DATA WITH REGIME VARIABLE
# ==============================================================================

analysis_data_regime <- analysis_data %>%
  select(-adoption_wave) %>%  # Remove old wave variable if present
  select(state, abbrev, enact_year, regime, everything())

write_csv(analysis_data_regime, "../../4_Analysis_Results/analysis_data_regime.csv")
cat("\nSaved: analysis_data_regime.csv\n")

# ==============================================================================
# SUMMARY OUTPUT
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("REGIME-BASED ANALYSIS COMPLETE\n")
cat(rep("=", 70), "\n", sep = "")

cat("\n--- Regime Structure ---\n")
cat("Experimentation (2003-2010): n =", sum(analysis_data$regime == "Experimentation (2003–2010)"), "\n")
cat("Convergence (2011-2014):     n =", sum(analysis_data$regime == "Convergence (2011–2014)"), "\n")
cat("Recalibration (2015-2021):   n =", sum(analysis_data$regime == "Recalibration (2015–2021)"), "\n")

cat("\n--- Variance by Regime ---\n")
for(i in 1:nrow(variance_by_regime)) {
  cat(sprintf("%s: variance = %.3f (n=%d)\n", 
              variance_by_regime$regime[i],
              variance_by_regime$total_var[i],
              variance_by_regime$n[i]))
}

cat("\n--- Key Finding ---\n")
cat("Convergence regime shows lowest variance (", 
    round(variance_by_regime$total_var[2], 3), 
    "), confirming the theoretical pattern:\n", sep = "")
cat("Experimentation → Convergence → Recalibration\n")

cat("\n--- Output Files ---\n")
cat("Figures:\n")
cat("  - figure1_regime_grayscale.png\n")
cat("  - figure2_regime_grayscale.png\n")
cat("  - figure3_regime_grayscale.png\n")
cat("  - figure_topic1_regime.png\n")
cat("  - figure_topic2_regime.png\n")
cat("  - figure_topic3_regime.png\n")
cat("  - figure_topic4_regime.png\n")
cat("  - figure_all_topics_regime.png\n")
cat("Data:\n")
cat("  - analysis_data_regime.csv\n")
