# ==============================================================================
# GRAYSCALE FIGURES FOR PUBLICATION
# Run this AFTER CONSOLIDATED_ANALYSIS.R has been run
# ==============================================================================

library(tidyverse)
library(here)

# ==============================================================================
# GRAYSCALE COLOR PALETTES
# ==============================================================================

# For Figure 1: 4 distinct grayscale fills with patterns via alpha
TOPIC_COLORS_GRAY <- c(
  "Liability/Warning" = "#2d2d2d",           # Dark gray
  "Governance/Administration" = "#707070",   # Medium gray
  "Land Use/Zoning" = "#a8a8a8",             # Light gray
  "Recreational Use" = "#e0e0e0"             # Very light gray
)

# For Figure 2: 5 waves in grayscale
WAVE_COLORS_GRAY <- c(
  "Wave 1 (2003-2006)" = "#1a1a1a",  # Darkest
  "Wave 2 (2007-2010)" = "#4d4d4d",
  "Wave 3 (2011-2014)" = "#808080",
  "Wave 4 (2015-2018)" = "#b3b3b3",
  "Wave 5 (2019-2021)" = "#e6e6e6"   # Lightest
)

# Wave shapes for additional differentiation
WAVE_SHAPES <- c(
  "Wave 1 (2003-2006)" = 16,  # Filled circle
  "Wave 2 (2007-2010)" = 17,  # Filled triangle
  "Wave 3 (2011-2014)" = 15,  # Filled square
  "Wave 4 (2015-2018)" = 18,  # Filled diamond
  "Wave 5 (2019-2021)" = 8    # Asterisk
)

# ==============================================================================
# LOAD DATA
# ==============================================================================

analysis_data <- read_csv("../../2_Data/Processed/final_analysis_data.csv")

# Calculate variance by wave (needed for Figure 3)
variance_by_wave <- analysis_data %>%
  group_by(adoption_wave) %>%
  summarise(
    n = n(),
    var_topic1 = var(topic1),
    var_topic2 = var(topic2),
    var_topic3 = var(topic3),
    var_topic4 = var(topic4),
    total_var = var_topic1 + var_topic2 + var_topic3 + var_topic4,
    mean_dist = mean(dist_to_centroid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(adoption_wave = factor(adoption_wave, levels = c(
    "Wave 1 (2003-2006)", "Wave 2 (2007-2010)", "Wave 3 (2011-2014)",
    "Wave 4 (2015-2018)", "Wave 5 (2019-2021)"
  )))

# ==============================================================================
# FIGURE 1: Topic Composition by State and Wave (GRAYSCALE)
# ==============================================================================

fig1_data <- analysis_data %>%
  select(state, abbrev, adoption_wave, topic1, topic2, topic3, topic4) %>%
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

# Sort states by dominant topic within each wave
state_order <- analysis_data %>%
  group_by(adoption_wave) %>%
  arrange(adoption_wave, desc(topic1)) %>%
  pull(abbrev)

fig1_data$abbrev <- factor(fig1_data$abbrev, levels = state_order)

fig1_gray <- ggplot(fig1_data, aes(x = abbrev, y = proportion, fill = topic_label)) +
  geom_col(position = "stack", width = 0.85, color = "white", linewidth = 0.2) +
  facet_wrap(~adoption_wave, scales = "free_x", nrow = 2) +
  scale_fill_manual(values = TOPIC_COLORS_GRAY, name = "Topic") +
  labs(
    title = "Topic Composition by State and Adoption Wave (N=43)",
    subtitle = "States sorted by dominant topic proportion within each wave",
    x = "State",
    y = "Topic Proportion"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("../../3_Output/Figures/figure1_grayscale.png"), fig1_gray, width = 12, height = 8, dpi = 300)
cat("Saved: figure1_grayscale.png\n")

# ==============================================================================
# FIGURE 2: Policy Convergence Over Time (GRAYSCALE)
# ==============================================================================

fig2_gray <- analysis_data %>%
  filter(!is.na(dist_to_centroid)) %>%
  mutate(adoption_wave = factor(adoption_wave, levels = names(WAVE_COLORS_GRAY))) %>%
  ggplot(aes(x = enact_year, y = dist_to_centroid, 
             color = adoption_wave, shape = adoption_wave)) +
  geom_point(size = 3.5, stroke = 1.2) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, 
              color = "gray30", fill = "gray80",
              linetype = "dashed", alpha = 0.4) +
  geom_text(aes(label = abbrev), hjust = -0.2, vjust = 0.5, size = 2.5, 
            check_overlap = TRUE, show.legend = FALSE, color = "black") +
  scale_color_manual(values = WAVE_COLORS_GRAY, name = "Adoption Wave") +
  scale_shape_manual(values = WAVE_SHAPES, name = "Adoption Wave") +
  scale_x_continuous(breaks = seq(2004, 2022, 2)) +
  labs(
    title = "Policy Convergence Over Time (N=43)",
    subtitle = "Distance of each state's statute to the cumulative centroid of prior adopters",
    x = "Year of Enactment",
    y = "Distance to Prior Adopters' Centroid",
    caption = "Lower distance = more similar to established template"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))

ggsave("../../3_Output/Figures/figure2_grayscale.png"), fig2_gray, width = 11, height = 7, dpi = 300)
cat("Saved: figure2_grayscale.png\n")

# ==============================================================================
# FIGURE 3: Variance by Wave (GRAYSCALE)
# ==============================================================================

fig3_gray <- variance_by_wave %>%
  ggplot(aes(x = adoption_wave, y = total_var)) +
  geom_col(fill = "gray40", width = 0.7, color = "black", linewidth = 0.3) +
  geom_text(aes(label = sprintf("n=%d", n)), vjust = -0.5, size = 3.5) +
  labs(
    title = "Topic Variance by Adoption Wave",
    subtitle = "Lower variance = greater convergence",
    x = "Adoption Wave",
    y = "Total Variance (sum across topics)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("../../3_Output/Figures/figure3_grayscale.png"), fig3_gray, width = 9, height = 6, dpi = 300)
cat("Saved: figure3_grayscale.png\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat(rep("=", 60), "\n", sep = "")
cat("GRAYSCALE FIGURES CREATED\n")
cat(rep("=", 60), "\n", sep = "")
cat("\nFiles saved:\n")
cat("  - figure1_grayscale.png (Topic composition by state/wave)\n")
cat("  - figure2_grayscale.png (Convergence over time)\n")
cat("  - figure3_grayscale.png (Variance by wave)\n")
cat("\nThese figures use:\n")
cat("  - Distinct gray shades for topics (dark to light)\n")
cat("  - Different point shapes for waves in Figure 2\n")
cat("  - High contrast for print reproduction\n")
