# ==============================================================================
# SUPPLEMENTARY FIGURES: Individual Topic Proportions Over Time
# Creates 4 figures (one per topic) + 1 combined faceted figure
# ==============================================================================

library(tidyverse)
library(here)

# Load data
analysis_data <- read_csv("../../2_Data/Processed/final_analysis_data.csv")

# Topic labels
TOPIC_LABELS <- c(
  "topic1" = "Topic 1: Liability/Warning",
  "topic2" = "Topic 2: Governance/Administration", 
  "topic3" = "Topic 3: Land Use/Zoning",
  "topic4" = "Topic 4: Recreational Use"
)

# Grayscale wave colors and shapes
WAVE_COLORS_GRAY <- c(
  "Wave 1 (2003-2006)" = "#1a1a1a",
  "Wave 2 (2007-2010)" = "#4d4d4d",
  "Wave 3 (2011-2014)" = "#808080",
  "Wave 4 (2015-2018)" = "#b3b3b3",
  "Wave 5 (2019-2021)" = "#333333"
)

WAVE_SHAPES <- c(
  "Wave 1 (2003-2006)" = 16,
  "Wave 2 (2007-2010)" = 17,
  "Wave 3 (2011-2014)" = 15,
  "Wave 4 (2015-2018)" = 18,
  "Wave 5 (2019-2021)" = 8
)

# Ensure wave is ordered factor
analysis_data <- analysis_data %>%
  mutate(adoption_wave = factor(adoption_wave, levels = names(WAVE_COLORS_GRAY)))

# ==============================================================================
# INDIVIDUAL TOPIC FIGURES
# ==============================================================================

create_topic_figure <- function(data, topic_col, topic_label, filename) {
  
  p <- ggplot(data, aes(x = enact_year, y = .data[[topic_col]], 
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
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
    labs(
      title = topic_label,
      subtitle = "Topic proportion by year of enactment (N=43)",
      x = "Year of Enactment",
      y = "Topic Proportion"
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

# Create individual figures
fig_t1 <- create_topic_figure(analysis_data, "topic1", 
                               "Topic 1: Liability/Warning", 
                               "figure_topic1_over_time.png")

fig_t2 <- create_topic_figure(analysis_data, "topic2", 
                               "Topic 2: Governance/Administration", 
                               "figure_topic2_over_time.png")

fig_t3 <- create_topic_figure(analysis_data, "topic3", 
                               "Topic 3: Land Use/Zoning", 
                               "figure_topic3_over_time.png")

fig_t4 <- create_topic_figure(analysis_data, "topic4", 
                               "Topic 4: Recreational Use", 
                               "figure_topic4_over_time.png")

# ==============================================================================
# COMBINED FACETED FIGURE (All 4 topics in one figure)
# ==============================================================================

# Reshape to long format
long_data <- analysis_data %>%
  select(state, abbrev, enact_year, adoption_wave, topic1, topic2, topic3, topic4) %>%
  pivot_longer(cols = starts_with("topic"), 
               names_to = "topic", 
               values_to = "proportion") %>%
  mutate(topic_label = recode(topic, !!!TOPIC_LABELS),
         topic_label = factor(topic_label, levels = TOPIC_LABELS))

fig_combined <- ggplot(long_data, aes(x = enact_year, y = proportion,
                                       color = adoption_wave, shape = adoption_wave)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = F,
              color = "gray30", fill = "gray80",
              linetype = "dashed", alpha = 0.3, linewidth = 0.8) +
  facet_wrap(~topic_label, nrow = 2, scales = "fixed") +
  scale_color_manual(values = WAVE_COLORS_GRAY, name = "Adoption Wave") +
  scale_shape_manual(values = WAVE_SHAPES, name = "Adoption Wave") +
  scale_x_continuous(breaks = seq(2004, 2020, 4)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5)) +
  labs(
    title = "Topic Proportions Over Time by Topic (N=43)",
    subtitle = "Each point represents one state's statute; LOESS trend line with 95% CI",
    x = "Year of Enactment",
    y = "Topic Proportion"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10),
    plot.title = element_text(size = 14, face = "bold")
  ) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))

ggsave("../../3_Output/Figures/figure_all_topics_over_time.png"), fig_combined, 
       width = 11, height = 8, dpi = 300)
cat("Saved: figure_all_topics_over_time.png\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat(rep("=", 60), "\n", sep = "")
cat("SUPPLEMENTARY TOPIC FIGURES CREATED\n")
cat(rep("=", 60), "\n", sep = "")
cat("\nFiles saved:\n")
cat("  - figure_topic1_over_time.png (Liability/Warning)\n")
cat("  - figure_topic2_over_time.png (Governance/Administration)\n")
cat("  - figure_topic3_over_time.png (Land Use/Zoning)\n")
cat("  - figure_topic4_over_time.png (Recreational Use)\n")
cat("  - figure_all_topics_over_time.png (Combined 2x2 faceted)\n")
cat("\nThese figures show raw topic proportions over time,\n")
cat("which is easier to interpret than the composite distance measure.\n")
