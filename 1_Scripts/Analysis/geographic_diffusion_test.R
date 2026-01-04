# ==============================================================================
# Geographic Diffusion Test
# Do states copy neighbors more than non-neighbors?
# ==============================================================================

library(tidyverse)
library(sf)
library(spdep)
library(here)

# Load data
sim_matrix <- read_csv("../../2_Data/STM/similarity_matrix.csv")
topic_data <- read_csv("../../2_Data/Processed/final_analysis_data.csv")

# ==============================================================================
# 1. BUILD CONTIGUITY MATRIX
# ==============================================================================

# State adjacency (manually defined for lower 48 + relevant states)
# This is a standard contiguity list

adjacency_list <- list(
  "Alabama" = c("Florida", "Georgia", "Mississippi", "Tennessee"),
  "Arkansas" = c("Louisiana", "Mississippi", "Missouri", "Oklahoma", "Tennessee", "Texas"),
  "Colorado" = c("Kansas", "Nebraska", "Oklahoma", "Utah"),
  "Florida" = c("Alabama", "Georgia"),
  "Georgia" = c("Alabama", "Florida", "North Carolina", "South Carolina", "Tennessee"),
  "Hawaii" = character(0),  # No neighbors
  "Idaho" = c("Montana", "Oregon", "Utah", "Washington"),
  "Illinois" = c("Indiana", "Kentucky", "Missouri", "Wisconsin"),
  "Indiana" = c("Illinois", "Kentucky", "Michigan", "Ohio"),
  "Kansas" = c("Colorado", "Missouri", "Nebraska", "Oklahoma"),
  "Kentucky" = c("Illinois", "Indiana", "Missouri", "Ohio", "Tennessee", "Virginia", "West Virginia"),
  "Louisiana" = c("Arkansas", "Mississippi", "Texas"),
  "Maine" = c("New Hampshire"),
  "Maryland" = c("Virginia", "West Virginia"),
  "Massachusetts" = c("New Hampshire"),
  "Michigan" = c("Indiana", "Ohio", "Wisconsin"),
  "Minnesota" = c("North Dakota", "South Dakota", "Wisconsin"),
  "Mississippi" = c("Alabama", "Arkansas", "Louisiana", "Tennessee"),
  "Missouri" = c("Arkansas", "Illinois", "Kansas", "Kentucky", "Nebraska", "Oklahoma", "Tennessee"),
  "Montana" = c("Idaho", "North Dakota", "South Dakota"),
  "Nebraska" = c("Colorado", "Kansas", "Missouri", "South Dakota"),
  "North Carolina" = c("Georgia", "South Carolina", "Tennessee", "Virginia"),
  "North Dakota" = c("Minnesota", "Montana", "South Dakota"),
  "Ohio" = c("Indiana", "Kentucky", "Michigan", "West Virginia"),
  "Oklahoma" = c("Arkansas", "Colorado", "Kansas", "Missouri", "Texas"),
  "Oregon" = c("Idaho", "Washington"),
  "South Carolina" = c("Georgia", "North Carolina"),
  "South Dakota" = c("Minnesota", "Montana", "Nebraska", "North Dakota"),
  "Tennessee" = c("Alabama", "Arkansas", "Georgia", "Kentucky", "Mississippi", "Missouri", "North Carolina", "Virginia"),
  "Texas" = c("Arkansas", "Louisiana", "Oklahoma"),
  "Utah" = c("Colorado", "Idaho"),
  "Virginia" = c("Kentucky", "Maryland", "North Carolina", "Tennessee", "West Virginia"),
  "Washington" = c("Idaho", "Oregon"),
  "West Virginia" = c("Kentucky", "Maryland", "Ohio", "Virginia"),
  "Wisconsin" = c("Illinois", "Michigan", "Minnesota")
)

# Function to check if two states are neighbors
are_neighbors <- function(state1, state2, adj_list) {
  if (!state1 %in% names(adj_list) | !state2 %in% names(adj_list)) {
    return(FALSE)
  }
  return(state2 %in% adj_list[[state1]] | state1 %in% adj_list[[state2]])
}

# ==============================================================================
# 2. ADD NEIGHBOR FLAG TO SIMILARITY MATRIX
# ==============================================================================

sim_matrix <- sim_matrix %>%
  rowwise() %>%
  mutate(
    neighbors = are_neighbors(state1, state2, adjacency_list)
  ) %>%
  ungroup()

# ==============================================================================
# 3. COMPARE NEIGHBOR VS NON-NEIGHBOR SIMILARITY
# ==============================================================================

cat("=== GEOGRAPHIC DIFFUSION TEST ===\n\n")

neighbor_comparison <- sim_matrix %>%
  group_by(neighbors) %>%
  summarize(
    n_pairs = n(),
    mean_similarity = mean(similarity),
    sd_similarity = sd(similarity),
    median_similarity = median(similarity),
    .groups = "drop"
  )

cat("Similarity by Neighbor Status:\n")
print(neighbor_comparison)

# T-test
t_result <- t.test(similarity ~ neighbors, data = sim_matrix)
cat("\n--- T-Test: Neighbors vs Non-Neighbors ---\n")
cat("Neighbor mean:", round(mean(sim_matrix$similarity[sim_matrix$neighbors]), 3), "\n")
cat("Non-neighbor mean:", round(mean(sim_matrix$similarity[!sim_matrix$neighbors]), 3), "\n")
cat("Difference:", round(t_result$estimate[2] - t_result$estimate[1], 3), "\n")
cat("t =", round(t_result$statistic, 3), "\n")
cat("p =", round(t_result$p.value, 4), "\n")

# Effect size (Cohen's d)
cohens_d <- (mean(sim_matrix$similarity[sim_matrix$neighbors]) - 
             mean(sim_matrix$similarity[!sim_matrix$neighbors])) /
            sd(sim_matrix$similarity)
cat("Cohen's d:", round(cohens_d, 3), "\n")

# ==============================================================================
# 4. VISUALIZATION
# ==============================================================================

# Box plot
p_neighbor <- ggplot(sim_matrix, aes(x = neighbors, y = similarity, fill = neighbors)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  scale_fill_manual(values = c("FALSE" = "#E74C3C", "TRUE" = "#27AE60"),
                    labels = c("Non-Neighbors", "Neighbors")) +
  scale_x_discrete(labels = c("Non-Neighbors", "Neighbors")) +
  labs(title = "Statutory Similarity: Neighbors vs. Non-Neighbors",
       subtitle = paste0("Neighbors are ", 
                        round((neighbor_comparison$mean_similarity[2] - neighbor_comparison$mean_similarity[1]) * 100, 1),
                        " percentage points more similar (p = ", round(t_result$p.value, 3), ")"),
       x = NULL,
       y = "Textual Similarity (Cosine)") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold"))

ggsave("../../3_Output/Figures/geographic_diffusion_test.png"), p_neighbor, width = 8, height = 6)
cat("\nSaved: geographic_diffusion_test.png\n")

# ==============================================================================
# 5. IDENTIFY HIGH-SIMILARITY NEIGHBOR PAIRS
# ==============================================================================

cat("\n--- High-Similarity Neighbor Pairs (>0.80) ---\n")
high_sim_neighbors <- sim_matrix %>%
  filter(neighbors & similarity > 0.80) %>%
  arrange(desc(similarity)) %>%
  select(state1, state2, similarity, year1, year2)

print(high_sim_neighbors)

# ==============================================================================
# 6. TEMPORAL ANALYSIS: Does neighbor-copying increase over time?
# ==============================================================================

cat("\n--- Neighbor Similarity by Adoption Wave ---\n")

# Add wave info
topics_waves <- topic_data %>% select(state, adoption_wave)

sim_with_waves <- sim_matrix %>%
  left_join(topics_waves, by = c("state1" = "state")) %>%
  rename(wave1 = adoption_wave) %>%
  left_join(topics_waves, by = c("state2" = "state")) %>%
  rename(wave2 = adoption_wave) %>%
  mutate(
    later_wave = pmax(
      as.numeric(factor(wave1, levels = c("Wave 1 (2004-2006)", "Wave 2 (2007-2010)", 
                                          "Wave 3 (2011-2014)", "Wave 4 (2015-2018)"))),
      as.numeric(factor(wave2, levels = c("Wave 1 (2004-2006)", "Wave 2 (2007-2010)", 
                                          "Wave 3 (2011-2014)", "Wave 4 (2015-2018)")))
    )
  )

neighbor_by_wave <- sim_with_waves %>%
  group_by(later_wave, neighbors) %>%
  summarize(
    mean_similarity = mean(similarity),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = neighbors, values_from = c(mean_similarity, n), names_sep = "_")

cat("\nMean similarity by wave and neighbor status:\n")
print(neighbor_by_wave)

# ==============================================================================
# 7. REGRESSION: Similarity ~ Neighbor + Controls
# ==============================================================================

cat("\n--- Regression: Similarity ~ Neighbor + Year Gap ---\n")

reg_data <- sim_matrix %>%
  mutate(
    year_gap = abs(year1 - year2),
    same_region = case_when(
      # Define rough regions
      state1 %in% c("Maine", "Massachusetts", "New Hampshire") & 
        state2 %in% c("Maine", "Massachusetts", "New Hampshire") ~ TRUE,
      state1 %in% c("North Carolina", "South Carolina", "Virginia", "Georgia", "Florida", 
                    "Alabama", "Mississippi", "Tennessee", "Kentucky", "West Virginia") &
        state2 %in% c("North Carolina", "South Carolina", "Virginia", "Georgia", "Florida",
                      "Alabama", "Mississippi", "Tennessee", "Kentucky", "West Virginia") ~ TRUE,
      state1 %in% c("Texas", "Oklahoma", "Arkansas", "Louisiana") &
        state2 %in% c("Texas", "Oklahoma", "Arkansas", "Louisiana") ~ TRUE,
      state1 %in% c("Montana", "Idaho", "Utah", "Colorado", "Oregon", "Washington") &
        state2 %in% c("Montana", "Idaho", "Utah", "Colorado", "Oregon", "Washington") ~ TRUE,
      state1 %in% c("North Dakota", "South Dakota", "Nebraska", "Kansas", "Minnesota", 
                    "Wisconsin", "Illinois", "Indiana", "Ohio", "Michigan", "Missouri") &
        state2 %in% c("North Dakota", "South Dakota", "Nebraska", "Kansas", "Minnesota",
                      "Wisconsin", "Illinois", "Indiana", "Ohio", "Michigan", "Missouri") ~ TRUE,
      TRUE ~ FALSE
    )
  )

model1 <- lm(similarity ~ neighbors, data = reg_data)
model2 <- lm(similarity ~ neighbors + year_gap, data = reg_data)
model3 <- lm(similarity ~ neighbors + year_gap + same_region, data = reg_data)

cat("\nModel 1: Similarity ~ Neighbors\n")
print(summary(model1))

cat("\nModel 2: Similarity ~ Neighbors + Year Gap\n")
print(summary(model2))

cat("\nModel 3: Similarity ~ Neighbors + Year Gap + Same Region\n")
print(summary(model3))

# ==============================================================================
# 8. SUMMARY
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("GEOGRAPHIC DIFFUSION: SUMMARY\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

cat("\nKEY FINDING:\n")
if (t_result$p.value < 0.05) {
  cat("  ✓ Neighboring states have SIGNIFICANTLY more similar statutes\n")
  cat("    Neighbor mean: ", round(neighbor_comparison$mean_similarity[2], 3), "\n")
  cat("    Non-neighbor mean: ", round(neighbor_comparison$mean_similarity[1], 3), "\n")
  cat("    Difference: ", round(neighbor_comparison$mean_similarity[2] - neighbor_comparison$mean_similarity[1], 3), "\n")
  cat("    p-value: ", round(t_result$p.value, 4), "\n")
} else {
  cat("  ✗ No significant difference between neighbors and non-neighbors\n")
  cat("    This suggests diffusion is NOT primarily geographic\n")
  cat("    States may be copying 'leader' states regardless of location\n")
}

cat("\nIMPLICATION FOR PAPER:\n")
cat("  Combined with null political economy results, this suggests:\n")
cat("  - States don't respond to internal conditions (ag sector, party control)\n")
if (t_result$p.value < 0.05) {
  cat("  - States DO copy neighboring states (geographic diffusion)\n")
} else {
  cat("  - Diffusion is driven by policy templates, not geography\n")
  cat("  - 'Leader' states (NC, VA, OK) set the template that others copy\n")
}
