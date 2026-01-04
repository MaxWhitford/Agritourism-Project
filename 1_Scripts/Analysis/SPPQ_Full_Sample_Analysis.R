# ==============================================================================
# Agritourism Policy Diffusion Analysis - FULL SAMPLE (N=43)
# Updated to include all states with agritourism statutes
# Excludes Connecticut (no enacted liability statute, only marketing language)
# ==============================================================================

library(tidyverse)
library(stm)
library(quanteda)
library(ggplot2)
library(here)

set.seed(2024)

# ==============================================================================
# COMPLETE ENACTMENT YEAR DATA (43 states)
# ==============================================================================

enactment_years <- tribble(
  ~state, ~enact_year, ~notes,
  # Original 35 states from CSPP analysis
  "Alabama", 2012, "Original sample",
  "Arkansas", 2011, "Original sample",
  "Colorado", 2008, "Original sample",
  "Florida", 2013, "Original sample",
  "Georgia", 2008, "Original sample",
  "Hawaii", 2006, "Original sample",
  "Idaho", 2014, "Original sample",
  "Illinois", 2006, "Original sample",
  "Indiana", 2011, "Original sample",
  "Kansas", 2004, "Original sample - early adopter",
  "Kentucky", 2012, "Original sample",
  "Louisiana", 2008, "Original sample",
  "Maine", 2012, "Original sample",
  "Maryland", 2018, "Original sample",
  "Massachusetts", 2006, "Original sample",
  "Michigan", 2006, "Original sample",
  "Minnesota", 2015, "Original sample",
  "Mississippi", 2012, "Original sample",
  "Missouri", 2010, "Original sample",
  "Montana", 2009, "Original sample",
  "Nebraska", 2015, "Original sample",
  "North Carolina", 2005, "Original sample - early adopter/template",
  "North Dakota", 2011, "Original sample",
  "Ohio", 2016, "Original sample",
  "Oklahoma", 2004, "Original sample - early adopter",
  "Oregon", 2015, "Original sample",
  "South Carolina", 2010, "Original sample",
  "South Dakota", 2010, "Original sample",
  "Tennessee", 2009, "Original sample",
  "Texas", 2015, "Original sample",
  "Utah", 2008, "Original sample",
  "Virginia", 2006, "Original sample - early adopter/template",
  "Washington", 2017, "Original sample",
  "West Virginia", 2018, "Original sample",
  "Wisconsin", 2014, "Original sample",
  # New states added (8 additional)
  "Alaska", 2003, "Added - SLA 2003 ch.121 (farm touring in sports/rec statute)",
  "Arizona", 2019, "Added - HB 2556 signed June 7, 2019",
  "Delaware", 2008, "Added - 76 Del. Laws c.409 (zoning-based)",
  "Iowa", 2021, "Added - SF 356 signed May 19, 2021",
  "New Hampshire", 2018, "Added - SB 412 signed May 16, 2018",
  "New York", 2017, "Added - Safety in Agricultural Tourism Act",
  "Pennsylvania", 2021, "Added - Act 27 signed June 30, 2021",
  "Vermont", 2021, "Added - Act 31 (H.89) signed May 17, 2021"
)

# Note: Connecticut EXCLUDED - no enacted agritourism liability statute
# (only has marketing/promotion language in § 22-38a)

cat("=== FULL SAMPLE: N =", nrow(enactment_years), "states ===\n\n")

# Summary by year
cat("Adoption Timeline:\n")
enactment_years %>%
  arrange(enact_year) %>%
  group_by(enact_year) %>%
  summarize(n = n(), states = paste(state, collapse = ", ")) %>%
  print(n = 30)

# ==============================================================================
# LOAD AND PROCESS STATUTORY TEXTS
# ==============================================================================

cat("\n=== LOADING STATUTORY TEXTS ===\n")

text_dir <- "../../State Agritourism Laws"

# Read all state texts (excluding Connecticut)
state_texts <- enactment_years %>%
  mutate(
    filepath = file.path(text_dir, paste0(state, ".txt")),
    text = map_chr(filepath, ~ {
      if (file.exists(.x)) {
        readLines(.x, warn = FALSE) %>% paste(collapse = " ")
      } else {
        NA_character_
      }
    })
  )

# Check for missing files
missing <- state_texts %>% filter(is.na(text))
if (nrow(missing) > 0) {
  cat("\nWARNING: Missing text files for:\n")
  print(missing$state)
}

# Filter to states with text
state_texts <- state_texts %>% filter(!is.na(text))
cat("\nStates with text files:", nrow(state_texts), "\n")

# ==============================================================================
# STM ANALYSIS
# ==============================================================================

cat("\n=== RUNNING STM ===\n")

# Create corpus
corpus <- corpus(state_texts, text_field = "text", docid_field = "state")

# Preprocess
dfm_data <- corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(c("shall", "section", "subsection", "act", "state", 
                  "means", "purpose", "person", "activity", "activities",
                  "including", "may", "provided", "pursuant", "code",
                  "statute", "statutes", "chapter", "title", "ann")) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 2, min_docfreq = 2)

# Convert to STM format
stm_data <- convert(dfm_data, to = "stm")

# Run STM with K=4 topics
stm_model <- stm(
  documents = stm_data$documents,
  vocab = stm_data$vocab,
  K = 4,
  max.em.its = 100,
  init.type = "Spectral",
  seed = 2024,
  verbose = TRUE
)

# Label topics
cat("\n=== TOP WORDS PER TOPIC ===\n")
labelTopics(stm_model, n = 10)

# Extract topic proportions - FIXED VERSION
theta <- stm_model$theta
colnames(theta) <- paste0("topic", 1:ncol(theta))
topic_props <- as_tibble(theta) %>%
  mutate(state = docnames(dfm_data))

# Merge with enactment data
analysis_data <- state_texts %>%
  select(state, enact_year, notes) %>%
  left_join(topic_props, by = "state") %>%
  mutate(
    adoption_wave = case_when(
      enact_year <= 2006 ~ "Wave 1 (2003-2006)",
      enact_year <= 2010 ~ "Wave 2 (2007-2010)",
      enact_year <= 2014 ~ "Wave 3 (2011-2014)",
      enact_year <= 2018 ~ "Wave 4 (2015-2018)",
      TRUE ~ "Wave 5 (2019-2021)"
    ),
    dominant_topic = case_when(
      topic1 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 1",
      topic2 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 2",
      topic3 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 3",
      TRUE ~ "Topic 4"
    )
  )

cat("\n=== TOPIC DISTRIBUTION BY WAVE ===\n")
analysis_data %>%
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    mean_t1 = mean(topic1),
    mean_t2 = mean(topic2),
    mean_t3 = mean(topic3),
    mean_t4 = mean(topic4),
    .groups = "drop"
  ) %>%
  print()

# ==============================================================================
# SIMILARITY MATRIX
# ==============================================================================

cat("\n=== COMPUTING SIMILARITY MATRIX ===\n")

# Compute cosine similarity
topic_matrix <- analysis_data %>%
  select(state, topic1, topic2, topic3, topic4) %>%
  column_to_rownames("state") %>%
  as.matrix()

cosine_sim <- function(a, b) {
  sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

states <- rownames(topic_matrix)
n_states <- length(states)
sim_matrix <- matrix(NA, n_states, n_states, dimnames = list(states, states))

for (i in 1:n_states) {
  for (j in 1:n_states) {
    sim_matrix[i, j] <- cosine_sim(topic_matrix[i, ], topic_matrix[j, ])
  }
}

# Convert to long format
sim_long <- expand_grid(state1 = states, state2 = states) %>%
  filter(state1 < state2) %>%  # Upper triangle only
  mutate(
    similarity = map2_dbl(state1, state2, ~ sim_matrix[.x, .y])
  ) %>%
  left_join(analysis_data %>% select(state, enact_year), by = c("state1" = "state")) %>%
  rename(year1 = enact_year) %>%
  left_join(analysis_data %>% select(state, enact_year), by = c("state2" = "state")) %>%
  rename(year2 = enact_year)

# ==============================================================================
# GEOGRAPHIC DIFFUSION TEST
# ==============================================================================

cat("\n=== GEOGRAPHIC DIFFUSION TEST ===\n")

# State adjacency list
adjacency_list <- list(
  "Alabama" = c("Florida", "Georgia", "Mississippi", "Tennessee"),
  "Alaska" = character(0),  # No contiguous neighbors
  "Arizona" = c("Colorado", "Utah"),
  "Arkansas" = c("Louisiana", "Mississippi", "Missouri", "Oklahoma", "Tennessee", "Texas"),
  "Colorado" = c("Arizona", "Kansas", "Nebraska", "Oklahoma", "Utah"),
  "Delaware" = c("Maryland"),
  "Florida" = c("Alabama", "Georgia"),
  "Georgia" = c("Alabama", "Florida", "North Carolina", "South Carolina", "Tennessee"),
  "Hawaii" = character(0),  # No neighbors
  "Idaho" = c("Montana", "Oregon", "Utah", "Washington"),
  "Illinois" = c("Indiana", "Kentucky", "Missouri", "Wisconsin"),
  "Indiana" = c("Illinois", "Kentucky", "Michigan", "Ohio"),
  "Iowa" = c("Minnesota", "Missouri", "Nebraska", "South Dakota", "Wisconsin"),
  "Kansas" = c("Colorado", "Missouri", "Nebraska", "Oklahoma"),
  "Kentucky" = c("Illinois", "Indiana", "Missouri", "Ohio", "Tennessee", "Virginia", "West Virginia"),
  "Louisiana" = c("Arkansas", "Mississippi", "Texas"),
  "Maine" = c("New Hampshire"),
  "Maryland" = c("Delaware", "Pennsylvania", "Virginia", "West Virginia"),
  "Massachusetts" = c("New Hampshire", "New York", "Vermont"),
  "Michigan" = c("Indiana", "Ohio", "Wisconsin"),
  "Minnesota" = c("Iowa", "North Dakota", "South Dakota", "Wisconsin"),
  "Mississippi" = c("Alabama", "Arkansas", "Louisiana", "Tennessee"),
  "Missouri" = c("Arkansas", "Illinois", "Iowa", "Kansas", "Kentucky", "Nebraska", "Oklahoma", "Tennessee"),
  "Montana" = c("Idaho", "North Dakota", "South Dakota"),
  "Nebraska" = c("Colorado", "Iowa", "Kansas", "Missouri", "South Dakota"),
  "New Hampshire" = c("Maine", "Massachusetts", "Vermont"),
  "New York" = c("Massachusetts", "New Jersey", "Pennsylvania", "Vermont"),
  "North Carolina" = c("Georgia", "South Carolina", "Tennessee", "Virginia"),
  "North Dakota" = c("Minnesota", "Montana", "South Dakota"),
  "Ohio" = c("Indiana", "Kentucky", "Michigan", "Pennsylvania", "West Virginia"),
  "Oklahoma" = c("Arkansas", "Colorado", "Kansas", "Missouri", "Texas"),
  "Oregon" = c("Idaho", "Washington"),
  "Pennsylvania" = c("Delaware", "Maryland", "New York", "Ohio", "West Virginia"),
  "South Carolina" = c("Georgia", "North Carolina"),
  "South Dakota" = c("Iowa", "Minnesota", "Montana", "Nebraska", "North Dakota"),
  "Tennessee" = c("Alabama", "Arkansas", "Georgia", "Kentucky", "Mississippi", "Missouri", "North Carolina", "Virginia"),
  "Texas" = c("Arkansas", "Louisiana", "Oklahoma"),
  "Utah" = c("Arizona", "Colorado", "Idaho"),
  "Vermont" = c("Massachusetts", "New Hampshire", "New York"),
  "Virginia" = c("Kentucky", "Maryland", "North Carolina", "Tennessee", "West Virginia"),
  "Washington" = c("Idaho", "Oregon"),
  "West Virginia" = c("Kentucky", "Maryland", "Ohio", "Pennsylvania", "Virginia"),
  "Wisconsin" = c("Illinois", "Iowa", "Michigan", "Minnesota")
)

# Function to check neighbors
are_neighbors <- function(s1, s2, adj) {
  if (!s1 %in% names(adj) | !s2 %in% names(adj)) return(FALSE)
  return(s2 %in% adj[[s1]] | s1 %in% adj[[s2]])
}

# Add neighbor flag
sim_long <- sim_long %>%
  rowwise() %>%
  mutate(neighbors = are_neighbors(state1, state2, adjacency_list)) %>%
  ungroup()

# T-test
neighbor_test <- t.test(similarity ~ neighbors, data = sim_long)

cat("\n--- Geographic Diffusion Results ---\n")
cat("Neighbor mean similarity:", round(mean(sim_long$similarity[sim_long$neighbors]), 3), "\n")
cat("Non-neighbor mean similarity:", round(mean(sim_long$similarity[!sim_long$neighbors]), 3), "\n")
cat("Difference:", round(mean(sim_long$similarity[sim_long$neighbors]) - 
                          mean(sim_long$similarity[!sim_long$neighbors]), 3), "\n")
cat("t =", round(neighbor_test$statistic, 3), "\n")
cat("p =", round(neighbor_test$p.value, 4), "\n")

# Regression with year gap control
sim_long <- sim_long %>%
  mutate(year_gap = abs(year1 - year2))

model <- lm(similarity ~ neighbors + year_gap, data = sim_long)
cat("\n--- Regression: Similarity ~ Neighbors + Year Gap ---\n")
print(summary(model))

# ==============================================================================
# EQUILIBRATION METRICS
# ==============================================================================

cat("\n=== TOPIC EQUILIBRATION METRICS ===\n")

# 1. Variance by wave
variance_by_wave <- analysis_data %>%
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    var_t1 = var(topic1),
    var_t2 = var(topic2),
    var_t3 = var(topic3),
    var_t4 = var(topic4),
    total_var = var_t1 + var_t2 + var_t3 + var_t4,
    .groups = "drop"
  )

cat("\n--- Variance by Wave ---\n")
print(variance_by_wave)

# 2. Centroid distance
centroid <- analysis_data %>%
  summarize(
    c1 = mean(topic1), c2 = mean(topic2), 
    c3 = mean(topic3), c4 = mean(topic4)
  )

analysis_data <- analysis_data %>%
  mutate(
    dist_to_centroid = sqrt(
      (topic1 - centroid$c1)^2 + (topic2 - centroid$c2)^2 +
      (topic3 - centroid$c3)^2 + (topic4 - centroid$c4)^2
    )
  )

dist_by_wave <- analysis_data %>%
  group_by(adoption_wave) %>%
  summarize(
    mean_dist = mean(dist_to_centroid),
    sd_dist = sd(dist_to_centroid),
    .groups = "drop"
  )

cat("\n--- Distance to Centroid by Wave ---\n")
print(dist_by_wave)

# 3. Within-wave similarity
sim_with_waves <- sim_long %>%
  left_join(analysis_data %>% select(state, adoption_wave), by = c("state1" = "state")) %>%
  rename(wave1 = adoption_wave) %>%
  left_join(analysis_data %>% select(state, adoption_wave), by = c("state2" = "state")) %>%
  rename(wave2 = adoption_wave) %>%
  mutate(same_wave = wave1 == wave2)

within_wave_sim <- sim_with_waves %>%
  filter(same_wave) %>%
  group_by(wave1) %>%
  summarize(
    n_pairs = n(),
    mean_similarity = mean(similarity),
    .groups = "drop"
  )

cat("\n--- Within-Wave Similarity ---\n")
print(within_wave_sim)

# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

cat("\n=== CREATING VISUALIZATIONS ===\n")

# 1. Topic proportions by wave
topic_by_wave <- analysis_data %>%
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    across(starts_with("topic"), mean),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("topic"), names_to = "topic", values_to = "proportion")

p1 <- ggplot(topic_by_wave, aes(x = adoption_wave, y = proportion, fill = topic)) +
  geom_col(position = "stack") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Topic Distribution by Adoption Wave (N=43)",
       subtitle = "Full sample including late adopters",
       x = "Adoption Wave", y = "Mean Topic Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../../3_Output/Figures/figure_topic_by_wave_full.png"), p1, width = 10, height = 6)

# 2. Geographic diffusion boxplot
p2 <- ggplot(sim_long, aes(x = neighbors, y = similarity, fill = neighbors)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1) +
  scale_fill_manual(values = c("FALSE" = "#E74C3C", "TRUE" = "#27AE60"),
                    labels = c("Non-Neighbors", "Neighbors")) +
  scale_x_discrete(labels = c("Non-Neighbors", "Neighbors")) +
  labs(title = "Statutory Similarity: Neighbors vs. Non-Neighbors (N=43)",
       subtitle = paste0("Neighbors are ", 
                        round((mean(sim_long$similarity[sim_long$neighbors]) - 
                               mean(sim_long$similarity[!sim_long$neighbors])) * 100, 1),
                        " percentage points more similar (p = ", 
                        round(neighbor_test$p.value, 4), ")"),
       x = NULL, y = "Textual Similarity (Cosine)") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("../../3_Output/Figures/figure_geographic_diffusion_full.png"), p2, width = 8, height = 6)

# 3. Variance over time
var_plot <- variance_by_wave %>%
  ggplot(aes(x = adoption_wave, y = total_var)) +
  geom_col(fill = "steelblue") +
  labs(title = "Topic Variance by Adoption Wave",
       subtitle = "Lower variance = greater convergence",
       x = "Adoption Wave", y = "Total Variance (sum across topics)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../../3_Output/Figures/figure_variance_by_wave_full.png"), var_plot, width = 10, height = 6)

# 4. Adoption timeline
timeline <- enactment_years %>%
  count(enact_year) %>%
  ggplot(aes(x = enact_year, y = n)) +
  geom_col(fill = "darkgreen") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_x_continuous(breaks = seq(2003, 2021, 2)) +
  labs(title = "Agritourism Statute Adoptions Over Time (N=43)",
       x = "Year", y = "Number of States") +
  theme_minimal()

ggsave("../../3_Output/Figures/figure_adoption_timeline.png"), timeline, width = 10, height = 6)

# ==============================================================================
# SAVE DATA
# ==============================================================================

cat("\n=== SAVING DATA ===\n")

write_csv(analysis_data, "../../2_Data/Processed/analysis_data_full_sample.csv")
write_csv(sim_long, "../../2_Data/STM/similarity_matrix_full_sample.csv")
write_csv(enactment_years, "../../2_Data/Raw/enactment_years_all_states.csv")

save(stm_model, analysis_data, sim_long, file = "../../4_Analysis_Results/full_analysis_results.RData")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("ANALYSIS COMPLETE - FULL SAMPLE (N=43)\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

cat("\nSample Composition:\n")
cat("  - Original CSPP sample: 35 states\n")
cat("  - Added states: 8 (AK, AZ, DE, IA, NH, NY, PA, VT)\n")
cat("  - Excluded: Connecticut (no enacted liability statute)\n")
cat("  - Total: 43 states\n")

cat("\nKey Findings:\n")
cat("  - Geographic diffusion: Neighbors are", 
    round((mean(sim_long$similarity[sim_long$neighbors]) - 
           mean(sim_long$similarity[!sim_long$neighbors])) * 100, 1),
    "pp more similar (p =", round(neighbor_test$p.value, 4), ")\n")

cat("\nAdoption Waves:\n")
enactment_years %>%
  mutate(wave = case_when(
    enact_year <= 2006 ~ "Wave 1 (2003-2006)",
    enact_year <= 2010 ~ "Wave 2 (2007-2010)",
    enact_year <= 2014 ~ "Wave 3 (2011-2014)",
    enact_year <= 2018 ~ "Wave 4 (2015-2018)",
    TRUE ~ "Wave 5 (2019-2021)"
  )) %>%
  count(wave) %>%
  print()

cat("\nOutput Files:\n")
cat("  - analysis_data_full_sample.csv\n")
cat("  - similarity_matrix_full_sample.csv\n")
cat("  - enactment_years_all_states.csv\n")
cat("  - full_analysis_results.RData\n")
cat("  - figure_topic_by_wave_full.png\n")
cat("  - figure_geographic_diffusion_full.png\n")
cat("  - figure_variance_by_wave_full.png\n")
cat("  - figure_adoption_timeline.png\n")
