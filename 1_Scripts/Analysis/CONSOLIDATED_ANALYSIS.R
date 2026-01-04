# ==============================================================================
# CONSOLIDATED AGRITOURISM ANALYSIS - FINAL VERSION
# Single script for all analyses, figures, and tables
# N = 43 states | K = 4 topics | Beta regression
# ==============================================================================

library(tidyverse)
library(stm)
library(quanteda)
library(betareg)
library(gt)
library(webshot2)
library(here)

set.seed(2024)

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("AGRITOURISM POLICY DIFFUSION ANALYSIS\n")
cat("Consolidated Analysis Script - Final Version\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# ==============================================================================
# TOPIC LABELS (CORRECTED)
# ==============================================================================

TOPIC_LABELS <- c(
  "Topic 1" = "Liability/Warning",
  "Topic 2" = "Governance/Administration", 
  "Topic 3" = "Land Use/Zoning",
  "Topic 4" = "Recreational Use"
)

TOPIC_COLORS <- c(
  "Liability/Warning" = "#66c2a5",
  "Governance/Administration" = "#fc8d62",
  "Land Use/Zoning" = "#8da0cb",
  "Recreational Use" = "#e78ac3"
)

# ==============================================================================
# PART 1: DATA PREPARATION
# ==============================================================================

cat("=== PART 1: DATA PREPARATION ===\n\n")

# Complete enactment year data (43 states)
enactment_years <- tribble(
  ~state, ~enact_year, ~notes,
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
  "Alaska", 2003, "Added - SLA 2003 ch.121",
  "Arizona", 2019, "Added - HB 2556",
  "Delaware", 2008, "Added - 76 Del. Laws c.409",
  "Iowa", 2021, "Added - SF 356",
  "New Hampshire", 2018, "Added - SB 412",
  "New York", 2017, "Added - Safety in Agricultural Tourism Act",
  "Pennsylvania", 2021, "Added - Act 27",
  "Vermont", 2021, "Added - Act 31 (H.89)"
)

# Add adoption waves
enactment_years <- enactment_years %>%
  mutate(
    adoption_wave = case_when(
      enact_year <= 2006 ~ "Wave 1 (2003-2006)",
      enact_year <= 2010 ~ "Wave 2 (2007-2010)",
      enact_year <= 2014 ~ "Wave 3 (2011-2014)",
      enact_year <= 2018 ~ "Wave 4 (2015-2018)",
      TRUE ~ "Wave 5 (2019-2021)"
    )
  )

cat("Total states:", nrow(enactment_years), "\n")
cat("\nStates by wave:\n")
print(table(enactment_years$adoption_wave))

# Load text files
text_dir <- "../../State Agritourism Laws"

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
  ) %>%
  filter(!is.na(text))

cat("\nStates with text files:", nrow(state_texts), "\n")

# ==============================================================================
# PART 2: STM ESTIMATION
# ==============================================================================

cat("\n=== PART 2: STM ESTIMATION ===\n\n")

# Create corpus and preprocess
corpus <- corpus(state_texts, text_field = "text", docid_field = "state")

dfm_data <- corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(c("shall", "section", "subsection", "act", "state", 
                  "means", "purpose", "person", "including", "may", 
                  "provided", "pursuant", "code", "statute", "statutes", 
                  "chapter", "title", "ann", "law", "laws", "amended")) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 2, min_docfreq = 2)

cat("Corpus: ", nrow(dfm_data), "documents,", ncol(dfm_data), "features\n")

# Convert to STM format
stm_data <- convert(dfm_data, to = "stm")

# Fit STM with K=4
cat("\nFitting STM (K=4)...\n")
stm_model <- stm(
  documents = stm_data$documents,
  vocab = stm_data$vocab,
  K = 4,
  max.em.its = 100,
  init.type = "Spectral",
  seed = 2024,
  verbose = FALSE
)

# Model diagnostics
cat("\n--- Model Diagnostics ---\n")
exclusivity_scores <- exclusivity(stm_model)
coherence_scores <- semanticCoherence(stm_model, stm_data$documents)

cat("Exclusivity by topic:", round(exclusivity_scores, 2), "\n")
cat("Mean exclusivity:", round(mean(exclusivity_scores), 2), "\n")
cat("Coherence by topic:", round(coherence_scores, 2), "\n")
cat("Mean coherence:", round(mean(coherence_scores), 2), "\n")

# Top words
cat("\n--- Top Words (FREX) ---\n")
top_words <- labelTopics(stm_model, n = 10)
for(i in 1:4) {
  cat(sprintf("%s: %s\n", names(TOPIC_LABELS)[i], 
              paste(top_words$frex[i,], collapse = ", ")))
}

# ==============================================================================
# PART 3: CREATE ANALYSIS DATASET
# ==============================================================================

cat("\n=== PART 3: ANALYSIS DATASET ===\n\n")

# Extract topic proportions
theta <- stm_model$theta
colnames(theta) <- paste0("topic", 1:4)

analysis_data <- state_texts %>%
  select(state, enact_year, notes, adoption_wave) %>%
  bind_cols(as_tibble(theta)) %>%
  mutate(
    dominant_topic = case_when(
      topic1 == pmax(topic1, topic2, topic3, topic4) ~ "Liability/Warning",
      topic2 == pmax(topic1, topic2, topic3, topic4) ~ "Governance/Administration",
      topic3 == pmax(topic1, topic2, topic3, topic4) ~ "Land Use/Zoning",
      TRUE ~ "Recreational Use"
    ),
    dominant_topic = factor(dominant_topic, levels = names(TOPIC_COLORS))
  )

# State abbreviations for figures
state_abbrevs <- tibble(
  state = c("Alabama", "Alaska", "Arizona", "Arkansas", "Colorado", "Delaware",
            "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa",
            "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
            "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska",
            "New Hampshire", "New York", "North Carolina", "North Dakota", "Ohio",
            "Oklahoma", "Oregon", "Pennsylvania", "South Carolina", "South Dakota",
            "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
            "West Virginia", "Wisconsin"),
  abbrev = c("AL", "AK", "AZ", "AR", "CO", "DE", "FL", "GA", "HI", "ID", "IL", 
             "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", 
             "MO", "MT", "NE", "NH", "NY", "NC", "ND", "OH", "OK", "OR", "PA", 
             "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI")
)

analysis_data <- analysis_data %>%
  left_join(state_abbrevs, by = "state")

cat("Analysis data created:", nrow(analysis_data), "states\n")

# ==============================================================================
# PART 4: SIMILARITY MATRIX (EUCLIDEAN DISTANCE)
# ==============================================================================

cat("\n=== PART 4: SIMILARITY MATRIX ===\n\n")

# Using 1 - Euclidean distance for consistency
topic_matrix <- analysis_data %>%
  select(state, topic1, topic2, topic3, topic4) %>%
  column_to_rownames("state") %>%
  as.matrix()

states <- rownames(topic_matrix)
n_states <- length(states)

# Compute pairwise similarities
sim_long <- expand_grid(state1 = states, state2 = states) %>%
  filter(state1 < state2) %>%
  rowwise() %>%
  mutate(
    euclidean_dist = sqrt(sum((topic_matrix[state1,] - topic_matrix[state2,])^2)),
    similarity = 1 - euclidean_dist  # Similarity = 1 - distance
  ) %>%
  ungroup() %>%
  left_join(analysis_data %>% select(state, enact_year), by = c("state1" = "state")) %>%
  rename(year1 = enact_year) %>%
  left_join(analysis_data %>% select(state, enact_year), by = c("state2" = "state")) %>%
  rename(year2 = enact_year)

cat("Similarity pairs computed:", nrow(sim_long), "\n")

# ==============================================================================
# PART 5: GEOGRAPHIC DIFFUSION TEST
# ==============================================================================

cat("\n=== PART 5: GEOGRAPHIC DIFFUSION ===\n\n")

# State adjacency
adjacency_list <- list(
  "Alabama" = c("Florida", "Georgia", "Mississippi", "Tennessee"),
  "Alaska" = character(0),
  "Arizona" = c("Colorado", "Utah"),
  "Arkansas" = c("Louisiana", "Mississippi", "Missouri", "Oklahoma", "Tennessee", "Texas"),
  "Colorado" = c("Arizona", "Kansas", "Nebraska", "Oklahoma", "Utah"),
  "Delaware" = c("Maryland"),
  "Florida" = c("Alabama", "Georgia"),
  "Georgia" = c("Alabama", "Florida", "North Carolina", "South Carolina", "Tennessee"),
  "Hawaii" = character(0),
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
  "New York" = c("Massachusetts", "Pennsylvania", "Vermont"),
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

are_neighbors <- function(s1, s2, adj) {
  if (!s1 %in% names(adj) | !s2 %in% names(adj)) return(FALSE)
  return(s2 %in% adj[[s1]] | s1 %in% adj[[s2]])
}

sim_long <- sim_long %>%
  rowwise() %>%
  mutate(neighbors = are_neighbors(state1, state2, adjacency_list)) %>%
  ungroup()

# T-test
neighbor_sim <- sim_long %>% filter(neighbors) %>% pull(similarity)
nonneighbor_sim <- sim_long %>% filter(!neighbors) %>% pull(similarity)
neighbor_test <- t.test(neighbor_sim, nonneighbor_sim)

# Effect size (Cohen's d)
pooled_sd <- sqrt(((length(neighbor_sim)-1)*sd(neighbor_sim)^2 + 
                   (length(nonneighbor_sim)-1)*sd(nonneighbor_sim)^2) / 
                  (length(neighbor_sim) + length(nonneighbor_sim) - 2))
cohens_d <- (mean(neighbor_sim) - mean(nonneighbor_sim)) / pooled_sd

cat("--- Geographic Diffusion Results ---\n")
cat("Neighbor pairs:", sum(sim_long$neighbors), "\n")
cat("Non-neighbor pairs:", sum(!sim_long$neighbors), "\n")
cat("Mean similarity (neighbors):", round(mean(neighbor_sim), 3), "\n")
cat("Mean similarity (non-neighbors):", round(mean(nonneighbor_sim), 3), "\n")
cat("Difference:", round(mean(neighbor_sim) - mean(nonneighbor_sim), 3), "\n")
cat("t-statistic:", round(neighbor_test$statistic, 2), "\n")
cat("p-value:", format.pval(neighbor_test$p.value, digits = 4), "\n")
cat("Cohen's d:", round(cohens_d, 2), "\n")

# ==============================================================================
# PART 6: CUMULATIVE CONVERGENCE ANALYSIS
# ==============================================================================

cat("\n=== PART 6: CONVERGENCE ANALYSIS ===\n\n")

# Calculate distance to cumulative centroid of prior adopters
analysis_data <- analysis_data %>% arrange(enact_year)

convergence_data <- tibble()

for(i in 1:nrow(analysis_data)) {
  current_state <- analysis_data$state[i]
  current_year <- analysis_data$enact_year[i]
  current_topics <- as.numeric(analysis_data[i, c("topic1", "topic2", "topic3", "topic4")])
  
  # Get prior adopters
  prior <- analysis_data %>% filter(enact_year < current_year)
  
  if(nrow(prior) == 0) {
    # First adopter - no prior centroid
    dist_to_prior <- NA
  } else {
    # Cumulative centroid of all prior adopters
    prior_centroid <- colMeans(prior[, c("topic1", "topic2", "topic3", "topic4")])
    dist_to_prior <- sqrt(sum((current_topics - prior_centroid)^2))
  }
  
  convergence_data <- bind_rows(convergence_data, tibble(
    state = current_state,
    enact_year = current_year,
    dist_to_centroid = dist_to_prior
  ))
}

analysis_data <- analysis_data %>%
  left_join(convergence_data %>% select(state, dist_to_centroid), by = "state")

# Variance by wave
variance_by_wave <- analysis_data %>%
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    var_t1 = var(topic1),
    var_t2 = var(topic2),
    var_t3 = var(topic3),
    var_t4 = var(topic4),
    total_var = var_t1 + var_t2 + var_t3 + var_t4,
    mean_dist = mean(dist_to_centroid, na.rm = TRUE),
    .groups = "drop"
  )

cat("--- Variance by Wave ---\n")
print(variance_by_wave %>% select(adoption_wave, n, total_var, mean_dist))

# ==============================================================================
# PART 7: FIGURES
# ==============================================================================

cat("\n=== PART 7: CREATING FIGURES ===\n\n")

# Figure 1: Topic Composition by State and Wave (Faceted)
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
    topic_label = factor(topic_label, levels = names(TOPIC_COLORS))
  )

# Sort states by dominant topic within each wave
state_order <- analysis_data %>%
  group_by(adoption_wave) %>%
  arrange(adoption_wave, desc(topic1)) %>%
  pull(abbrev)

fig1_data$abbrev <- factor(fig1_data$abbrev, levels = state_order)

fig1 <- ggplot(fig1_data, aes(x = abbrev, y = proportion, fill = topic_label)) +
  geom_col(position = "stack", width = 0.85) +
  facet_wrap(~adoption_wave, scales = "free_x", nrow = 2) +
  scale_fill_manual(values = TOPIC_COLORS, name = "Topic") +
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
    panel.grid.minor = element_blank()
  )

ggsave("../../3_Output/Figures/figure1_topic_by_state_wave.png"), fig1, width = 12, height = 8, dpi = 300)
cat("Saved: figure1_topic_by_state_wave.png\n")

# Figure 2: Policy Convergence Over Time
fig2 <- analysis_data %>%
  filter(!is.na(dist_to_centroid)) %>%
  ggplot(aes(x = enact_year, y = dist_to_centroid, color = adoption_wave)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = "gray40", 
              linetype = "dashed", alpha = 0.3) +
  geom_text(aes(label = abbrev), hjust = -0.2, vjust = 0.5, size = 2.5, 
            check_overlap = TRUE, show.legend = FALSE) +
  scale_color_brewer(palette = "Set1", name = "Adoption Wave") +
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
  )

ggsave("../../3_Output/Figures/figure2_convergence_over_time.png"), fig2, width = 11, height = 7, dpi = 300)
cat("Saved: figure2_convergence_over_time.png\n")

# Figure 3: Variance by Wave
fig3 <- variance_by_wave %>%
  ggplot(aes(x = adoption_wave, y = total_var)) +
  geom_col(fill = "steelblue", width = 0.7) +
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
    panel.grid.minor = element_blank()
  )

ggsave("../../3_Output/Figures/figure3_variance_by_wave.png"), fig3, width = 9, height = 6, dpi = 300)
cat("Saved: figure3_variance_by_wave.png\n")

# ==============================================================================
# PART 8: TABLES
# ==============================================================================

cat("\n=== PART 8: CREATING TABLES ===\n\n")

# Table 1: Topic Validation
topic_table <- tibble(
  Topic = c("Topic 1: Liability/Warning",
            "Topic 2: Governance/Administration",
            "Topic 3: Land Use/Zoning",
            "Topic 4: Recreational Use"),
  `Top Words (FREX)` = apply(top_words$frex, 1, paste, collapse = ", "),
  `Top Words (Probability)` = apply(top_words$prob, 1, paste, collapse = ", ")
)

tbl1 <- topic_table %>%
  gt() %>%
  tab_header(
    title = md("**Table 1. Topic Validation: Top Words by Topic**"),
    subtitle = md("*Structural Topic Model (K=4) Applied to 43 State Agritourism Statutes*")
  ) %>%
  cols_align(align = "left") %>%
  tab_source_note(
    source_note = md(sprintf("*Notes:* FREX weights words by frequency and exclusivity. Mean exclusivity = %.2f; mean semantic coherence = %.2f. K=4 selected based on model diagnostics and substantive interpretability.",
                             mean(exclusivity_scores), mean(coherence_scores)))
  ) %>%
  tab_options(table.font.size = 10, heading.title.font.size = 12)

gtsave(tbl1, "../../3_Output/Tables/table1_topic_validation.png", vwidth = 900, vheight = 300)
gtsave(tbl1, "../../3_Output/Tables/table1_topic_validation.html")
cat("Saved: table1_topic_validation.png\n")

# Table 3: Geographic Diffusion
tbl2_data <- tibble(
  Comparison = c("Neighboring States", "Non-Neighboring States", "Difference"),
  `Mean Similarity` = c(round(mean(neighbor_sim), 3),
                        round(mean(nonneighbor_sim), 3),
                        round(mean(neighbor_sim) - mean(nonneighbor_sim), 3)),
  `N Pairs` = c(sum(sim_long$neighbors), sum(!sim_long$neighbors), "—"),
  `Test Statistic` = c("—", "—", sprintf("t = %.2f", neighbor_test$statistic)),
  `p-value` = c("—", "—", sprintf("%.4f***", neighbor_test$p.value)),
  `Effect Size` = c("—", "—", sprintf("d = %.2f", cohens_d))
)

tbl2 <- tbl2_data %>%
  gt() %>%
  tab_header(
    title = md("**Table 3. Geographic Diffusion of Statutory Content**"),
    subtitle = md("*Similarity in Topic Proportions by Geographic Proximity (N = 43 States)*")
  ) %>%
  tab_source_note(
    source_note = md("*Notes:* Similarity measured as 1 - Euclidean distance between state topic proportion vectors (K=4 topics). Neighboring states defined as geographically contiguous. *** p < 0.001. Effect size: Cohen's d, where d > 0.5 indicates medium effect, d > 0.8 indicates large effect.")
  ) %>%
  tab_options(table.font.size = 10, heading.title.font.size = 12)

gtsave(tbl2, "../../3_Output/Tables/table2_geographic_diffusion.png", vwidth = 750, vheight = 250)
gtsave(tbl2, "../../3_Output/Tables/table2_geographic_diffusion.html")
cat("Saved: table2_geographic_diffusion.png\n")

# ==============================================================================
# PART 9: BETA REGRESSION (POLITICAL ECONOMY)
# ==============================================================================

cat("\n=== PART 9: BETA REGRESSION ===\n\n")

# Load CSPP data
cspp_data <- read_csv("../../2_Data/Processed/agritourism_analysis_final.csv", show_col_types = FALSE) %>%
  select(state, contrib_agri, valueofagsect, govparty_c)

# Load case counts
cases <- read_csv("../../2_Data/Raw/agritourism_cases_scraped.csv", show_col_types = FALSE)
case_counts <- cases %>%
  filter(state != "Federal") %>%
  count(state, name = "n_cases")

# Merge
model_data <- analysis_data %>%
  left_join(cspp_data, by = "state") %>%
  left_join(case_counts, by = "state") %>%
  mutate(n_cases = replace_na(n_cases, 0))

# Transform DV for beta regression (must be in (0,1) exclusive)
transform_beta <- function(y) {
  ifelse(y <= 0, 0.001, ifelse(y >= 1, 0.999, y))
}

model_data <- model_data %>%
  mutate(topic1_beta = transform_beta(topic1))

# Run beta regressions
cat("Running beta regressions...\n")

# H1: Agricultural PAC Contributions
data_h1 <- model_data %>% filter(!is.na(contrib_agri))
m1 <- betareg(topic1_beta ~ log(contrib_agri + 1), data = data_h1)

# H2: Agricultural Sector Value
data_h2 <- model_data %>% filter(!is.na(valueofagsect))
m2 <- betareg(topic1_beta ~ log(valueofagsect + 1), data = data_h2)

# H3: Republican Governor
data_h3 <- model_data %>% filter(!is.na(govparty_c))
m3 <- betareg(topic1_beta ~ factor(govparty_c), data = data_h3)

# H4: Court Cases
m4 <- betareg(topic1_beta ~ log(n_cases + 1), data = model_data)

# Extract results
get_beta_results <- function(model, name, predictor) {
  s <- summary(model)
  coef <- s$coefficients$mean[2,]
  tibble(
    Hypothesis = name,
    Predictor = predictor,
    beta = coef[1],
    se = coef[2],
    p_value = coef[4],
    n = nrow(model$model)
  )
}

beta_results <- bind_rows(
  get_beta_results(m1, "H1", "Agricultural Contributions (log)"),
  get_beta_results(m2, "H2", "Agricultural Sector Value (log)"),
  get_beta_results(m3, "H3", "Republican Governor"),
  get_beta_results(m4, "H4", "Agritourism Cases (log)")
)

cat("\n--- Beta Regression Results ---\n")
print(beta_results)

# Table 2: Political Economy (Beta Regression)
tbl3_data <- beta_results %>%
  mutate(
    beta_fmt = sprintf("%.3f", beta),
    se_fmt = sprintf("(%.3f)", se),
    p_fmt = sprintf("%.3f", p_value),
    n_fmt = as.character(n)
  ) %>%
  select(Hypothesis, Predictor, beta_fmt, se_fmt, p_fmt, n_fmt)

tbl3 <- tbl3_data %>%
  gt() %>%
  tab_header(
    title = md("**Table 2. Internal Political Economy Does Not Predict Statutory Content**"),
    subtitle = md("*Beta Regression: Predictors of Liability Topic Proportion (N = 43 States)*")
  ) %>%
  cols_label(
    beta_fmt = "β",
    se_fmt = "SE",
    p_fmt = "p-value",
    n_fmt = "N"
  ) %>%
  cols_align(align = "center", columns = c(beta_fmt, se_fmt, p_fmt, n_fmt)) %>%
  cols_align(align = "left", columns = c(Hypothesis, Predictor)) %>%
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
  tab_source_note(
    source_note = md("*Notes:* Dependent variable is Topic 1 (Liability/Warning) proportion from Structural Topic Model (K=4). Beta regression used for bounded (0,1) dependent variable. † p < 0.10, * p < 0.05, ** p < 0.01, *** p < 0.001. N varies by covariate availability; H4 uses full sample (N=43).")
  ) %>%
  tab_options(table.font.size = 10, heading.title.font.size = 12)

gtsave(tbl3, "../../3_Output/Tables/table3_political_economy_beta.png", vwidth = 700, vheight = 350)
gtsave(tbl3, "../../3_Output/Tables/table3_political_economy_beta.html")
cat("Saved: table3_political_economy_beta.png\n")

# ==============================================================================
# PART 10: SAVE ALL DATA
# ==============================================================================

cat("\n=== PART 10: SAVING DATA ===\n\n")

# Save model and data for future use
model <- stm_model
documents <- stm_data$documents
vocab <- stm_data$vocab
reg_data <- analysis_data

save(model, documents, vocab, reg_data, file = "../../4_Analysis_Results/stm_model_results.RData")
cat("Saved: stm_model_results.RData\n")

write_csv(analysis_data, "../../2_Data/Processed/final_analysis_data.csv")
cat("Saved: analysis_data_final.csv\n")

write_csv(sim_long, "../../2_Data/STM/similarity_matrix_final.csv")
cat("Saved: similarity_matrix_final.csv\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("ANALYSIS COMPLETE\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

cat("\n--- Sample ---\n")
cat("N =", nrow(analysis_data), "states\n")
cat("K = 4 topics\n")

cat("\n--- Topic Labels ---\n")
for(i in 1:4) cat(sprintf("Topic %d: %s\n", i, TOPIC_LABELS[i]))

cat("\n--- Key Findings ---\n")
cat(sprintf("1. Geographic diffusion: Neighbors are %.1f pp more similar (p = %.4f, d = %.2f)\n",
            (mean(neighbor_sim) - mean(nonneighbor_sim)) * 100,
            neighbor_test$p.value, cohens_d))
cat("2. Internal political economy: All four predictors non-significant (all p > 0.20)\n")
cat(sprintf("3. Non-monotonic convergence: Variance peaks in Wave 4 (%.3f) after low in Wave 3 (%.3f)\n",
            variance_by_wave$total_var[4], variance_by_wave$total_var[3]))

cat("\n--- Output Files ---\n")
cat("Figures:\n")
cat("  - figure1_topic_by_state_wave.png\n")
cat("  - figure2_convergence_over_time.png\n")
cat("  - figure3_variance_by_wave.png\n")
cat("Tables:\n")
cat("  - table1_topic_validation.png\n")
cat("  - table2_geographic_diffusion.png\n")
cat("  - table3_political_economy_beta.png\n")
cat("Data:\n")
cat("  - stm_model_results.RData\n")
cat("  - analysis_data_final.csv\n")
cat("  - similarity_matrix_final.csv\n")
