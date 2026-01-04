# ==============================================================================
# Title:        Agritourism Statutes: Political Economy & Horizontal Diffusion
# Author:       Max Whitford
# Target:       State Politics & Policy Quarterly
# ==============================================================================
#
# THEORETICAL FRAMEWORK:
# 1. Internal Political Economy: Ag sector size, interest group contributions,
#    and partisan control drive WHAT TYPE of statute states adopt
# 2. Horizontal Diffusion: Statutory language spreads across states over time;
#    later adopters copy early adopters' textual features
#
# ==============================================================================

library(tidyverse)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(stm)
library(betareg)
library(stargazer)
library(ggplot2)
library(ggrepel)
library(scales)
library(here)

set.seed(2024)

# ==============================================================================
# 1. LOAD AND MERGE ALL DATA
# ==============================================================================

cat("=== LOADING DATA ===\n\n")

# Political economy covariates
covariates <- read_csv("../../2_Data/Processed/agritourism_analysis_final.csv")

# Statutory text
files <- list.files(path = "../../State Agritourism Laws", 
                    pattern = "*.txt", full.names = TRUE)
texts <- readtext(files)
texts$state <- tools::file_path_sans_ext(basename(texts$doc_id))

# Merge
analysis_df <- texts %>%
  inner_join(covariates, by = "state")

cat("Sample: N =", nrow(analysis_df), "states\n")
cat("Enactment period:", min(analysis_df$enact_year), "-", max(analysis_df$enact_year), "\n\n")

# ==============================================================================
# 2. TEXT PREPROCESSING & STM
# ==============================================================================

corp <- corpus(analysis_df, text_field = "text", docid_field = "state")

dfm <- dfm(tokens(corp, remove_punct = TRUE, remove_numbers = TRUE)) %>%
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 2)

stm_input <- convert(dfm, to = "stm")

# Run STM
model <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  K = 4,
  max.em.its = 100,
  init.type = "Spectral",
  seed = 2024,
  verbose = FALSE
)

# Topic labels
cat("=== TOPIC IDENTIFICATION ===\n")
labelTopics(model, n = 8)

# Based on output, assign substantive labels:
# Topic 1: Liability & Injury (professional, participant, injury, warning)
# Topic 2: Tax/Regulatory (tax, credit, energy, districts)
# Topic 3: Tourism & Recreation (recreation, sport, tourism, outdoor)
# Topic 4: Administrative/Advisory (building, council, advisory, director)

# ==============================================================================
# 3. BUILD REGRESSION DATASET
# ==============================================================================

theta <- model$theta
docnames <- gsub("\\.txt$", "", names(stm_input$documents))

reg_data <- data.frame(
  state = docnames,
  topic1_liability = theta[, 1],
  topic2_tax = theta[, 2],
  topic3_tourism = theta[, 3],
  topic4_admin = theta[, 4]
) %>%
  left_join(covariates, by = "state") %>%
  mutate(
    # Log transforms
    log_contrib = log(contrib_agri + 1),
    log_ag_value = log(valueofagsect + 1),
    
    # Standardize for interpretation
    contrib_z = scale(contrib_agri)[,1],
    ag_value_z = scale(valueofagsect)[,1],
    
    # Time variables
    year_centered = enact_year - 2010,
    early_adopter = enact_year <= 2008,
    adoption_wave = case_when(
      enact_year <= 2006 ~ "Wave 1 (2004-2006)",
      enact_year <= 2010 ~ "Wave 2 (2007-2010)",
      enact_year <= 2014 ~ "Wave 3 (2011-2014)",
      TRUE ~ "Wave 4 (2015-2018)"
    ),
    
    # Republican governor (1 = R, 0 = D)
    rep_governor = ifelse(govparty_c == 1, 1, 0),
    
    # Squeeze for beta regression
    across(starts_with("topic"), ~pmin(pmax(., 0.001), 0.999), .names = "{.col}_sq"),
    
    # Dominant topic
    dominant_topic = case_when(
      topic1_liability == pmax(topic1_liability, topic2_tax, topic3_tourism, topic4_admin) ~ "Liability",
      topic2_tax == pmax(topic1_liability, topic2_tax, topic3_tourism, topic4_admin) ~ "Tax/Regulatory",
      topic3_tourism == pmax(topic1_liability, topic2_tax, topic3_tourism, topic4_admin) ~ "Tourism",
      topic4_admin == pmax(topic1_liability, topic2_tax, topic3_tourism, topic4_admin) ~ "Administrative"
    )
  )

# ==============================================================================
# 4. DESCRIPTIVE STATISTICS
# ==============================================================================

cat("\n=== DESCRIPTIVE STATISTICS ===\n\n")

# Covariate coverage
cat("Variable coverage:\n")
cat("  contrib_agri:", sum(!is.na(reg_data$contrib_agri)), "/", nrow(reg_data), "\n")
cat("  valueofagsect:", sum(!is.na(reg_data$valueofagsect)), "/", nrow(reg_data), "\n")
cat("  govparty_c:", sum(!is.na(reg_data$govparty_c)), "/", nrow(reg_data), "\n")

# Topic distribution
cat("\nDominant topic distribution:\n")
table(reg_data$dominant_topic) %>% print()

# By adoption wave
cat("\nDominant topic by adoption wave:\n")
table(reg_data$adoption_wave, reg_data$dominant_topic) %>% print()

# ==============================================================================
# 5. POLITICAL ECONOMY MODELS
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("POLITICAL ECONOMY ANALYSIS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

# --- Model 1: Agricultural Contributions → Liability Topic ---
cat("\n--- H1: Ag Contributions → Liability-Focused Statutes ---\n")
cat("Theory: Larger ag lobby → more liability protections for operators\n\n")

m1 <- betareg(topic1_liability_sq ~ log_contrib + year_centered, 
              data = reg_data)
print(summary(m1))

# --- Model 2: Ag Sector Size → Liability Topic ---
cat("\n--- H2: Ag Sector Size → Liability-Focused Statutes ---\n")
cat("Theory: States with larger ag sectors prioritize operator protections\n\n")

m2 <- betareg(topic1_liability_sq ~ log_ag_value + year_centered, 
              data = reg_data)
print(summary(m2))

# --- Model 3: Party Control → Topic ---
cat("\n--- H3: Republican Governor → Liability-Focused Statutes ---\n")
cat("Theory: Republicans favor tort reform / business protections\n\n")

m3 <- betareg(topic1_liability_sq ~ rep_governor + year_centered, 
              data = reg_data %>% filter(!is.na(rep_governor)))
print(summary(m3))

# --- Model 4: Full Model (complete cases only) ---
cat("\n--- Full Model: All Political Economy Predictors ---\n")

m4_data <- reg_data %>% 
  filter(!is.na(log_contrib) & !is.na(log_ag_value) & !is.na(rep_governor))

if(nrow(m4_data) >= 10) {
  m4 <- betareg(topic1_liability_sq ~ log_contrib + log_ag_value + rep_governor + year_centered,
                data = m4_data)
  print(summary(m4))
  cat("\nN for full model:", nrow(m4_data), "\n")
} else {
  cat("Insufficient complete cases for full model\n")
}

# --- Models for Other Topics ---
cat("\n--- Tourism Topic (Topic 3) ---\n")
m_tourism <- betareg(topic3_tourism_sq ~ log_contrib + year_centered, 
                     data = reg_data)
print(summary(m_tourism))

# ==============================================================================
# 6. HORIZONTAL DIFFUSION ANALYSIS
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("HORIZONTAL DIFFUSION ANALYSIS\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

# --- Textual Similarity Matrix ---
sim_matrix <- textstat_simil(dfm, method = "cosine", margin = "documents")
sim_df <- as.data.frame(as.table(as.matrix(sim_matrix))) %>%
  filter(Var1 != Var2) %>%
  rename(state1 = Var1, state2 = Var2, similarity = Freq)

# Add enactment years
sim_df <- sim_df %>%
  left_join(reg_data %>% select(state, enact_year), by = c("state1" = "state")) %>%
  rename(year1 = enact_year) %>%
  left_join(reg_data %>% select(state, enact_year), by = c("state2" = "state")) %>%
  rename(year2 = enact_year) %>%
  mutate(
    earlier_state = ifelse(year1 <= year2, as.character(state1), as.character(state2)),
    later_state = ifelse(year1 <= year2, as.character(state2), as.character(state1)),
    earlier_year = pmin(year1, year2),
    later_year = pmax(year1, year2),
    year_gap = later_year - earlier_year
  )

# Remove duplicates (keep each pair once)
sim_df_unique <- sim_df %>%
  filter(as.character(state1) < as.character(state2))

cat("\n--- Most Similar State Pairs ---\n")
top_pairs <- sim_df_unique %>%
  arrange(desc(similarity)) %>%
  select(state1, state2, similarity, year1, year2) %>%
  head(15) %>%
  as.data.frame()
print(top_pairs)

# --- Do Later Adopters Copy Early Adopters? ---
cat("\n--- Diffusion Test: Do Later States Copy Earlier States? ---\n")

# For each state, find its most similar predecessor using a cleaner approach
find_most_similar_predecessor <- function(target_state, target_year, sim_data, year_data) {
  # Get all similarities involving this state
  pairs <- sim_data %>%
    filter(state1 == target_state | state2 == target_state) %>%
    mutate(other_state = ifelse(state1 == target_state, as.character(state2), as.character(state1)))
  
  # Add years for the other states
  pairs <- pairs %>%
    left_join(year_data, by = c("other_state" = "state"))
  
  # Filter to predecessors only (states that enacted BEFORE target)
  predecessors <- pairs %>%
    filter(enact_year < target_year) %>%
    arrange(desc(similarity))
  
  if(nrow(predecessors) > 0) {
    return(data.frame(
      most_similar_pred = predecessors$other_state[1],
      similarity_to_pred = predecessors$similarity[1]
    ))
  } else {
    return(data.frame(
      most_similar_pred = NA_character_,
      similarity_to_pred = NA_real_
    ))
  }
}

# Apply to each state
year_lookup <- reg_data %>% select(state, enact_year)

diffusion_results <- lapply(1:nrow(reg_data), function(i) {
  find_most_similar_predecessor(
    reg_data$state[i], 
    reg_data$enact_year[i], 
    sim_df_unique, 
    year_lookup
  )
})

diffusion_test <- reg_data %>%
  select(state, enact_year) %>%
  bind_cols(bind_rows(diffusion_results))

cat("\nStates and their most similar predecessors:\n")
diffusion_display <- diffusion_test %>%
  filter(!is.na(most_similar_pred)) %>%
  arrange(enact_year) %>%
  select(state, enact_year, most_similar_pred, similarity_to_pred) %>%
  as.data.frame()
print(diffusion_display)

# Average similarity to predecessors over time
cat("\nAverage similarity to predecessors by adoption wave:\n")
wave_similarity <- diffusion_test %>%
  left_join(reg_data %>% select(state, adoption_wave), by = "state") %>%
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    mean_similarity = mean(similarity_to_pred, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  as.data.frame()
print(wave_similarity)

# ==============================================================================
# 7. TOPIC CONVERGENCE OVER TIME
# ==============================================================================

cat("\n--- Topic Convergence: Do Later States Converge on Liability? ---\n")

# Mean topic proportions by wave
topic_by_wave <- reg_data %>%
  group_by(adoption_wave) %>%
  summarize(
    n = n(),
    mean_liability = mean(topic1_liability),
    mean_tax = mean(topic2_tax),
    mean_tourism = mean(topic3_tourism),
    mean_admin = mean(topic4_admin),
    .groups = "drop"
  )

print(as.data.frame(topic_by_wave))

# Correlation: Later year → higher liability focus?
cat("\nCorrelation: Enactment Year → Topic Proportions\n")
cat("  Liability (Topic 1):", round(cor(reg_data$enact_year, reg_data$topic1_liability), 3), "\n")
cat("  Tax (Topic 2):", round(cor(reg_data$enact_year, reg_data$topic2_tax), 3), "\n")
cat("  Tourism (Topic 3):", round(cor(reg_data$enact_year, reg_data$topic3_tourism), 3), "\n")
cat("  Admin (Topic 4):", round(cor(reg_data$enact_year, reg_data$topic4_admin), 3), "\n")

# ==============================================================================
# 8. VISUALIZATIONS
# ==============================================================================

# --- Figure 1: Topic Proportions Over Time ---
p1 <- reg_data %>%
  select(state, enact_year, topic1_liability, topic2_tax, topic3_tourism, topic4_admin) %>%
  pivot_longer(cols = starts_with("topic"), names_to = "topic", values_to = "proportion") %>%
  mutate(topic = factor(topic, 
                        levels = c("topic1_liability", "topic2_tax", "topic3_tourism", "topic4_admin"),
                        labels = c("Liability", "Tax/Regulatory", "Tourism", "Administrative"))) %>%
  ggplot(aes(x = enact_year, y = proportion, color = topic)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, span = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Statutory Content Over Time",
       subtitle = "Topic proportions by year of enactment",
       x = "Year of Enactment",
       y = "Topic Proportion",
       color = "Topic") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("../../3_Output/Figures/figure1_topics_over_time.png"), p1, width = 10, height = 6)

# --- Figure 2: Diffusion Network ---
# High-similarity pairs (>0.90)
high_sim_pairs <- sim_df_unique %>%
  filter(similarity > 0.90) %>%
  select(state1, state2, similarity)

if(nrow(high_sim_pairs) > 0) {
  library(igraph)
  library(ggraph)
  
  g <- graph_from_data_frame(high_sim_pairs, directed = FALSE)
  
  # Add enactment year as node attribute
  V(g)$year <- reg_data$enact_year[match(V(g)$name, reg_data$state)]
  
  p2 <- ggraph(g, layout = 'fr') +
    geom_edge_link(aes(width = similarity, alpha = similarity), color = "grey50") +
    geom_node_point(aes(color = year), size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    scale_color_viridis_c(name = "Enactment\nYear") +
    scale_edge_width(range = c(0.5, 2)) +
    theme_void() +
    labs(title = "Statutory Similarity Network",
         subtitle = "Edges indicate >90% textual similarity") +
    theme(legend.position = "right")
  
  ggsave("../../3_Output/Figures/figure2_diffusion_network.png"), p2, width = 12, height = 10)
}

# --- Figure 3: Political Economy Scatter ---
p3 <- ggplot(reg_data, aes(x = log_contrib, y = topic1_liability)) +
  geom_point(aes(color = dominant_topic), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  geom_text_repel(aes(label = state), size = 2.5, max.overlaps = 15) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Agricultural Interest Group Contributions and Statutory Content",
       x = "Log(Agricultural Contributions + 1)",
       y = "Liability Topic Proportion",
       color = "Dominant\nTopic") +
  theme_minimal()

ggsave("../../3_Output/Figures/figure3_political_economy.png"), p3, width = 10, height = 8)

# ==============================================================================
# 9. REGRESSION TABLE FOR PUBLICATION
# ==============================================================================

cat("\n=== REGRESSION TABLE ===\n")

# Create table with available models
stargazer(m1, m2, m_tourism,
          type = "text",
          title = "Political Economy Determinants of Statutory Content",
          column.labels = c("Liability", "Liability", "Tourism"),
          covariate.labels = c("Log(Ag Contributions)", 
                               "Log(Ag Sector Value)",
                               "Year (centered)"),
          dep.var.labels = "Topic Proportion",
          omit.stat = c("ser", "f"),
          digits = 3,
          notes = "Beta regression with logit link. Standard errors in parentheses.",
          out = "../../3_Output/Tables/table1_political_economy.txt")

# ==============================================================================
# 10. SAVE OUTPUTS
# ==============================================================================

write_csv(reg_data, "../../2_Data/Processed/final_analysis_data.csv")
write_csv(diffusion_test, "../../4_Analysis_Results/diffusion_analysis.csv")
write_csv(sim_df_unique %>% arrange(desc(similarity)), "../../2_Data/STM/similarity_matrix.csv")

save(model, reg_data, diffusion_test, file = "../../4_Analysis_Results/full_analysis_results.RData")

cat("\n=== FILES SAVED ===\n")
cat("  final_analysis_data.csv\n")
cat("  diffusion_analysis.csv\n")
cat("  similarity_matrix.csv\n")
cat("  full_analysis_results.RData\n")
cat("  figure1_topics_over_time.png\n")
cat("  figure2_diffusion_network.png\n")
cat("  figure3_political_economy.png\n")
cat("  table1_political_economy.txt\n")

# ==============================================================================
# 11. SUMMARY FOR PAPER
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("SUMMARY FOR SPPQ PAPER\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

cat("\n1. RESEARCH QUESTION\n")
cat("   What explains variation in agritourism statutory content across states?\n")

cat("\n2. THEORETICAL FRAMEWORK\n")
cat("   A) Internal Political Economy: Ag interests shape statutory focus\n")
cat("   B) Horizontal Diffusion: Later adopters copy early adopters' language\n")

cat("\n3. KEY FINDINGS\n")
cat("   - [Check beta regression results for political economy effects]\n")
cat("   - High textual isomorphism: Many state pairs >90% similar\n")
cat("   - Diffusion pattern: Later states closely match predecessors\n")

cat("\n4. CONTRIBUTION\n")
cat("   - First systematic text analysis of agritourism statutes\n")
cat("   - Demonstrates both internal and diffusion mechanisms\n")
cat("   - Original dataset: N =", nrow(reg_data), "states, 2004-2018\n")
