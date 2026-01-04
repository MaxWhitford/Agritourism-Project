# ==============================================================================
# Title:        Agritourism Statute Analysis: Litigation → Statutory Content
# Author:       Max Whitford
# Target:       State Politics & Policy Quarterly
# ==============================================================================
# 
# KEY HYPOTHESIS: Pre-statute litigation predicts statutory content
# - States with more liability litigation → Topic 1 (Liability & Injury)
# - States with zoning litigation → Topic 4 (Land Use & Zoning)
#
# ==============================================================================

library(tidyverse)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(stm)
library(betareg)
library(stargazer)
library(here)

set.seed(2024)

# ==============================================================================
# 1. LOAD ALL DATA
# ==============================================================================

cat("=== LOADING DATA ===\n")

# Load pre-statute case counts (from scraping NALC)
case_counts <- read_csv("../../4_Analysis_Results/pre_statute_case_counts.csv")
cat("Litigation data: ", nrow(case_counts), " states\n")

# Load statutory text files
files <- list.files(path = "../../State Agritourism Laws", 
                    pattern = "*.txt", full.names = TRUE)
texts <- readtext(files)
texts$state <- tools::file_path_sans_ext(basename(texts$doc_id))
cat("Text files: ", nrow(texts), " states\n")

# Merge - only keep states with BOTH text AND case data
analysis_df <- texts %>%
  inner_join(case_counts, by = "state")

cat("Merged sample: ", nrow(analysis_df), " states\n\n")

# ==============================================================================
# 2. TEXT PREPROCESSING
# ==============================================================================

corp <- corpus(analysis_df, text_field = "text", docid_field = "state")

dfm <- dfm(tokens(corp, remove_punct = TRUE, remove_numbers = TRUE)) %>%
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 2)

cat("Corpus: ", ndoc(corp), " documents, ", nfeat(dfm), " features\n\n")

# ==============================================================================
# 3. STRUCTURAL TOPIC MODEL (K=4)
# ==============================================================================

cat("=== RUNNING STM ===\n")

stm_input <- convert(dfm, to = "stm")

model <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  K = 4,
  max.em.its = 100,
  init.type = "Spectral",
  seed = 2024,
  verbose = FALSE
)

# ==============================================================================
# 4. LABEL TOPICS
# ==============================================================================

cat("\n=== TOPIC LABELS (Top Words) ===\n")
labels <- labelTopics(model, n = 10)
print(labels)

# Display FREX words (most distinctive)
cat("\n=== FREX WORDS (Most Distinctive) ===\n")
for (i in 1:4) {
  cat("Topic ", i, ": ", paste(labels$frex[i, 1:8], collapse = ", "), "\n")
}

# ==============================================================================
# 5. CREATE ANALYSIS DATASET
# ==============================================================================

theta <- model$theta
docnames <- gsub("\\.txt$", "", names(stm_input$documents))

# Build regression data
reg_data <- data.frame(
  state = docnames,
  topic1 = theta[, 1],
  topic2 = theta[, 2],
  topic3 = theta[, 3],
  topic4 = theta[, 4]
) %>%
  left_join(case_counts, by = "state") %>%
  mutate(
    # Log transformations for count variables
    log_pre_cases = log(pre_statute_cases + 1),
    log_pre_liability = log(pre_statute_liability + 1),
    
    # Binary indicators
    any_litigation = pre_statute_cases > 0,
    any_liability = pre_statute_liability > 0,
    high_litigation = pre_statute_cases >= 5,
    
    # Squeeze topic proportions for beta regression
    topic1_sq = pmin(pmax(topic1, 0.001), 0.999),
    topic2_sq = pmin(pmax(topic2, 0.001), 0.999),
    topic3_sq = pmin(pmax(topic3, 0.001), 0.999),
    topic4_sq = pmin(pmax(topic4, 0.001), 0.999),
    
    # Time control
    year_centered = enact_year - 2010
  )

cat("\n=== ANALYSIS SAMPLE ===\n")
cat("N = ", nrow(reg_data), " states\n")
cat("States with pre-statute liability cases: ", sum(reg_data$any_liability), "\n")
cat("States with high litigation (5+): ", sum(reg_data$high_litigation), "\n")

# ==============================================================================
# 6. DESCRIPTIVE STATISTICS
# ==============================================================================

cat("\n=== DESCRIPTIVE STATISTICS ===\n")

# Topic proportions summary
cat("\nTopic Proportions:\n")
reg_data %>%
  select(starts_with("topic") & !ends_with("_sq")) %>%
  summary() %>%
  print()

# Litigation summary
cat("\nPre-Statute Litigation:\n")
reg_data %>%
  select(pre_statute_cases, pre_statute_liability, pre_statute_equine) %>%
  summary() %>%
  print()

# Cross-tab: Litigation level by dominant topic
reg_data <- reg_data %>%
  mutate(dominant_topic = case_when(
    topic1 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 1",
    topic2 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 2",
    topic3 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 3",
    topic4 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 4"
  ))

cat("\nDominant Topic by Litigation Level:\n")
table(reg_data$high_litigation, reg_data$dominant_topic) %>% print()

# ==============================================================================
# 7. KEY HYPOTHESIS TESTS
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("HYPOTHESIS TESTS: Pre-Statute Litigation → Statutory Content\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

# --- BIVARIATE CORRELATIONS ---
cat("\n=== CORRELATIONS ===\n")

cor_matrix <- reg_data %>%
  select(topic1, topic2, topic3, topic4, 
         pre_statute_cases, pre_statute_liability) %>%
  cor(use = "complete.obs")

cat("\nCorrelation: Pre-statute liability cases with topic proportions:\n")
cat("  Topic 1: ", round(cor_matrix["pre_statute_liability", "topic1"], 3), "\n")
cat("  Topic 2: ", round(cor_matrix["pre_statute_liability", "topic2"], 3), "\n")
cat("  Topic 3: ", round(cor_matrix["pre_statute_liability", "topic3"], 3), "\n")
cat("  Topic 4: ", round(cor_matrix["pre_statute_liability", "topic4"], 3), "\n")

# --- BETA REGRESSION MODELS ---
cat("\n=== BETA REGRESSION MODELS ===\n")

# Model 1: Topic 1 (expected to be Liability)
cat("\n--- Model 1: Topic 1 = f(Pre-statute Liability) ---\n")
m1 <- betareg(topic1_sq ~ log_pre_liability + year_centered, data = reg_data)
print(summary(m1))

# Model 2: Topic 1 with total case count
cat("\n--- Model 2: Topic 1 = f(All Pre-statute Cases) ---\n")
m2 <- betareg(topic1_sq ~ log_pre_cases + year_centered, data = reg_data)
print(summary(m2))

# Model 3: Using binary indicator
cat("\n--- Model 3: Topic 1 = f(Any Liability Litigation) ---\n")
m3 <- betareg(topic1_sq ~ any_liability + year_centered, data = reg_data)
print(summary(m3))

# Models for other topics (to show contrast)
cat("\n--- Model 4: Topic 4 = f(Pre-statute Liability) ---\n")
m4 <- betareg(topic4_sq ~ log_pre_liability + year_centered, data = reg_data)
print(summary(m4))

# ==============================================================================
# 8. T-TEST COMPARISON
# ==============================================================================

cat("\n=== T-TEST: Topic Proportions by Litigation Status ===\n")

# States WITH vs WITHOUT pre-statute liability cases
cat("\nTopic 1 (Liability) proportion:\n")
cat("  States WITH liability litigation: ", 
    mean(reg_data$topic1[reg_data$any_liability], na.rm = TRUE) %>% round(3), "\n")
cat("  States WITHOUT liability litigation: ", 
    mean(reg_data$topic1[!reg_data$any_liability], na.rm = TRUE) %>% round(3), "\n")

t_test_result <- t.test(topic1 ~ any_liability, data = reg_data)
cat("  t =", round(t_test_result$statistic, 2), 
    ", p =", round(t_test_result$p.value, 3), "\n")

# ==============================================================================
# 9. VISUALIZATIONS
# ==============================================================================

library(ggplot2)

# Scatter: Pre-statute liability cases vs Topic 1 proportion
p1 <- ggplot(reg_data, aes(x = pre_statute_liability, y = topic1)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  geom_text(aes(label = state), hjust = -0.1, vjust = 0, size = 2.5, 
            check_overlap = TRUE) +
  labs(title = "Pre-Statute Liability Litigation and Statutory Content",
       subtitle = "Higher Topic 1 = More Liability/Injury Focus",
       x = "Pre-Statute Liability Cases",
       y = "Topic 1 Proportion") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("../../3_Output/Figures/litigation_topic1_scatter.png"), p1, width = 10, height = 8)
cat("\nSaved: litigation_topic1_scatter.png\n")

# Box plot: Topic proportions by litigation status
reg_data_long <- reg_data %>%
  pivot_longer(cols = c(topic1, topic2, topic3, topic4),
               names_to = "topic", values_to = "proportion") %>%
  mutate(topic = factor(topic, 
                        levels = c("topic1", "topic2", "topic3", "topic4"),
                        labels = c("Liability", "Regulatory", "Tourism", "Land Use")))

p2 <- ggplot(reg_data_long, aes(x = topic, y = proportion, fill = any_liability)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("FALSE" = "#3498DB", "TRUE" = "#E74C3C"),
                    labels = c("No Pre-Statute\nLiability Cases", 
                               "Had Pre-Statute\nLiability Cases")) +
  labs(title = "Topic Proportions by Pre-Statute Litigation History",
       x = NULL,
       y = "Topic Proportion",
       fill = "Litigation\nHistory") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom")

ggsave("../../3_Output/Figures/topic_by_litigation_boxplot.png", p2, width = 10, height = 6)
cat("Saved: topic_by_litigation_boxplot.png\n")

# ==============================================================================
# 10. EXPORT REGRESSION TABLE (for paper)
# ==============================================================================

# Create publication-ready table
cat("\n=== REGRESSION TABLE (Stargazer) ===\n")

stargazer(m1, m2, m3, m4,
          type = "text",
          title = "Beta Regression: Pre-Statute Litigation and Statutory Content",
          column.labels = c("Topic 1", "Topic 1", "Topic 1", "Topic 4"),
          covariate.labels = c("Log(Liability Cases + 1)", 
                               "Log(All Cases + 1)",
                               "Any Liability Litigation",
                               "Year (centered)"),
          dep.var.labels = "Topic Proportion",
          omit.stat = c("ser", "f"),
          digits = 3,
          out = "../../3_Output/Tables/regression_table.txt")

cat("\nSaved: regression_table.txt\n")

# ==============================================================================
# 11. SAVE ANALYSIS DATA
# ==============================================================================

write_csv(reg_data, "../../2_Data/STM/stm_litigation_merged.csv")
cat("Saved: stm_litigation_merged.csv\n")

# Save STM model object
save(model, reg_data, file = "../../4_Analysis_Results/stm_model_results.RData")
cat("Saved: stm_model_results.RData\n")

# ==============================================================================
# 12. SUMMARY FOR PAPER
# ==============================================================================

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("SUMMARY FOR SPPQ PAPER\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

cat("\n1. SAMPLE\n")
cat("   N = ", nrow(reg_data), " states with agritourism statutes\n")
cat("   Period: ", min(reg_data$enact_year), "-", max(reg_data$enact_year), "\n")

cat("\n2. KEY FINDING\n")
cat("   Pre-statute liability litigation predicts liability-focused statutes\n")
cat("   Correlation (liability cases → Topic 1): ", 
    round(cor_matrix["pre_statute_liability", "topic1"], 3), "\n")

cat("\n3. THEORETICAL CONTRIBUTION\n")
cat("   - 'Reactive policymaking': States respond to litigation pressure\n")
cat("   - Explains regional variation in statutory content\n")
cat("   - Policy diffusion + litigation = complete explanation\n")

cat("\n4. HIGH-LIABILITY STATES (check Topic 1 dominance)\n")
high_liab <- reg_data %>%
  filter(pre_statute_liability >= 2) %>%
  select(state, pre_statute_liability, topic1, dominant_topic) %>%
  arrange(desc(pre_statute_liability))
print(high_liab)

cat("\n5. ZERO-LITIGATION STATES (check Topic 3/4 dominance)\n")
zero_liab <- reg_data %>%
  filter(pre_statute_cases == 0) %>%
  select(state, topic1, topic3, topic4, dominant_topic)
print(zero_liab)
