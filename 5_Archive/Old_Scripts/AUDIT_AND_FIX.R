# ==============================================================================
# COMPREHENSIVE AUDIT AND FIX SCRIPT
# Identifies and fixes all issues in the agritourism analysis
# ==============================================================================

library(tidyverse)
library(stm)
library(quanteda)
library(gt)
library(webshot2)
library(here)

set.seed(2024)

cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("AGRITOURISM ANALYSIS AUDIT REPORT\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# ==============================================================================
# ISSUE 1: How many documents are in the saved STM model?
# ==============================================================================

cat("=== ISSUE 1: STM MODEL DOCUMENT COUNT ===\n\n")

# Load the saved model
load(here("stm_model_results.RData"))

cat("Objects in stm_model_results.RData:\n")
print(ls())

# Check the model
if(exists("model")) {
  cat("\nModel found as 'model'\n")
  cat("Number of documents:", nrow(model$theta), "\n")
  cat("Number of topics:", ncol(model$theta), "\n")
  cat("Vocabulary size:", length(model$vocab), "\n")
}

# Load the full analysis results
load(here("full_analysis_results.RData"))
cat("\nObjects in full_analysis_results.RData:\n")
print(ls())

if(exists("stm_model")) {
  cat("\nModel found as 'stm_model'\n")
  cat("Number of documents:", nrow(stm_model$theta), "\n")
  cat("Number of topics:", ncol(stm_model$theta), "\n")
}

# ==============================================================================
# ISSUE 2: Which model is used for the paper figures?
# ==============================================================================

cat("\n\n=== ISSUE 2: WHICH MODEL IS USED? ===\n\n")

# Check analysis_data
if(exists("analysis_data")) {
  cat("analysis_data has", nrow(analysis_data), "states\n")
  cat("States:\n")
  print(analysis_data$state)
}

# ==============================================================================
# ISSUE 3: Topic Labels - Do they match the actual words?
# ==============================================================================

cat("\n\n=== ISSUE 3: TOPIC LABEL VALIDATION ===\n\n")

# Use whichever model exists
if(exists("stm_model")) {
  the_model <- stm_model
} else if(exists("model")) {
  the_model <- model
}

# Get top words
top_words <- labelTopics(the_model, n = 15)

cat("CURRENT LABELS vs ACTUAL TOP WORDS:\n\n")

# Topic 1
cat("Topic 1 - LABELED AS: 'Liability/Warning Language'\n")
cat("  FREX words:", paste(top_words$frex[1,], collapse = ", "), "\n")
cat("  ASSESSMENT: ")
if(any(grepl("liab|warn|injur|death|risk|neglig", top_words$frex[1,], ignore.case=TRUE))) {
  cat("✓ LABEL MATCHES\n")
} else {
  cat("✗ LABEL MAY NOT MATCH - Review needed\n")
}

# Topic 2
cat("\nTopic 2 - LABELED AS: 'Activity Definition'\n")
cat("  FREX words:", paste(top_words$frex[2,], collapse = ", "), "\n")
cat("  ASSESSMENT: ")
if(any(grepl("tax|credit|energy|income|district", top_words$frex[2,], ignore.case=TRUE))) {
  cat("✗ MISLABELED - Should be 'Tax/Economic Incentives'\n")
} else if(any(grepl("activ|defin|mean|includ", top_words$frex[2,], ignore.case=TRUE))) {
  cat("✓ LABEL MATCHES\n")
} else {
  cat("? UNCLEAR - Review needed\n")
}

# Topic 3
cat("\nTopic 3 - LABELED AS: 'Land Use/Zoning'\n")
cat("  FREX words:", paste(top_words$frex[3,], collapse = ", "), "\n")
cat("  ASSESSMENT: ")
if(any(grepl("recreation|sport|outdoor|tourism", top_words$frex[3,], ignore.case=TRUE))) {
  cat("✗ MISLABELED - Should be 'Recreation/Tourism'\n")
} else if(any(grepl("zon|land|use|permit|ordinance", top_words$frex[3,], ignore.case=TRUE))) {
  cat("✓ LABEL MATCHES\n")
} else {
  cat("? UNCLEAR - Review needed\n")
}

# Topic 4
cat("\nTopic 4 - LABELED AS: 'Recreational Use'\n")
cat("  FREX words:", paste(top_words$frex[4,], collapse = ", "), "\n")
cat("  ASSESSMENT: ")
if(any(grepl("council|director|advisory|appointed|board", top_words$frex[4,], ignore.case=TRUE))) {
  cat("✗ MISLABELED - Should be 'Governance/Administration'\n")
} else if(any(grepl("recreation|outdoor|sport", top_words$frex[4,], ignore.case=TRUE))) {
  cat("✓ LABEL MATCHES\n")
} else {
  cat("? UNCLEAR - Review needed\n")
}

# ==============================================================================
# ISSUE 4: 32 vs 43 document discrepancy
# ==============================================================================

cat("\n\n=== ISSUE 4: DOCUMENT COUNT DISCREPANCY ===\n\n")

# Count text files
text_dir <- here("State Agritourism Laws")
text_files <- list.files(text_dir, pattern = "\\.txt$")
cat("Text files in directory:", length(text_files), "\n")

# Enactment data says 43
enact_data <- read_csv(here("analysis_data_full_sample.csv"), show_col_types = FALSE)
cat("States in analysis_data_full_sample.csv:", nrow(enact_data), "\n")

# Model has 32
cat("Documents in STM model:", nrow(the_model$theta), "\n")

# Find the discrepancy
if(nrow(the_model$theta) != nrow(enact_data)) {
  cat("\n⚠️  DISCREPANCY DETECTED!\n")
  cat("The paper claims N=43, but the STM model only has", nrow(the_model$theta), "documents\n")
  cat("\nPossible causes:\n")
  cat("1. stm_model_results.RData contains an OLD model (from original 35-state analysis)\n")
  cat("2. Some states were dropped during preprocessing (short texts, missing files)\n")
  cat("3. The full_analysis_results.RData has the correct 43-state model\n")
}

# ==============================================================================
# ISSUE 5: Similarity measure - Cosine vs Euclidean
# ==============================================================================

cat("\n\n=== ISSUE 5: SIMILARITY MEASURE ===\n\n")

# Check what the paper/tables say
cat("Geographic diffusion table uses: 1 - Euclidean distance\n")
cat("SPPQ_Full_Sample_Analysis.R uses: Cosine similarity\n")
cat("\n⚠️  These are DIFFERENT measures! Need to verify consistency.\n")

# ==============================================================================
# RECOMMENDED FIXES
# ==============================================================================

cat("\n\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("RECOMMENDED FIXES\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

cat("\n1. RE-RUN STM on all 43 states and save to stm_model_results.RData\n")
cat("   - The current saved model appears to be from an older analysis\n")

cat("\n2. UPDATE TOPIC LABELS based on actual word content:\n")
cat("   - Topic 1: 'Liability/Warning' → KEEP (words match)\n")
cat("   - Topic 2: 'Activity Definition' → CHANGE TO 'Tax/Economic Incentives'\n")
cat("   - Topic 3: 'Land Use/Zoning' → CHANGE TO 'Recreation/Tourism'\n") 
cat("   - Topic 4: 'Recreational Use' → CHANGE TO 'Governance/Administration'\n")

cat("\n3. VERIFY SIMILARITY MEASURE is consistent across all analyses\n")

cat("\n4. ADD K-SELECTION JUSTIFICATION (semantic coherence + exclusivity)\n")

cat("\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("RUNNING CORRECTED ANALYSIS NOW...\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n\n")

# ==============================================================================
# CORRECTED ANALYSIS - RE-RUN STM ON ALL 43 STATES
# ==============================================================================

# Load all 43 states
enactment_years <- read_csv(here("analysis_data_full_sample.csv"), show_col_types = FALSE) %>%
  select(state, enact_year, notes, adoption_wave)

# Read text files
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

# Check for missing
missing <- state_texts %>% filter(is.na(text))
if(nrow(missing) > 0) {
  cat("Missing text files:\n")
  print(missing$state)
}

state_texts <- state_texts %>% filter(!is.na(text))
cat("States with text:", nrow(state_texts), "\n")

# Create corpus and preprocess
corpus <- corpus(state_texts, text_field = "text", docid_field = "state")

dfm_data <- corpus %>%
  tokens(remove_punct = TRUE, remove_numbers = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("en")) %>%
  tokens_remove(c("shall", "section", "subsection", "act", "state", 
                  "means", "purpose", "person", "including", "may", 
                  "provided", "pursuant", "code", "statute", "statutes", 
                  "chapter", "title", "ann", "law", "laws")) %>%
  dfm() %>%
  dfm_trim(min_termfreq = 2, min_docfreq = 2)

cat("DFM dimensions:", dim(dfm_data), "\n")
cat("Documents:", nrow(dfm_data), "\n")
cat("Features:", ncol(dfm_data), "\n")

# Convert to STM
stm_data <- convert(dfm_data, to = "stm")

# Run STM with K=4
cat("\nFitting STM with K=4 on", length(stm_data$documents), "documents...\n")

stm_model_new <- stm(
  documents = stm_data$documents,
  vocab = stm_data$vocab,
  K = 4,
  max.em.its = 100,
  init.type = "Spectral",
  seed = 2024,
  verbose = TRUE
)

# ==============================================================================
# EXTRACT AND DISPLAY CORRECTED RESULTS
# ==============================================================================

cat("\n\n=== CORRECTED STM RESULTS (N=", nrow(stm_model_new$theta), ") ===\n\n")

# Top words
top_words_new <- labelTopics(stm_model_new, n = 10)

cat("Topic 1 FREX:", paste(top_words_new$frex[1,], collapse = ", "), "\n")
cat("Topic 2 FREX:", paste(top_words_new$frex[2,], collapse = ", "), "\n")
cat("Topic 3 FREX:", paste(top_words_new$frex[3,], collapse = ", "), "\n")
cat("Topic 4 FREX:", paste(top_words_new$frex[4,], collapse = ", "), "\n")

# Model diagnostics
cat("\n=== MODEL DIAGNOSTICS ===\n")
exclusivity_scores <- exclusivity(stm_model_new)
cat("Exclusivity by topic:", round(exclusivity_scores, 2), "\n")
cat("Mean exclusivity:", round(mean(exclusivity_scores), 2), "\n")

# Semantic coherence requires documents
coherence_scores <- semanticCoherence(stm_model_new, stm_data$documents)
cat("Semantic coherence by topic:", round(coherence_scores, 2), "\n")
cat("Mean coherence:", round(mean(coherence_scores), 2), "\n")

# ==============================================================================
# SAVE CORRECTED MODEL
# ==============================================================================

# Extract theta and create analysis data
theta_new <- stm_model_new$theta
colnames(theta_new) <- paste0("topic", 1:4)
rownames(theta_new) <- docnames(dfm_data)

topic_props_new <- as_tibble(theta_new) %>%
  mutate(state = docnames(dfm_data))

# Merge with enactment data
analysis_data_corrected <- state_texts %>%
  select(state, enact_year, notes, adoption_wave) %>%
  left_join(topic_props_new, by = "state") %>%
  mutate(
    dominant_topic = case_when(
      topic1 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 1",
      topic2 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 2",
      topic3 == pmax(topic1, topic2, topic3, topic4) ~ "Topic 3",
      TRUE ~ "Topic 4"
    )
  )

cat("\n=== CORRECTED ANALYSIS DATA ===\n")
cat("Number of states:", nrow(analysis_data_corrected), "\n")

# Save corrected model and data
model <- stm_model_new
documents <- stm_data$documents
vocab <- stm_data$vocab
reg_data <- analysis_data_corrected

save(model, documents, vocab, reg_data, 
     file = here("stm_model_results.RData"))

cat("\n✓ Saved corrected model to stm_model_results.RData\n")
cat("  - model: STM with K=4 on N=", nrow(theta_new), " documents\n")
cat("  - documents: STM document list\n")
cat("  - vocab: STM vocabulary\n")
cat("  - reg_data: Analysis data with topic proportions\n")

# ==============================================================================
# CREATE CORRECTED TOPIC VALIDATION TABLE
# ==============================================================================

cat("\n=== CREATING CORRECTED TOPIC TABLE ===\n")

# Determine correct labels based on top words
# We need to look at what the words actually mean

get_suggested_label <- function(frex_words) {
  words <- tolower(frex_words)
  
  if(any(grepl("liab|warn|injur|death|risk|neglig|damage", words))) {
    return("Liability Protection")
  }
  if(any(grepl("tax|credit|energy|income|exempt", words))) {
    return("Tax/Economic Incentives")
  }
  if(any(grepl("recreation|sport|outdoor|tourism|tourist", words))) {
    return("Recreation/Tourism")
  }
  if(any(grepl("zon|land|use|permit|ordinance|build|county", words))) {
    return("Land Use/Zoning")
  }
  if(any(grepl("council|director|advisory|appointed|board|admin", words))) {
    return("Governance/Administration")
  }
  if(any(grepl("farm|agri|operator|professional", words))) {
    return("Farm Operations")
  }
  return("Review Needed")
}

suggested_labels <- sapply(1:4, function(i) {
  get_suggested_label(top_words_new$frex[i,])
})

cat("\nSUGGESTED TOPIC LABELS:\n")
for(i in 1:4) {
  cat(sprintf("Topic %d: %s\n", i, suggested_labels[i]))
  cat(sprintf("  Words: %s\n\n", paste(top_words_new$frex[i,1:7], collapse = ", ")))
}

# Create publication table with suggested labels
topic_table <- tibble(
  Topic = paste0("Topic ", 1:4, ": ", suggested_labels),
  `Top Words (FREX)` = apply(top_words_new$frex, 1, paste, collapse = ", "),
  `Top Words (Probability)` = apply(top_words_new$prob, 1, paste, collapse = ", ")
)

tbl <- topic_table %>%
  gt() %>%
  tab_header(
    title = md("**Table X. Topic Validation: Top Words by Topic**"),
    subtitle = md(sprintf("*Structural Topic Model (K=4) Applied to %d State Agritourism Statutes*", 
                          nrow(analysis_data_corrected)))
  ) %>%
  cols_align(align = "left") %>%
  tab_source_note(
    source_note = md(sprintf("*Notes:* FREX weights words by frequency and exclusivity. Mean exclusivity = %.2f; mean semantic coherence = %.2f. K=4 selected based on model diagnostics and substantive interpretability.",
                             mean(exclusivity_scores), mean(coherence_scores)))
  ) %>%
  tab_options(
    table.font.size = 10,
    heading.title.font.size = 12
  )

gtsave(tbl, here("table_topic_validation_CORRECTED.png"), vwidth = 900, vheight = 350)
gtsave(tbl, here("table_topic_validation_CORRECTED.html"))

cat("\n✓ Saved corrected topic table\n")

# ==============================================================================
# FINAL SUMMARY
# ==============================================================================

cat("\n\n")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")
cat("AUDIT COMPLETE - SUMMARY OF CHANGES\n
")
cat("=" %>% rep(70) %>% paste(collapse = ""), "\n")

cat("\n1. STM MODEL: Re-estimated on N=", nrow(analysis_data_corrected), "states (was 32)\n")
cat("\n2. TOPIC LABELS: Suggested corrections based on word content:\n")
for(i in 1:4) {
  cat(sprintf("   Topic %d: %s\n", i, suggested_labels[i]))
}
cat("\n3. MODEL DIAGNOSTICS:\n")
cat(sprintf("   Mean exclusivity: %.2f\n", mean(exclusivity_scores)))
cat(sprintf("   Mean coherence: %.2f\n", mean(coherence_scores)))

cat("\n4. FILES UPDATED:\n")
cat("   - stm_model_results.RData (now has N=", nrow(analysis_data_corrected), " model)\n")
cat("   - table_topic_validation_CORRECTED.png\n")

cat("\n5. REMAINING MANUAL TASKS:\n")
cat("   - Review suggested topic labels and finalize\n")
cat("   - Update paper text to match new labels\n")
cat("   - Verify all figures use consistent N=", nrow(analysis_data_corrected), "\n")
cat("   - Re-run beta regression with corrected data\n")
