# ==============================================================================
# Topic Validation Table - Top Words per Topic
# Shows substantive content of each topic for reviewer validation
# ==============================================================================

library(tidyverse)
library(stm)
library(gt)
library(webshot2)
library(here)

# ==============================================================================
# LOAD STM MODEL
# ==============================================================================

# Load the saved STM model results
load("../../4_Analysis_Results/stm_model_results.RData")

# Check what objects are available
cat("Objects in RData file:\n")
print(ls())

# The model might be named 'model' instead of 'stm_model'
if(exists("model")) {
  stm_model <- model
  cat("\nFound model object named 'model'\n")
}

# Check model structure
cat("\nModel summary:\n")
print(stm_model)

# ==============================================================================
# EXTRACT TOP WORDS
# ==============================================================================

# Get number of topics
K <- stm_model$settings$dim$K
cat(sprintf("\nNumber of topics: %d\n", K))

# Get top words for each topic using different metrics
top_words <- labelTopics(stm_model, n = 10)

cat("\nTop words by topic:\n")
print(top_words)

# Create a dataframe with top words
topic_words <- data.frame(
  Topic = 1:K,
  Highest_Prob = apply(top_words$prob, 1, paste, collapse = ", "),
  FREX = apply(top_words$frex, 1, paste, collapse = ", ")
)

print(topic_words)

# ==============================================================================
# CREATE PUBLICATION TABLE
# ==============================================================================

# Define topic labels based on interpretation
# These should match what you've been using
topic_labels <- c(

  "Topic 1: Liability/Warning Language",
  "Topic 2: Activity Definition",
  "Topic 3: Land Use/Zoning",
  "Topic 4: Recreational Use"
)

# Truncate to K topics if needed
topic_labels <- topic_labels[1:K]

# Build table for publication
pub_table <- tibble(
  Topic = topic_labels,
  `Top Words (FREX)` = topic_words$FREX,
  `Top Words (Probability)` = topic_words$Highest_Prob
)

# Create gt table
tbl <- pub_table %>%
  gt() %>%
  tab_header(
    title = md("**Table X. Topic Validation: Top Words by Topic**"),
    subtitle = md("*Structural Topic Model (K=4) Applied to 43 State Agritourism Statutes*")
  ) %>%
  cols_align(align = "left") %>%
  cols_width(
    Topic ~ px(200),
    `Top Words (FREX)` ~ px(320),
    `Top Words (Probability)` ~ px(320)
  ) %>%
  tab_source_note(
    source_note = md("*Notes:* FREX weights words by frequency and exclusivity to the topic. Probability shows most frequent words within each topic. K=4 selected based on semantic coherence and exclusivity diagnostics. Topic labels assigned by author based on substantive interpretation.")
  ) %>%
  tab_options(
    table.font.size = 10,
    heading.title.font.size = 12,
    heading.subtitle.font.size = 10,
    source_notes.font.size = 8,
    column_labels.font.weight = "bold"
  )

print(tbl)

# Save
gtsave(tbl, "../../3_Output/Tables/table_topic_validation.png", vwidth = 880, vheight = 320)
gtsave(tbl, "../../3_Output/Tables/table_topic_validation.html")

cat("\n\nSaved: table_topic_validation.png\n")

# ==============================================================================
# MODEL DIAGNOSTICS FOR K SELECTION JUSTIFICATION
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("MODEL DIAGNOSTICS FOR METHODS SECTION\n")
cat(rep("=", 70), "\n", sep = "")

# Check if we have the documents object
if(exists("reg_data")) {
  cat("\nFound reg_data object\n")
}

# Try to get diagnostics if documents are available
tryCatch({
  # Semantic coherence
  if(exists("documents")) {
    coherence <- semanticCoherence(stm_model, documents)
    cat("\nSemantic Coherence by Topic:\n")
    for(i in 1:length(coherence)) {
      cat(sprintf("  Topic %d: %.2f\n", i, coherence[i]))
    }
    cat(sprintf("  Mean: %.2f\n", mean(coherence)))
  }
}, error = function(e) {
  cat("\nNote: Could not compute semantic coherence (documents object not found)\n")
  cat("You may need to report these from your original model fitting.\n")
})

# Exclusivity (doesn't need documents)
tryCatch({
  exclusivity_scores <- exclusivity(stm_model)
  cat("\nExclusivity by Topic:\n")
  for(i in 1:length(exclusivity_scores)) {
    cat(sprintf("  Topic %d: %.2f\n", i, exclusivity_scores[i]))
  }
  cat(sprintf("  Mean: %.2f\n", mean(exclusivity_scores)))
}, error = function(e) {
  cat("\nCould not compute exclusivity\n")
})

# ==============================================================================
# SUMMARY STATISTICS FOR PAPER
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("SUMMARY FOR PAPER\n")
cat(rep("=", 70), "\n", sep = "")

cat("\nSuggested text for methods section:\n")
cat("'I fit structural topic models with K ranging from 3 to 6 topics.\n")
cat("K=4 was selected based on a combination of semantic coherence,\n")
cat("exclusivity, and substantive interpretability of the resulting topics.\n")
cat("Table X presents the top words for each topic, which I label as:\n")
cat("(1) Liability/Warning Language, (2) Activity Definition,\n")
cat("(3) Land Use/Zoning, and (4) Recreational Use.'\n")
