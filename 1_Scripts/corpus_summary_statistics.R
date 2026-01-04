# ==============================================================================
# CORPUS SUMMARY STATISTICS - AGRITOURISM STATUTES
# ==============================================================================
# Calculates token/word counts and summary statistics for all statute documents
# N=43 (excludes Connecticut)
# ==============================================================================

library(tidyverse)
library(flextable)
library(officer)

cat("=== CORPUS SUMMARY STATISTICS ===\n\n")

# ==============================================================================
# 1. READ ALL STATUTE FILES (EXCLUDING CONNECTICUT)
# ==============================================================================

cat("Reading statute text files...\n")

# Directory with statute files
statute_dir <- "../State Agritourism Laws"

# Get all .txt files
statute_files <- list.files(statute_dir, pattern = "\\.txt$", full.names = TRUE)

cat("  Found", length(statute_files), "total files\n")

# Read each file and count tokens
corpus_stats <- map_df(statute_files, function(file) {
  # Extract state name from filename
  state_name <- basename(file) %>% str_remove("\\.txt$")
  
  # Read file
  text <- read_file(file)
  
  # Count tokens (words)
  # Split on whitespace and punctuation for word count
  words <- str_split(text, "\\s+")[[1]]
  words <- words[words != ""]  # Remove empty strings
  n_words <- length(words)
  
  # Count tokens (more strict - alphanumeric sequences)
  tokens <- str_extract_all(text, "\\b[A-Za-z0-9]+\\b")[[1]]
  n_tokens <- length(tokens)
  
  # Count characters (excluding whitespace)
  n_chars <- str_remove_all(text, "\\s+") %>% nchar()
  
  # Count sentences (approximate - periods followed by space/end)
  n_sentences <- str_count(text, "[.!?](?=\\s|$)")
  
  tibble(
    state = state_name,
    n_words = n_words,
    n_tokens = n_tokens,
    n_characters = n_chars,
    n_sentences = n_sentences,
    avg_words_per_sentence = if_else(n_sentences > 0, n_words / n_sentences, NA_real_)
  )
})

# EXCLUDE CONNECTICUT (not in final N=43 analysis)
corpus_stats <- corpus_stats %>%
  filter(state != "Connecticut")

cat("  Excluded Connecticut (not in final sample)\n")
cat("  Final corpus:", nrow(corpus_stats), "statutes\n\n")

# ==============================================================================
# 2. CALCULATE SUMMARY STATISTICS
# ==============================================================================

cat("=== SUMMARY STATISTICS ===\n\n")

cat("Documents (Statutes):", nrow(corpus_stats), "\n\n")

cat("Words per Document:\n")
word_summary <- summary(corpus_stats$n_words)
print(word_summary)
cat("  SD:", round(sd(corpus_stats$n_words), 2), "\n\n")

cat("Tokens per Document:\n")
token_summary <- summary(corpus_stats$n_tokens)
print(token_summary)
cat("  SD:", round(sd(corpus_stats$n_tokens), 2), "\n\n")

cat("Characters per Document:\n")
char_summary <- summary(corpus_stats$n_characters)
print(char_summary)
cat("  SD:", round(sd(corpus_stats$n_characters), 2), "\n\n")

cat("Sentences per Document:\n")
sent_summary <- summary(corpus_stats$n_sentences)
print(sent_summary)
cat("  SD:", round(sd(corpus_stats$n_sentences), 2), "\n\n")

cat("Total Corpus:\n")
cat("  Total words:", sum(corpus_stats$n_words), "\n")
cat("  Total tokens:", sum(corpus_stats$n_tokens), "\n")
cat("  Total characters:", sum(corpus_stats$n_characters), "\n\n")

# ==============================================================================
# 3. IDENTIFY OUTLIERS
# ==============================================================================

cat("=== SHORTEST AND LONGEST DOCUMENTS ===\n\n")

cat("Shortest 5 documents (by word count):\n")
shortest <- corpus_stats %>%
  arrange(n_words) %>%
  head(5) %>%
  select(state, n_words, n_tokens, n_sentences)
print(shortest)
cat("\n")

cat("Longest 5 documents (by word count):\n")
longest <- corpus_stats %>%
  arrange(desc(n_words)) %>%
  head(5) %>%
  select(state, n_words, n_tokens, n_sentences)
print(longest)
cat("\n")

# ==============================================================================
# 4. CREATE SUMMARY TABLE
# ==============================================================================

cat("=== CREATING SUMMARY TABLE ===\n\n")

# Create summary statistics table
summary_table <- tibble(
  Statistic = c("Documents", "Mean", "Median", "SD", "Min", "Max", "Total"),
  Words = c(
    nrow(corpus_stats),
    round(mean(corpus_stats$n_words), 1),
    round(median(corpus_stats$n_words), 1),
    round(sd(corpus_stats$n_words), 1),
    min(corpus_stats$n_words),
    max(corpus_stats$n_words),
    sum(corpus_stats$n_words)
  ),
  Tokens = c(
    nrow(corpus_stats),
    round(mean(corpus_stats$n_tokens), 1),
    round(median(corpus_stats$n_tokens), 1),
    round(sd(corpus_stats$n_tokens), 1),
    min(corpus_stats$n_tokens),
    max(corpus_stats$n_tokens),
    sum(corpus_stats$n_tokens)
  ),
  Sentences = c(
    nrow(corpus_stats),
    round(mean(corpus_stats$n_sentences), 1),
    round(median(corpus_stats$n_sentences), 1),
    round(sd(corpus_stats$n_sentences), 1),
    min(corpus_stats$n_sentences),
    max(corpus_stats$n_sentences),
    sum(corpus_stats$n_sentences)
  )
)

# Create flextable
ft <- flextable(summary_table) %>%
  set_caption("Table: Corpus Summary Statistics - Agritourism Statutes (N=43)",
              style = "Table Caption") %>%
  theme_booktabs() %>%
  fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>%
  bold(j = 1) %>%
  align(align = "left", j = 1, part = "body") %>%
  align(align = "center", j = 2:4, part = "all") %>%
  width(j = 1, width = 1.5) %>%
  width(j = 2:4, width = 1.2) %>%
  add_footer_lines(
    values = c(
      "Notes: Statistics calculated from text of state agritourism liability statutes.",
      "Words = whitespace-separated tokens; Tokens = alphanumeric sequences.",
      "Sentences estimated using terminal punctuation (., !, ?).",
      "N=43 states included (Connecticut excluded from analysis)."
    )
  ) %>%
  fontsize(size = 9, part = "footer") %>%
  italic(part = "footer")

# Display table
print(ft)

# ==============================================================================
# 5. SAVE OUTPUTS
# ==============================================================================

cat("\n=== SAVING OUTPUTS ===\n\n")

# Save detailed statistics
write_csv(corpus_stats, "../4_Analysis_Results/corpus_statistics_by_state.csv")
cat("Saved: corpus_statistics_by_state.csv\n")

# Save summary table to Word
doc <- read_docx()

doc <- doc %>%
  body_add_par("Corpus Summary Statistics", style = "heading 1") %>%
  body_add_par("") %>%
  body_add_flextable(ft) %>%
  body_add_par("") %>%
  body_add_par("Interpretation", style = "heading 2") %>%
  body_add_par(sprintf(
    "The corpus consists of %d state agritourism liability statutes enacted between 2003 and 2021. Documents average %.0f words (SD=%.0f, range=%d–%d), with a median of %.0f words. The total corpus contains %s words across all documents. Document length variation reflects differences in statutory scope and detail, with some states enacting brief liability shields while others include comprehensive regulatory frameworks.",
    nrow(corpus_stats),
    mean(corpus_stats$n_words),
    sd(corpus_stats$n_words),
    min(corpus_stats$n_words),
    max(corpus_stats$n_words),
    median(corpus_stats$n_words),
    format(sum(corpus_stats$n_words), big.mark = ",")
  ))

output_file <- "../3_Output/Documents/Corpus_Summary_Statistics.docx"
print(doc, target = output_file)
cat("Saved:", output_file, "\n\n")

# ==============================================================================
# 6. FINAL SUMMARY
# ==============================================================================

cat(rep("=", 80), "\n", sep = "")
cat("CORPUS SUMMARY COMPLETE\n")
cat(rep("=", 80), "\n", sep = "")
cat("\n")

cat("KEY STATISTICS:\n")
cat(sprintf("  Documents: %d statutes\n", nrow(corpus_stats)))
cat(sprintf("  Mean length: %.0f words (SD=%.0f)\n", 
            mean(corpus_stats$n_words), sd(corpus_stats$n_words)))
cat(sprintf("  Median length: %.0f words\n", median(corpus_stats$n_words)))
cat(sprintf("  Range: %d - %d words\n", 
            min(corpus_stats$n_words), max(corpus_stats$n_words)))
cat(sprintf("  Total corpus: %s words\n", 
            format(sum(corpus_stats$n_words), big.mark = ",")))
cat("\n")

cat("FILES CREATED:\n")
cat("  - corpus_statistics_by_state.csv (detailed stats for each state, N=43)\n")
cat("  - Corpus_Summary_Statistics.docx (summary table)\n\n")

cat(rep("=", 80), "\n", sep = "")
cat("COMPLETE!\n")
cat(rep("=", 80), "\n", sep = "")
