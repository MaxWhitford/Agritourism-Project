# ==============================================================================
# Title:        Agritourism Laws Analysis - CLEANED VERSION
# Author:       Max Whitford
# Description:  Sentiment analysis, STM topic modeling, and spatial diffusion
# ==============================================================================

# Clear environment
rm(list = ls())

# ------------------------------------------------------------------------------
# LOAD PACKAGES
# ------------------------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(textdata)
library(widyr)
library(ggraph)
library(igraph)
library(maps)
library(car)
library(modelsummary)
library(here)
library(knitr)
library(stm)
library(tm)
library(readtext)
library(quanteda)
library(betareg)
library(sf)
library(spdep)
library(spData)
library(spatialreg)

set.seed(2122)

# ==============================================================================
# PART 1: SENTIMENT ANALYSIS (from your original paper methodology)
# ==============================================================================

# Load the merged-cleaned data for sentiment analysis
# NOTE: This file uses COMMAS, so use read_csv (not read_csv2 which expects semicolons)
state_data <- read_csv(here('merged-cleaned.csv'), col_names = TRUE)

# Verify column names
cat("Column names in state_data:\n")
print(colnames(state_data))

# Tokenize
tidy_states <- state_data %>%
  unnest_tokens(word, placeHolder)

# Get NRC joy sentiments
nrcjoy <- get_sentiments('nrc') %>%
  filter(sentiment == 'joy')

# Example: Alabama joy words
tidy_states %>%
  filter(stateLabels == 'Alabama') %>%
  inner_join(nrcjoy, by = "word") %>%
  count(word, sort = TRUE)

# Calculate sentiment scores using Bing lexicon
stateSentiment <- tidy_states %>%
  inner_join(get_sentiments('bing'), by = "word") %>%
  count(stateLabels, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment = positive - negative)

# Plot sentiment by state
ggplot(stateSentiment, aes(x = reorder(stateLabels, sentiment), y = sentiment)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Net Sentiment Score by State",
       x = "State", y = "Sentiment (Positive - Negative)") +
  theme_minimal()

# Pairwise correlations between states based on word usage
words_by_states <- tidy_states %>%
  count(stateLabels, word, sort = TRUE)

stateCors <- words_by_states %>%
  pairwise_cor(stateLabels, word, n, sort = TRUE)

# Remove duplicate pairs (A-B and B-A are the same)
stateCors_unique <- stateCors %>%
  filter(item1 < item2)  # This keeps only one of each pair

# Plot correlation network (states with >0.92 correlation)
stateCors_unique %>%
  filter(correlation > 0.92) %>%
  graph_from_data_frame(directed = FALSE) %>%
  ggraph(layout = 'fr') +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 3, color = 'lightblue') +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  ggtitle('State Law Similarity (>.92 Correlation)') +
  theme_void()

# Histogram of correlations
hist(stateCors_unique$correlation, 
     main = "Distribution of Pairwise Correlations",
     xlab = "Correlation", col = "lightblue")

# ==============================================================================
# PART 2: STRUCTURAL TOPIC MODEL (STM)
# ==============================================================================

# FIX: Use here() for portable path
files <- list.files(path = here("State Agritourism Laws"), 
                    pattern = "*.txt", full.names = TRUE)

texts <- readtext(files)
texts$state <- tools::file_path_sans_ext(basename(texts$doc_id))

# Check what states we have
cat("States in text files:\n")
print(sort(texts$state))

# Create corpus and DFM
corp <- corpus(texts, text_field = "text")
dfm <- dfm(tokens(corp))
dfm <- dfm_remove(dfm, pattern = stopwords("en"))
dfm <- dfm_remove(dfm, pattern = "[[:punct:]]", valuetype = "regex")
dfm <- dfm_trim(dfm, min_termfreq = 5, min_docfreq = 2)

# Convert for STM
stm_input <- convert(dfm, to = "stm")

# Get document names from STM (these come from text file names)
docnames_stm <- names(stm_input$documents)
cat("\nDocument names in STM:\n")
print(docnames_stm)

# ------------------------------------------------------------------------------
# Load and align metadata
# ------------------------------------------------------------------------------

# Load both metadata files
metadata_main <- read.csv(here("stm_metadata_corrected_all_vars.csv")) %>%
  mutate(state = trimws(state))  # Keep original case since text files are capitalized

old_covariates <- read.csv(here('agritourism_covariates_michigan.csv')) %>%
  mutate(state = trimws(state))

# Merge metadata
combined_metadata <- inner_join(metadata_main, old_covariates, by = "state")

cat("\n=== METADATA DEBUG ===")
cat("\nRows in metadata_main:", nrow(metadata_main))
cat("\nRows in old_covariates:", nrow(old_covariates))
cat("\nRows after merge:", nrow(combined_metadata))
cat("\nStates in combined_metadata:\n")
print(combined_metadata$state)

# Get document names from STM
# These typically come from the text field or doc_id
docnames_stm <- names(stm_input$documents)
cat("\nDocument names from STM (first 10):\n")
print(head(docnames_stm, 10))

# CRITICAL: Check if docnames have .txt extension
cat("\nDo docnames contain '.txt'?", any(grepl("\\.txt", docnames_stm)), "\n")

# Clean docnames - remove .txt if present
docnames_clean <- gsub("\\.txt$", "", docnames_stm)
cat("\nCleaned docnames (first 10):\n")
print(head(docnames_clean, 10))

# Check overlap between metadata states and document names
overlap <- intersect(combined_metadata$state, docnames_clean)
cat("\nStates that match between metadata and STM:", length(overlap), "\n")
if (length(overlap) < 5) {
  cat("WARNING: Very few matches!\n")
  cat("Metadata states sample:", paste(head(combined_metadata$state), collapse = ", "), "\n")
  cat("STM docnames sample:", paste(head(docnames_clean), collapse = ", "), "\n")
}

# Filter and align metadata to match STM document order
combined_metadata <- combined_metadata %>%
  filter(state %in% docnames_clean) %>%
  arrange(match(state, docnames_clean))

cat("\nRows in combined_metadata after filtering:", nrow(combined_metadata), "\n")

# Verify alignment
cat("\nStates in metadata after alignment:\n")
print(combined_metadata$state)
cat("\nNumber of docs in STM:", length(stm_input$documents), "\n")
cat("Number of rows in metadata:", nrow(combined_metadata), "\n")

# Check if they match
if (length(stm_input$documents) != nrow(combined_metadata)) {
  warning("MISMATCH: STM documents and metadata have different lengths!")
  
  # Find which states are missing
  missing_from_metadata <- setdiff(docnames_stm, combined_metadata$state)
  missing_from_stm <- setdiff(combined_metadata$state, docnames_stm)
  
  cat("States in STM but not in metadata:", paste(missing_from_metadata, collapse = ", "), "\n")
  cat("States in metadata but not in STM:", paste(missing_from_stm, collapse = ", "), "\n")
}

# ------------------------------------------------------------------------------
# Run STM without covariates first (more stable)
# ------------------------------------------------------------------------------

model_no_cov <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  K = 4,
  max.em.its = 75,
  init.type = "Spectral",
  verbose = TRUE
)

# View topics
plot(model_no_cov, type = "summary", main = "Topic Prevalence")
labelTopics(model_no_cov, n = 10)

# Extract theta (topic proportions)
theta <- model_no_cov$theta
rownames(theta) <- docnames_stm

# ==============================================================================
# PART 3: BETA REGRESSION ON TOPIC PROPORTIONS
# ==============================================================================

# Prepare data for regression
# FIX: Ensure theta rows match metadata rows using cleaned docnames
regression_data <- combined_metadata %>%
  mutate(
    theta1 = theta[match(state, docnames_clean), 1],
    theta2 = theta[match(state, docnames_clean), 2],
    theta3 = theta[match(state, docnames_clean), 3],
    theta4 = theta[match(state, docnames_clean), 4]
  )

cat("\n=== REGRESSION DATA DEBUG ===")
cat("\nRows in regression_data:", nrow(regression_data))
cat("\nTheta values (first 5 rows):\n")
print(head(regression_data[, c("state", "theta1", "theta2", "theta3", "theta4")]))

# Check for NAs
cat("\nMissing values in regression data:\n")
print(colSums(is.na(regression_data)))

# Convert factors
regression_data <- regression_data %>%
  mutate(
    sals = factor(sals),
    agtaxcredit = factor(agtaxcredit),
    interstate_compact_on_agricultur = factor(interstate_compact_on_agricultur),
    econdev = factor(econdev),
    pldvpag = factor(pldvpag),
    drawmilk = factor(drawmilk),
    gov_party = factor(gov_party)
  )

# FIX: Beta regression requires Y in (0,1) EXCLUSIVE
# Squeeze values away from 0 and 1
squeeze <- function(x, eps = 0.001) {
  pmin(pmax(x, eps), 1 - eps)
}

regression_data <- regression_data %>%
  mutate(
    theta1_sq = squeeze(theta1),
    theta2_sq = squeeze(theta2),
    theta3_sq = squeeze(theta3),
    theta4_sq = squeeze(theta4)
  )

# ==============================================================================
# DIAGNOSTIC: Check factor levels before regression
# ==============================================================================
cat("\n=== FACTOR LEVEL DIAGNOSTICS ===")
cat("\n\ngov_party levels:", nlevels(regression_data$gov_party), "\n")
print(table(regression_data$gov_party, useNA = "always"))

cat("\necondev levels:", nlevels(regression_data$econdev), "\n")
print(table(regression_data$econdev, useNA = "always"))

cat("\nagtaxcredit levels:", nlevels(regression_data$agtaxcredit), "\n")
print(table(regression_data$agtaxcredit, useNA = "always"))

cat("\npldvpag levels:", nlevels(regression_data$pldvpag), "\n")
print(table(regression_data$pldvpag, useNA = "always"))

cat("\nsals levels:", nlevels(regression_data$sals), "\n")
print(table(regression_data$sals, useNA = "always"))

# Check for single-level factors and remove them from formula
factors_to_use <- c()
if (nlevels(regression_data$gov_party) >= 2) factors_to_use <- c(factors_to_use, "gov_party")
if (nlevels(regression_data$econdev) >= 2) factors_to_use <- c(factors_to_use, "econdev")
if (nlevels(regression_data$agtaxcredit) >= 2) factors_to_use <- c(factors_to_use, "agtaxcredit")
if (nlevels(regression_data$pldvpag) >= 2) factors_to_use <- c(factors_to_use, "pldvpag")

cat("\nFactors with 2+ levels (usable in regression):", paste(factors_to_use, collapse = ", "), "\n")

# FIX: Use I() for polynomial terms in formulas
# rural_pop^2 doesn't work - need I(rural_pop^2)

# Build formula dynamically based on which factors have 2+ levels
base_formula <- "~ ag_gdp + rural_pop + I(rural_pop^2) + I(ag_gdp^2) + contrib_agri"
if (length(factors_to_use) > 0) {
  factor_terms <- paste(factors_to_use, collapse = " + ")
  full_formula <- paste(base_formula, "+", factor_terms)
} else {
  full_formula <- base_formula
}
cat("\nUsing formula:", full_formula, "\n\n")

tryCatch({
  lm1 <- betareg(as.formula(paste("theta1_sq", full_formula)), data = regression_data)
  lm2 <- betareg(as.formula(paste("theta2_sq", full_formula)), data = regression_data)
  lm3 <- betareg(as.formula(paste("theta3_sq", full_formula)), data = regression_data)
  lm4 <- betareg(as.formula(paste("theta4_sq", full_formula)), data = regression_data)
  
  # Display results
  cat("\n=== BETA REGRESSION RESULTS ===\n")
  cat("\nTopic 1 (Liability & Injury):\n")
  print(summary(lm1))
  cat("\nTopic 2 (Regulatory/Admin):\n")
  print(summary(lm2))
  cat("\nTopic 3 (Tourism & Recreation):\n")
  print(summary(lm3))
  cat("\nTopic 4 (Land Use & Zoning):\n")
  print(summary(lm4))
  
}, error = function(e) {
  cat("Beta regression failed:", e$message, "\n")
  cat("This often happens with small samples or near-boundary values.\n")
})

# ==============================================================================
# PART 4: SPATIAL ANALYSIS (HORIZONTAL DIFFUSION)
# ==============================================================================

# Load US states spatial data
data("us_states")

# Debug: Check what's in each dataset
cat("\n=== SPATIAL DATA ALIGNMENT DEBUG ===")
cat("\nStates in regression_data:\n")
print(regression_data$state)

cat("\nStates in us_states:\n")
print(us_states$NAME)

# FIX: Filter states properly by name
# The us_states NAME column has values like "Alabama", "Alaska", etc.
states_in_analysis <- regression_data$state

# Match with spatial data
subset_sf <- us_states %>%
  filter(NAME %in% states_in_analysis)

cat("\nNumber of states matched:", nrow(subset_sf), "\n")

# If no matches, try to diagnose
if (nrow(subset_sf) == 0) {
  cat("\nNo matches found! Checking for case/whitespace issues...\n")
  cat("Sample from regression_data$state:", head(regression_data$state), "\n")
  cat("Sample from us_states$NAME:", head(us_states$NAME), "\n")
  
  # Try matching with trimmed, title-case names
  states_in_analysis_clean <- tools::toTitleCase(trimws(regression_data$state))
  subset_sf <- us_states %>%
    filter(NAME %in% states_in_analysis_clean)
  cat("After cleaning, matched:", nrow(subset_sf), "states\n")
}

# Check which states matched
cat("\nStates matched with spatial data:\n")
print(subset_sf$NAME)

# Guard: Only proceed if we have matches
if (nrow(subset_sf) < 3) {
  warning("Not enough states matched for spatial analysis. Need at least 3.")
  cat("\nSkipping spatial analysis section.\n")
} else {

# FIX: Align regression_data with spatial data order
geo_metadata <- regression_data %>%
  filter(state %in% subset_sf$NAME) %>%
  arrange(match(state, subset_sf$NAME))

# Extract corresponding theta values
theta_spatial <- geo_metadata %>%
  select(theta1, theta2, theta3, theta4) %>%
  as.matrix()

# Create spatial weights
nb <- poly2nb(subset_sf, queen = TRUE)
W_list <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Calculate spatial lags
geo_metadata$theta1_lag <- lag.listw(W_list, theta_spatial[, 1], zero.policy = TRUE)
geo_metadata$theta2_lag <- lag.listw(W_list, theta_spatial[, 2], zero.policy = TRUE)
geo_metadata$theta3_lag <- lag.listw(W_list, theta_spatial[, 3], zero.policy = TRUE)
geo_metadata$theta4_lag <- lag.listw(W_list, theta_spatial[, 4], zero.policy = TRUE)

# Check for multicollinearity
cat("\n=== VIF CHECK ===\n")
vif_model <- lm(theta1 ~ ag_gdp + econdev + pldvpag + contrib_agri + rural_pop + gov_party, 
                data = geo_metadata)
print(vif(vif_model))

# Run spatial lag models
tryCatch({
  lag_model1 <- lagsarlm(theta1 ~ ag_gdp + econdev + pldvpag + agtaxcredit + 
                           interstate_compact_on_agricultur + gov_party,
                         data = geo_metadata, listw = W_list, 
                         method = "eigen", zero.policy = TRUE)
  
  cat("\n=== SPATIAL LAG MODEL - Topic 1 ===\n")
  print(summary(lag_model1))
  
  # The rho coefficient indicates spatial autocorrelation
  # Significant rho suggests horizontal policy diffusion
  
}, error = function(e) {
  cat("Spatial model failed:", e$message, "\n")
})

} # End of spatial analysis if-block

# ==============================================================================
# PART 5: CHOROPLETH MAP
# ==============================================================================

# Load US map FIRST
state_map <- map_data("state")

# Calculate dominant topic for each state
dominant_topics <- apply(theta, 1, which.max)

# FIX: Remove .txt from state names before creating choropleth data
state_names_clean <- gsub("\\.txt$", "", rownames(theta))

choropleth_data <- data.frame(
  state = state_names_clean,
  dominant_topic = dominant_topics
) %>%
  mutate(state_lower = tolower(state))

# Debug: Check the join
cat("\n=== CHOROPLETH DEBUG ===")
cat("\nSample state_lower values:", head(choropleth_data$state_lower), "\n")
cat("Sample map regions:", head(unique(state_map$region)), "\n")

# Join
plot_data <- left_join(state_map, choropleth_data, by = c("region" = "state_lower"))

# Create topic labels
# CORRECT LABELS based on labelTopics() output:
# Topic 1: agritourism, professional, participant, injury, liability, warning = Liability & Injury
# Topic 2: agricultural, farm, operation, management, advisory, districts = Regulatory/Administrative  
# Topic 3: activity, tourism, recreational, sport, recreation = Tourism & Recreation
# Topic 4: land, county, building, zoning, board, division = Land Use & Zoning
plot_data$topic_label <- factor(plot_data$dominant_topic,
                                levels = 1:4,
                                labels = c("Liability & Injury",
                                           "Regulatory/Admin",
                                           "Tourism & Recreation",
                                           "Land Use & Zoning"))

# Plot
ggplot(plot_data, aes(x = long, y = lat, group = group, fill = topic_label)) +
  geom_polygon(color = "white", size = 0.2) +
  coord_fixed(1.3) +
  theme_void() +
  scale_fill_manual(
    values = c(
      "Liability & Injury" = "#e78ac3",
      "Regulatory/Admin" = "#66c2a5",
      "Tourism & Recreation" = "#fc8d62",
      "Land Use & Zoning" = "#8da0cb"
    ),
    na.value = "grey80",
    name = "Dominant Topic"
  ) +
  labs(title = "Dominant Agritourism Statutory Topic by State")

# ==============================================================================
# SUMMARY STATISTICS
# ==============================================================================

cat("\n=== FINAL SUMMARY ===\n")
cat("Total states analyzed:", nrow(regression_data), "\n")

topic_counts <- table(dominant_topics)
cat("\nDominant topic distribution:\n")
print(topic_counts)

cat("\nTop 10 most similar state pairs:\n")
print(head(stateCors_unique, 10))

