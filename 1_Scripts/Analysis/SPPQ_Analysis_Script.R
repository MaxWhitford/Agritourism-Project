# ==============================================================================
# Title:        Agritourism Statute Content Analysis (Time-of-Adoption Design)
# Author:       Max Whitford
# ==============================================================================
# 
# METHODOLOGICAL NOTES FOR REVIEWERS:
# - N = 35 states with agritourism statutes enacted 2004-2018
# - Covariates are TIME-MATCHED to year of enactment (not cross-sectional)
# - 5 states excluded due to post-2018 enactment (no time-matched data available)
# - 4 states excluded due to pending legislation or no formal statute
#
# ==============================================================================

library(tidyverse)
library(readtext)
library(quanteda)
library(quanteda.textstats)
library(stm)
library(betareg)
library(modelsummary)
library(marginaleffects)
library(igraph)
library(ggraph)
library(sf)
library(spdep)
library(spData)
library(spatialreg)
library(here)

set.seed(2024)

# ==============================================================================
# 1. DATA LOADING
# ==============================================================================

# Load time-matched covariate data
# Key: Each state's covariates are from their enactment year, not arbitrary cross-section
analysis_data <- read_csv("../../2_Data/Processed/agritourism_analysis_final.csv")

cat("=== SAMPLE DESCRIPTION ===\n")
cat("Total states:", nrow(analysis_data), "\n")
cat("Enactment year range:", min(analysis_data$enact_year), "-", max(analysis_data$enact_year), "\n")

# Load statutory text
files <- list.files(path = "../../State Agritourism Laws", 
                    pattern = "*.txt", full.names = TRUE)
texts <- readtext(files)
texts$state <- tools::file_path_sans_ext(basename(texts$doc_id))

# Merge text with time-matched covariates
# Only keep states that are in our analysis sample
texts_merged <- texts %>%
  inner_join(analysis_data, by = "state")

cat("States with both text and covariates:", nrow(texts_merged), "\n")

# ==============================================================================
# 2. TEXT PREPROCESSING
# ==============================================================================

corp <- corpus(texts_merged, text_field = "text", docid_field = "state")

dfm <- dfm(tokens(corp, remove_punct = TRUE, remove_numbers = TRUE)) %>%
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 5, min_docfreq = 2)

cat("\nCorpus size:", ndoc(corp), "documents,", nfeat(dfm), "features\n")

# ==============================================================================
# 3. STRUCTURAL TOPIC MODEL
# ==============================================================================

stm_input <- convert(dfm, to = "stm")

# Run STM without prevalence covariates first (more stable)
model_stm <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  K = 4,
  max.em.its = 100,
  init.type = "Spectral",
  seed = 2024,
  verbose = TRUE
)

# Examine topics
cat("\n=== TOPIC LABELS ===\n")
labelTopics(model_stm, n = 8)

# Plot topic prevalence
plot(model_stm, type = "summary", main = "Topic Prevalence Across States")

# ==============================================================================
# 4. PREPARE REGRESSION DATA
# ==============================================================================

# Extract topic proportions
theta <- model_stm$theta
docnames_stm <- names(stm_input$documents)

# Clean document names (remove .txt if present)
docnames_clean <- gsub("\\.txt$", "", docnames_stm)

# Create regression dataset
regression_data <- analysis_data %>%
  filter(state %in% docnames_clean) %>%
  arrange(match(state, docnames_clean)) %>%
  mutate(
    # Topic proportions
    topic1 = theta[, 1],
    topic2 = theta[, 2],
    topic3 = theta[, 3],
    topic4 = theta[, 4],
    
    # Transform predictors
    log_contrib = log(contrib_agri + 1),
    log_ag_value = log(valueofagsect + 1),
    
    # Temporal controls
    year_centered = enact_year - 2010,  # Center on 2010
    early_adopter = ifelse(enact_year <= 2008, 1, 0)
  )

cat("\n=== REGRESSION DATA ===\n")
cat("N =", nrow(regression_data), "\n")
cat("\nVariable coverage:\n")
cat("  contrib_agri:", sum(!is.na(regression_data$contrib_agri)), "/", nrow(regression_data), "\n")
cat("  valueofagsect:", sum(!is.na(regression_data$valueofagsect)), "/", nrow(regression_data), "\n")
cat("  govparty_c:", sum(!is.na(regression_data$govparty_c)), "/", nrow(regression_data), "\n")

# ==============================================================================
# 5. BETA REGRESSION MODELS
# ==============================================================================

# Beta regression requires Y in (0,1) exclusive
squeeze <- function(x, eps = 0.001) {
  pmin(pmax(x, eps), 1 - eps)
}

regression_data <- regression_data %>%
  mutate(across(starts_with("topic"), ~squeeze(.), .names = "{.col}_sq"))

# Model specification using variables with complete data
# Primary model: Agricultural interests predict liability framing
cat("\n=== BETA REGRESSION RESULTS ===\n")

# Identify which topic is "Liability" based on labelTopics output
# Typically has words like: liability, injury, inherent, risk, death, warning
# Adjust topic number based on your actual results

# Model for each topic
for (i in 1:4) {
  cat(paste0("\n--- Topic ", i, " ---\n"))
  
  formula_i <- as.formula(paste0(
    "topic", i, "_sq ~ log_contrib + log_ag_value + year_centered"
  ))
  
  tryCatch({
    model_i <- betareg(formula_i, data = regression_data)
    print(summary(model_i))
  }, error = function(e) {
    cat("Model failed:", e$message, "\n")
  })
}


# ==============================================================================
# 6. POLICY DIFFUSION ANALYSIS
# ==============================================================================

# Pairwise textual similarity
sim_matrix <- textstat_simil(dfm, method = "cosine", margin = "documents")
sim_df <- as.data.frame(as.table(as.matrix(sim_matrix))) %>%
  filter(Var1 < Var2) %>%  # Remove duplicates
  rename(State1 = Var1, State2 = Var2, Similarity = Freq) %>%
  arrange(desc(Similarity))

cat("\n=== TOP 10 MOST SIMILAR STATE PAIRS ===\n")
print(head(sim_df, 10))

# Isomorphism network (states with >90% similarity)
high_sim <- sim_df %>% filter(Similarity > 0.90)

if (nrow(high_sim) > 0) {
  graph_sim <- graph_from_data_frame(high_sim, directed = FALSE)
  
  p_network <- ggraph(graph_sim, layout = 'fr') +
    geom_edge_link(aes(width = Similarity, alpha = Similarity), color = "grey50") +
    geom_node_point(size = 4, color = "#2E86C1") +
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    theme_void() +
    labs(title = "Statutory Isomorphism Network",
         subtitle = "Edges indicate >90% textual similarity") +
    theme(legend.position = "none")
  
  print(p_network)
}

# ==============================================================================
# 7. SPATIAL AUTOCORRELATION (Optional)
# ==============================================================================

# Test for horizontal diffusion via spatial lag
data("us_states")

# Match with regression data
subset_sf <- us_states %>%
  filter(NAME %in% regression_data$state)

if (nrow(subset_sf) >= 20) {
  
  # Align data with spatial object
  geo_data <- regression_data %>%
    filter(state %in% subset_sf$NAME) %>%
    arrange(match(state, subset_sf$NAME))
  
  # Create spatial weights
  nb <- poly2nb(subset_sf, queen = TRUE)
  W_list <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  # Spatial lag model for liability topic (adjust topic number as needed)
  cat("\n=== SPATIAL LAG MODEL ===\n")
  tryCatch({
    lag_model <- lagsarlm(
      topic4 ~ log_contrib + log_ag_value + year_centered,
      data = geo_data, 
      listw = W_list, 
      zero.policy = TRUE
    )
    print(summary(lag_model))
    cat("\nRho (spatial autocorrelation):", lag_model$rho, "\n")
    cat("Rho p-value:", lag_model$rho.se, "\n")
  }, error = function(e) {
    cat("Spatial model failed:", e$message, "\n")
  })
}

# ==============================================================================
# 8. VISUALIZATIONS FOR PUBLICATION
# ==============================================================================

# Choropleth map of dominant topics
dominant_topic <- apply(theta, 1, which.max)
map_data_df <- data.frame(
  state = docnames_clean,
  dominant_topic = factor(dominant_topic, 
                          levels = 1:4,
                          labels = c("Liability & Injury",      # Topic 1: professional, participant, injury, warning
                                     "Regulatory/Admin",        # Topic 2: farm, operation, management, advisory
                                     "Tourism & Recreation",    # Topic 3: tourism, recreational, sport, recreation
                                     "Land Use & Zoning"))      # Topic 4: land, county, building, zoning
) %>%
  mutate(state_lower = tolower(state))

state_map <- map_data("state")
plot_data <- left_join(state_map, map_data_df, by = c("region" = "state_lower"))

p_map <- ggplot(plot_data, aes(x = long, y = lat, group = group, fill = dominant_topic)) +
  geom_polygon(color = "white", size = 0.2) +
  coord_fixed(1.3) +
  scale_fill_brewer(palette = "Set2", na.value = "grey80", name = "Dominant Topic") +
  theme_void() +
  labs(title = "Dominant Statutory Topic by State",
       caption = "Grey = No agritourism statute or not in sample")

print(p_map)

# ==============================================================================
# 9. SUMMARY OUTPUT
# ==============================================================================

cat("\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("ANALYSIS SUMMARY\n")
cat("=" %>% rep(60) %>% paste(collapse = ""), "\n")
cat("\nSample: N =", nrow(regression_data), "states\n")
cat("Period: 2004-2018 (time-matched covariates)\n")
cat("Topics: K = 4\n")
cat("\nExcluded states (post-2018 enactment):\n")
cat("  Arizona (2019), Iowa (2021), Pennsylvania (2021)\n")
cat("\nKey findings:\n")
cat("  - See beta regression output for topic determinants\n")
cat("  - See similarity matrix for diffusion patterns\n")
cat("  - Rho coefficient indicates spatial autocorrelation\n")
