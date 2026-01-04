# ==============================================================================
# Title:        The Diffusion of Regulatory Shields (Time-of-Adoption Analysis)
# Author:       [Your Name]
# Journal:      State Politics & Policy Quarterly (SPPQ)
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
library(spdep)
library(spatialreg)
library(here) 

set.seed(2024) 

# ------------------------------------------------------------------------------
# 1. Data Ingestion
# ------------------------------------------------------------------------------
# A. Load Data
# We use read_csv, which creates a Tibble.
analysis_data <- read_csv(here("time_of_adoption_agritourism_2004_2018.csv"))

# B. Load Text
files <- list.files(path = here("State Agritourism Laws"), pattern = "*.txt", full.names = TRUE)
texts <- readtext(files)
texts$state <- tolower(tools::file_path_sans_ext(basename(texts$doc_id)))

# C. Merge Text with Data
texts_final <- texts %>%
  inner_join(analysis_data, by = "state")

# Force Document ID
texts_final$doc_id <- texts_final$state

print(paste("Final Analysis Set:", nrow(texts_final), "States"))

# ------------------------------------------------------------------------------
# 2. Text Pre-processing (Strict Cleaning)
# ------------------------------------------------------------------------------
corp <- corpus(texts_final, text_field = "text", docid_field = "doc_id")

# Create DFM with strict pruning
dfm  <- dfm(tokens(corp, remove_punct = TRUE, remove_numbers = TRUE)) %>%
  dfm_remove(pattern = stopwords("en")) %>%
  dfm_trim(min_termfreq = 3, min_docfreq = 2) %>%
  # CRITICAL: Remove empty documents created by the trimming above
  dfm_subset(ntoken(.) > 0) 

# ------------------------------------------------------------------------------
# 3. Structural Topic Modeling (STM)
# ------------------------------------------------------------------------------
# Convert to STM format
stm_input <- convert(dfm, to = "stm")

# *** CRITICAL FIX: The "De-Tibble" Step ***
# STM crashes if metadata is a "tbl_df" (Tibble). We force it to be a standard data.frame.
stm_input$meta <- as.data.frame(stm_input$meta)

# Run Model
# We skip 'prepDocuments' because 'quanteda::convert' already did the work.
# We use "Spectral" init because it's deterministic.
model_lda <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  data = stm_input$meta, 
  K = 4, 
  init.type = "Spectral",
  seed = 2024,
  verbose = FALSE
)

# Label Topics
theta <- as.data.frame(model_lda$theta)
colnames(theta) <- c("Topic1", "Topic2", "Topic3", "Topic4_Liability")

# Combine with Metadata for Regression
regression_data <- cbind(stm_input$meta, theta)

# ------------------------------------------------------------------------------
# 4. Statistical Analysis (Beta Regression)
# ------------------------------------------------------------------------------
# Create Variables
regression_data <- regression_data %>%
  mutate(
    log_ag_lobby = log(contrib_agri + 1),
    log_gdp_share = log(valueofagsect + 1),
    is_republican = factor(ifelse(govparty_c == 1 | govparty_c == "Republican", 1, 0)),
    has_planning = factor(pldvpag)
  )

# Formula
f_final <- Topic4_Liability ~ log_ag_lobby + log_gdp_share + is_republican + has_planning

# Run Regression
fit <- betareg(f_final, data = regression_data)

# Print Table
modelsummary(fit, stars = TRUE, 
             title = "Determinants of Statutory Liability Framing (Time-of-Adoption)",
             coef_rename = c("log_ag_lobby" = "Ag. Lobbying (Log)",
                             "log_gdp_share" = "Ag. Economic Value (Log)",
                             "is_republican1" = "Republican Governor",
                             "has_planning1" = "State Planning Agency"))

# ------------------------------------------------------------------------------
# 5. Visualizations
# ------------------------------------------------------------------------------

# A. Marginal Effects Plot
plot_predictions(fit, condition = "log_ag_lobby") +
  theme_minimal() +
  labs(
    title = "Impact of Lobbying on Liability Protections",
    x = "Agricultural Campaign Contributions (Log)",
    y = "Proportion of Statute Devoted to Liability"
  ) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line(color = "firebrick", size = 1)

# B. Isomorphism Network Graph
sim_matrix <- textstat_simil(dfm, method = "cosine", margin = "documents")
sim_df <- as.data.frame(as.table(as.matrix(sim_matrix))) %>%
  filter(Var1 != Var2) %>%
  rename(State1 = Var1, State2 = Var2, Similarity = Freq) %>%
  filter(Similarity > 0.90) 

graph_sim <- graph_from_data_frame(sim_df, directed = FALSE)

ggraph(graph_sim, layout = 'fr') +
  geom_edge_link(aes(width = Similarity), color = "grey70") +
  geom_node_point(size = 5, color = "#2E86C1") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  theme_void() +
  ggtitle("Statutory Isomorphism: Boilerplate Diffusion")