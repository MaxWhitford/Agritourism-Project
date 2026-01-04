
library(knitr)
library(ggraph)
library(igraph)
library(widyr)
library(tidyverse)
library(maps)
library(car)
library(modelsummary)
library(here)
library(stringr)
library(tidytext)
library(textdata)
dev.new()


state_data <- read_csv2('merged-cleaned.csv',col_names = T,)



tidy_states <- state_data %>%
  unnest_tokens(word,placeHolder); tidy_states

nrcjoy <- get_sentiments('nrc') %>%
  filter(sentiment == 'joy')

tidy_states %>%
  filter(stateLabels == 'Alabama') %>%
  inner_join(nrcjoy) %>%
  count(word,sort=T)


stateSentiment <- tidy_states %>%
  inner_join(get_sentiments('bing')) %>%
  count(stateLabels, index = stateLabels, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive - negative)

ggplot(stateSentiment, aes(sentiment,index))+
  geom_col()


#linear model

lm1 <- lm(data=stateSentiment, formula = sentiment ~ factor(index))
modelsummary(lm1)

stateSentiment2 <- stateSentiment %>%
  mutate(index = index) 

 #write.csv(stateSentiment)

words_by_states <- tidy_states %>%
  count(stateLabels,word,sort=T)


stateCors <- words_by_states %>%
  pairwise_cor(stateLabels,word,n, sort=T)

stateCors %>%
  filter(item1 == 'North Carolina')


toDelete <- seq(0,nrow(stateCors),2)
#CORS GRAPH


stateCors2 <- stateCors[toDelete,]
stateCors2


set.seed(2122)
stateCors2 %>%
  filter(correlation>.92) %>%
  graph_from_data_frame() %>%
  ggraph(layout='fr') +
  geom_edge_link(aes(alpha=correlation,width=correlation)) +
  geom_node_point(size=1,color='lightblue')+
  geom_node_text(aes(label=name,color='red'),repel=T)+
  ggtitle('Overlap Among States (>.92 Correlation)')+
  theme_void()

head(stateCors2)

 Correlations <- stateCors2$correlation
hist(Correlations)


library(stm)
library(tm)
library(readtext)
library(quanteda)
library(tidyverse)

files <- list.files(path = "/Users/mbwhitford/Dropbox/classes/2024/spring_2024/POLS4707R/State Agritourism Laws", pattern = "*.txt", full.names = TRUE)

texts <- readtext(files)
texts$state <- tools::file_path_sans_ext(basename(texts$doc_id))

# Convert to quanteda corpus
corp <- corpus(texts, text_field = "text")

# Build the dfm without the deprecated 'remove' argument
dfm <- dfm(tokens(corp))

#DATA FOR agritourism_covariates
#	•	USDA ERS State Fact Sheets (updated March 2025) — agriculture value-added share of GDP and share of population in non-metro (rural) areas
#•	Ballotpedia / NCSL — Governor’s party as of 2025

# Now remove stopwords and punctuation explicitly
dfm <- dfm_remove(dfm, pattern = stopwords("en"))
dfm <- dfm_remove(dfm, pattern = "[[:punct:]]", valuetype = "regex")

# Trim infrequent terms
dfm <- dfm_trim(dfm, min_termfreq = 5, min_docfreq = 2)

metadata <- read.csv("agritourism_covariates_michigan.csv")
metadata$ag_gdp <- as.numeric(metadata$ag_gdp)
metadata$rural_pop <- as.numeric(metadata$rural_pop)
metadata$gov_party <- as.numeric(metadata$gov_party)

# 4. Convert for STM
stm_input <- convert(dfm, to = "stm")  # <- this creates `stm_input`
# Order metadata to match stm_input document names
docnames <- names(stm_input$documents)
#metadata <- metadata %>% filter(state %in% docnames)
#metadata <- metadata %>% arrange(factor(state, levels = docnames))





# 6. Fit your STM model
model <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  data = metadata,
  K = 4,
  prevalence = ~ ag_gdp + rural_pop + gov_party,
  max.em.its = 75,
  init.type = "Spectral"
)

plot(model)


prep <- estimateEffect(1:4 ~ ag_gdp + rural_pop + gov_party, model, metadata = metadata, documents = stm_input$documents)
summary(prep)

state_topics <- data.frame(state = metadata$state, model$theta)

plot.estimateEffect(
  prep,
  covariate = "ag_gdp",
  model = model,
  topics = 1:4,
  method = "continuous",
  printlegend = TRUE,
  xlab = "Agriculture Share of State GDP",
  main = "Effect of ag_gdp on Topic Prevalence",
  labeltype = "custom",
  custom.labels = c("Regulatory Language", "Tourism & Rec", "Land Use & Zoning", "Liability & Injury"),
  ylim = c(0, 1)
)


# Load topic dominance data
choropleth_data <- read_csv("choropleth_data_topic_dominance.csv")
choropleth_data$state <- tolower(choropleth_data$state)

# Load U.S. state map
state_map <- map_data("state")

# Join state map with topic data
plot_data <- left_join(state_map, choropleth_data, by = c("region" = "state"))

# Replace missing states with "No Data"
plot_data$Dominant_Topic <- ifelse(is.na(plot_data$Dominant_Topic), "No Data", plot_data$Dominant_Topic)

# Rename topic codes to descriptive names
plot_data$Dominant_Topic <- recode(plot_data$Dominant_Topic,
                                   "Topic1" = "Statutory/Regulatory Language",
                                   "Topic2" = "Tourism & Recreation Framing",
                                   "Topic3" = "Land Use & Zoning",
                                   "Topic4" = "Liability & Injury Protections",
                                   "No Data" = "No Data"
)

# Plot the choropleth with labeled legend
ggplot(plot_data, aes(x = long, y = lat, group = group, fill = Dominant_Topic)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  theme_void() +
  scale_fill_manual(
    values = c(
      "Statutory/Regulatory Language" = "#66c2a5",
      "Tourism & Recreation Framing" = "#fc8d62",
      "Land Use & Zoning" = "#8da0cb",
      "Liability & Injury Protections" = "#e78ac3",
      "No Data" = "grey80"
    ),
    name = "Dominant Legal Emphasis"
  ) +
  labs(title = "Dominant Agritourism Statutory Topic by State")


results_df <- tibble(
  Topic = c("Topic 1: Regulatory", "Topic 2: Tourism", "Topic 3: Land Use", "Topic 4: Liability"),
  
  ag_gdp_est = c(-1.657,  3.295, -8.449,  6.723),
  ag_gdp_p   = c(0.685,   0.503,  0.040,   0.250),
  
  rural_pop_est = c(-0.0057, -0.0003, 0.0057, 0.0005),
  rural_pop_p   = c(0.310,   0.961,   0.322,  0.953),
  
  gov_party_est = c(-0.135,  0.028, 0.017, 0.089),
  gov_party_p   = c(0.310,   0.851, 0.889, 0.603)
)

# Format and display the table
kable(results_df, digits = 3, caption = "Estimated Effects of Covariates on Topic Prevalence") 


############################ WITH NEW VARIABLES #####################


# STEP 1: Load and preprocess texts
files <- list.files(path = "/Users/mbwhitford/Dropbox/classes/2024/spring_2024/POLS4707R/State Agritourism Laws", 
                    pattern = "*.txt", full.names = TRUE)

texts <- readtext(files)
texts$state <- tools::file_path_sans_ext(basename(texts$doc_id))

corp <- corpus(texts, text_field = "text")

dfm <- dfm(tokens(corp))
dfm <- dfm_remove(dfm, pattern = stopwords("en"))
dfm <- dfm_remove(dfm, pattern = "[[:punct:]]", valuetype = "regex")
dfm <- dfm_trim(dfm, min_termfreq = 5, min_docfreq = 2)

# STEP 2: Load STM input
stm_input <- convert(dfm, to = "stm")

# 1. Standardize state names
metadata <- read.csv("stm_metadata_corrected_all_vars.csv") %>%
  mutate(state = trimws(tolower(state)))

old_covariates <- read.csv('agritourism_covariates_michigan.csv')
old_covariates$state <- tolower(old_covariates$state)
#2. Merge on lowercase trimmed state names
combined_metadata <- inner_join(metadata, old_covariates, by = "state")

# 3. Make sure metadata aligns with STM documents
docnames <- names(stm_input$documents)
docnames <- tolower(docnames)  # Match formatting

#combined_metadata <- combined_metadata %>%
#  filter(state %in% docnames) %>%
#  arrange(factor(state, levels = docnames))

# 4. Rerun STM
model <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  data = combined_metadata,
  K = 4,
  prevalence = ~ contrib_agri +
    agemploy+ factor(econdev) + ag_gdp + rural_pop + factor(gov_party),
  max.em.its = 75,
  init.type = "Spectral"
)

prep2 <- estimateEffect(1:4 ~ contrib_agri +
                          agemploy+ factor(econdev) + ag_gdp + rural_pop + factor(gov_party), model, metadata = combined_metadata, documents = stm_input$documents)
modelsummary(prep2)


# STEP 6: Save topic proportions by state
state_topics <- data.frame(state = combined_metadata$state, model$theta)



################### Without negative binomial #################

model_no_cov <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  K = 4,
  max.em.its = 75,
  init.type = "Spectral"
)

plot(model_no_cov)

theta <- model_no_cov$theta


# Example: regress Topic 1 prevalence manually
combined_metadata <- combined_metadata %>%
  mutate(sals = factor(sals), 
         agtaxcredit = factor(agtaxcredit), 
         interstate_compact_on_agricultur = factor(interstate_compact_on_agricultur),
         econdev = factor(econdev),
         pldvpag = factor(pldvpag),
         drawmilk = factor(drawmilk),
         gov_party = factor(gov_party)
         )

library(betareg)

lm1 <- betareg(theta[, 1] ~ ag_gdp + rural_pop + rural_pop^2 + ag_gdp^2 + gov_party + contrib_agri + econdev + agtaxcredit + pldvpag, data = combined_metadata)
lm2 <- betareg(theta[, 2] ~ ag_gdp + rural_pop + rural_pop^2 + ag_gdp^2 + gov_party + contrib_agri + econdev + agtaxcredit + pldvpag, data = combined_metadata)
lm3 <- betareg(theta[, 3] ~ ag_gdp + rural_pop + rural_pop^2 + ag_gdp^2 + gov_party + contrib_agri + econdev + agtaxcredit + pldvpag, data = combined_metadata)
lm4 <- betareg(theta[, 4] ~ ag_gdp + rural_pop + rural_pop^2 + ag_gdp^2 + gov_party + contrib_agri + econdev + agtaxcredit + pldvpag, data = combined_metadata)
modelsummary(lm1); modelsummary(lm2); modelsummary(lm3); modelsummary(lm4)

#"Topic 1: Regulatory", "Topic 2: Tourism", "Topic 3: Land Use", "Topic 4: Liability"


################ TESTING FOR HORIZONTAL DIFFUSION ####################
library(sf)
library(spdep)
library(spData)

data("us_states")  # loads an sf with all 50 states + DC
geo_metadata <- combined_metadata
geo_metadata <- geo_metadata[-2,]
geo_metadata <- geo_metadata[-10,]

us_states$NAME <- tolower(us_states$NAME)
subset_sf <- us_states %>%
  filter(NAME %in% combined_metadata$state)  # match your 44 states

theta2 <- theta
theta2 <- theta2[-2,]
theta2 <- theta2[-10,]

nb <- poly2nb(subset_sf, queen=TRUE)
W_list <- nb2listw(nb, style="W")  # row-normalized

geo_metadata$theta1_lag <- lag.listw(W_list, theta2[,1])
geo_metadata$theta2_lag <- lag.listw(W_list, theta2[,2])
geo_metadata$theta3_lag <- lag.listw(W_list, theta2[,3])
geo_metadata$theta4_lag <- lag.listw(W_list, theta2[,4])

#check for multicolinearity
vif(lm(theta2[,1] ~ ag_gdp + econdev + pldvpag + contrib_agri + rural_pop + gov_party, data = geo_metadata))

library(spatialreg)
lag_model1 <- lagsarlm(theta2[,1] ~ ag_gdp + econdev + pldvpag + agtaxcredit + interstate_compact_on_agricultur + gov_party, 
                      data = geo_metadata, listw = W_list, method="eigen")
lag_model2 <- lagsarlm(theta2[,2] ~ ag_gdp + econdev + pldvpag + agtaxcredit + interstate_compact_on_agricultur + gov_party, 
                      data = geo_metadata, listw = W_list, method="eigen")
lag_model3 <- lagsarlm(theta2[,3] ~ ag_gdp + econdev + pldvpag + agtaxcredit + interstate_compact_on_agricultur + gov_party, 
                      data = geo_metadata, listw = W_list, method="eigen")
lag_model4 <- lagsarlm(theta2[,4] ~ ag_gdp + econdev + pldvpag + agtaxcredit + interstate_compact_on_agricultur + gov_party, 
                      data = geo_metadata, listw = W_list, method="eigen")
modelsummary(lag_model1);modelsummary(lag_model2); modelsummary(lag_model3); modelsummary(lag_model4)


lm5 <- betareg(theta2[, 1] ~ ag_gdp + rural_pop + rural_pop^2 + ag_gdp^2 + gov_party + contrib_agri + econdev + agtaxcredit + pldvpag+theta1_lag, data = geo_metadata)
lm6 <- betareg(theta2[, 2] ~ ag_gdp + rural_pop + rural_pop^2 + ag_gdp^2 + gov_party + contrib_agri + econdev + agtaxcredit + pldvpag+theta2_lag, data = geo_metadata)
lm7 <- betareg(theta2[, 3] ~ ag_gdp + rural_pop + rural_pop^2 + ag_gdp^2 + gov_party + contrib_agri + econdev + agtaxcredit + pldvpag+theta3_lag, data = geo_metadata)
lm8 <- betareg(theta2[, 4] ~ ag_gdp + rural_pop + rural_pop^2 + ag_gdp^2 + gov_party + contrib_agri + econdev + agtaxcredit + pldvpag+theta4_lag, data = geo_metadata)
modelsummary(lm5); modelsummary(lm6); modelsummary(lm7); modelsummary(lm8)

# Pretty solid evidence for path dependence, possible followup paper

# Find the index of the most probable topic for each document
dominant_topics <- apply(theta2, 1, which.max)

# Count how many times each topic was dominant
topic_counts <- table(dominant_topics)

# Convert to data frame for display or saving
topic_counts_df <- as.data.frame(topic_counts)
colnames(topic_counts_df) <- c("Topic", "Document_Count")

# Sort by count descending
topic_counts_df <- topic_counts_df[order(-topic_counts_df$Document_Count), ]

# View result
print(topic_counts_df)
