# ==============================================================================
# Topic Distribution by Wave - WITH STATE NAMES
# Creates publication-ready figure showing which states adopted in each wave
# ==============================================================================

library(tidyverse)
library(here)

# ==============================================================================
# LOAD DATA
# ==============================================================================

data <- read_csv("../../2_Data/Processed/analysis_data_full_sample.csv")

# Define topic names for better labeling
topic_names <- c(
  "topic1" = "Liability/Warning",
  "topic2" = "Activity Definition", 
  "topic3" = "Land Use/Zoning",
  "topic4" = "Recreational Use"
)

# ==============================================================================
# PREPARE DATA
# ==============================================================================

# Create state abbreviations for compact labeling
state_abbrev <- c(
  "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "Arkansas" = "AR",
  "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE",
  "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID",
  "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS",
  "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD",
  "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
  "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV",
  "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY",
  "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK",
  "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC",
  "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT",
  "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV",
  "Wisconsin" = "WI", "Wyoming" = "WY"
)

data$abbrev <- state_abbrev[data$state]

# Get states by wave with their dominant topics
states_by_wave <- data %>%
  arrange(adoption_wave, state) %>%
  group_by(adoption_wave) %>%
  summarise(
    states = paste(abbrev, collapse = ", "),
    n_states = n(),
    .groups = "drop"
  )

print(states_by_wave)

# Calculate mean topic proportions by wave
wave_means <- data %>%
  group_by(adoption_wave) %>%
  summarise(
    topic1 = mean(topic1),
    topic2 = mean(topic2),
    topic3 = mean(topic3),
    topic4 = mean(topic4),
    n = n(),
    .groups = "drop"
  )

# Pivot for stacked bar chart
wave_long <- wave_means %>%
  pivot_longer(cols = starts_with("topic"), names_to = "topic", values_to = "proportion") %>%
  mutate(topic_label = topic_names[topic])

# ==============================================================================
# CREATE FIGURE
# ==============================================================================

# Custom colors
topic_colors <- c(
  "Liability/Warning" = "#66c2a5",      # Green
  "Activity Definition" = "#fc8d62",    # Orange  
  "Land Use/Zoning" = "#8da0cb",         # Blue
  "Recreational Use" = "#e78ac3"         # Pink
)

# Create the plot
p <- ggplot(wave_long, aes(x = adoption_wave, y = proportion, fill = topic_label)) +
  geom_bar(stat = "identity", width = 0.7, color = "white", size = 0.3) +
  
  # Add state labels at the top of each bar
  geom_text(data = states_by_wave,
            aes(x = adoption_wave, y = 1.02, label = states, fill = NULL),
            size = 2.5, hjust = 0.5, vjust = 0, fontface = "bold",
            inherit.aes = FALSE) +
  
  # Add count labels
  geom_text(data = wave_means,
            aes(x = adoption_wave, y = -0.05, label = paste0("n=", n), fill = NULL),
            size = 3, fontface = "italic",
            inherit.aes = FALSE) +
  
  scale_fill_manual(values = topic_colors, name = "Topic") +
  scale_y_continuous(limits = c(-0.1, 1.15), breaks = seq(0, 1, 0.25),
                     labels = scales::percent_format(accuracy = 1)) +
  
  labs(
    title = "Topic Distribution by Adoption Wave (N=43)",
    subtitle = "State abbreviations shown above each wave",
    x = "Adoption Wave",
    y = "Mean Topic Proportion"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(size = 11),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

print(p)

# Save figure
ggsave("../../3_Output/Figures/figure_topic_by_wave_with_states.png"), p, 
       width = 12, height = 7, dpi = 300, bg = "white")

cat("\nSaved: figure_topic_by_wave_with_states.png\n")

# ==============================================================================
# ALTERNATIVE: Faceted version with state names inside bars
# ==============================================================================

# Create detailed data for each state
state_long <- data %>%
  select(state, abbrev, adoption_wave, topic1, topic2, topic3, topic4, dominant_topic) %>%
  pivot_longer(cols = starts_with("topic"), names_to = "topic", values_to = "proportion") %>%
  mutate(topic_label = topic_names[topic])

# Create faceted plot by wave
p2 <- ggplot(state_long, aes(x = reorder(abbrev, -proportion), y = proportion, fill = topic_label)) +
  geom_bar(stat = "identity", width = 0.8) +
  facet_wrap(~adoption_wave, scales = "free_x", ncol = 3) +
  scale_fill_manual(values = topic_colors, name = "Topic") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Topic Composition by State and Adoption Wave (N=43)",
    subtitle = "States sorted by dominant topic proportion within each wave",
    x = "State",
    y = "Topic Proportion"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    strip.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.spacing = unit(1, "lines")
  )

print(p2)

ggsave("../../3_Output/Figures/figure_topic_by_state_faceted.png"), p2,
       width = 14, height = 10, dpi = 300, bg = "white")

cat("Saved: figure_topic_by_state_faceted.png\n")

# ==============================================================================
# SUMMARY TABLE
# ==============================================================================

cat("\n")
cat(rep("=", 70), "\n", sep = "")
cat("STATES BY WAVE SUMMARY\n")
cat(rep("=", 70), "\n", sep = "")

for(i in 1:nrow(states_by_wave)) {
  cat("\n", states_by_wave$adoption_wave[i], " (n=", states_by_wave$n_states[i], "):\n", sep = "")
  
  wave_states <- data %>% 
    filter(adoption_wave == states_by_wave$adoption_wave[i]) %>%
    arrange(state) %>%
    select(state, dominant_topic, enact_year)
  
  for(j in 1:nrow(wave_states)) {
    cat("  ", wave_states$state[j], " (", wave_states$enact_year[j], ") - ", 
        wave_states$dominant_topic[j], "\n", sep = "")
  }
}
