#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Calculate Entropy per group                                  #
#################################################################

# Packages -----------------------------------------------------------------
library(tidyverse)
library(readr)
options(scipen = 9999)
rm(list = ls())

# Import database ---------------------------------------------------------

#import database
flow_interaction_arg <- read_csv("~/GitHub/ideology_consumption_network/01_data/flow_interaction_arg.csv")
flow_interaction_arg <- as.data.frame(flow_interaction_arg)
head(flow_interaction_arg)

#import survey
read_wave <- read_csv("~/GitHub/ideology_consumption_network/01_data/read_wave.csv")
read_wave <- as.data.frame(read_wave)

# Join Dataset
flow_interaction_arg <- flow_interaction_arg %>% left_join(read_wave[,c("panelist_id", "Genero", "Edad", "ideology",
                                                                        "QSDTrabaja", "Provincia")])
flow_interaction_arg[,c("panelist_id", "first_type", "second_type", "ideology")]

#Calculate entropy for Progressives
transition_counts_p <- flow_interaction_arg %>%
  filter(ideology %in% c(1,2)) %>%
  group_by(first_type, second_type) %>%
  summarize(count = n())
total_transitions_p <- transition_counts_p %>%
  group_by(first_type) %>%
  summarize(total = sum(count))
transition_probabilities_p <- transition_counts_p %>%
  left_join(total_transitions, by = "first_type") %>%
  mutate(probability = count / total)

entropy_p <- transition_probabilities_p %>%
  group_by(first_type) %>%
  summarize(entropy = -sum(probability * log2(probability)))
head(entropy_p)

# # A tibble: 3 x 2
# first_type   entropy
# desktop        0.176
# mobile - app   0.284
# mobile - web   0.766

#. Higher entropy indicates higher unpredictability or more even 
#distribution of transitions, 
#while lower entropy indicates more predictable patterns.

#Calculate entropy for Conservatives
transition_counts_c <- flow_interaction_arg %>%
  filter(ideology %in% c(6,7)) %>%
  group_by(first_type, second_type) %>%
  summarize(count = n())
total_transitions_c <- transition_counts_c %>%
  group_by(first_type) %>%
  summarize(total = sum(count))
transition_probabilities_c <- transition_counts_c %>%
  left_join(total_transitions, by = "first_type") %>%
  mutate(probability = count / total)

entropy_c <- transition_probabilities_c %>%
  group_by(first_type) %>%
  summarize(entropy = -sum(probability * log2(probability)))
head(entropy_c)

# first_type   entropy
# desktop        0.569
# mobile - app   0.636
# mobile - web   0.856


# Plot entropy
ggplot(entropy_c, aes(x = first_type, y = entropy)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Entropy of Transitions between Device Types",
       x = "Device Type",
       y = "Entropy") +
  theme_minimal()
