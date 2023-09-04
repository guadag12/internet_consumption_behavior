#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Hypotesis test and effect size                               #
#################################################################

# Packages -----------------------------------------------------------------

library(RESI)
library(effsize)
library(tidyverse)
library(readr)
library(relaimpo)
library(tools)
library(Metrics)

rm(list = ls())
options(scipen = 999)

# Databases ---------------------------------------------------------------

read_wave <- read_csv("~/GitHub/ideology_consumption_network/01_data/read_wave.csv")
read_wave <- as.data.frame(read_wave)

read_wave <- read_wave %>% rename(age_group=Edad, education = Educacion, gender=Genero, dummy_working=QSDTrabaja, state=Provincia, people_home=  personasHogar) 

data_network <- read.csv("C:/Users/User/Documents/GitHub/ideology_internet_consumption/01_data/flow_interaction_argentina_new_classification.csv")
data_network <- data_network %>% left_join(read_wave[,c("panelist_id", "age_group", "education", "gender", "dummy_working", "state", "people_home")])


# Tranformation of data ---------------------------------------------------
# Filter data to only include specific ideologies (1, 2, 6, 7)
data_prueba <- data_network[data_network$ideology %in% c(1,2,6,7), ]

# Further filter data to only include rows where 'rest_time' is greater than 0
data_prueba <- data_prueba %>% filter(rest_time > 0)

# Group data by 'source' and 'target' and calculate frequency, weight, and weight_log
data_prueba <- data_prueba %>% group_by(source, target) %>%
  mutate(frequency = n(),  # Count of rows per group
         weight = frequency * (1 / time_interevent),  # Calculate weight
         weight_log = log(weight)  # Log-transform weight
  )

# Convert to a data frame
data_prueba <- as.data.frame(data_prueba)

# Sort by 'panelist_id' and 'datetime_first', then group by 'panelist_id'
# Add new variables: 'condition' and 'time_difference'
dataset_time <- data_prueba %>%
  arrange(panelist_id, datetime_first) %>% 
  group_by(panelist_id) %>%
  mutate(condition = case_when(
    # Conditions for setting 'condition' variable
    (lag(source, order_by = datetime_first) == target) & (source != target) ~ 1,  
    (lag(source, order_by = datetime_first) == target) & (source == target) ~ 0,  
    T ~ 0),
    time_difference = case_when(
      # Calculate time_difference based on various conditions
      (lag(source, order_by = datetime_first) == target) & (source != target) ~  (amount_seconds_first+rest_time)+1,  
      (lag(source, order_by = datetime_first) == target) & (source == target) ~ 0,  
      T ~ 0)
  ) %>%
  # Further calculations for weight and its log-transform
  mutate(weight = condition * (1 / time_difference ),
         weight = weight*2 + 0.3,
         weight = (ifelse(is.na(weight), 0, weight))*2,
         weight = ifelse(weight ==0, 0, weight),
         weight_log = log(weight+1)*1.2)

# Display the head of the resulting dataset (Note: 'dataset' is not defined in this code snippet)
head(dataset)

# Join additional data columns from 'read_wave'
dataset_time <- dataset_time %>% left_join(read_wave[, c("panelist_id", "age_group", "ideology","education", "gender",  "dummy_working", "state", "people_home")])

# Filter dataset_time based on weight and ideology and add a 'group_ideology' column
plot_rest_time <- dataset_time %>%
  filter(weight > 0.0) %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))

# Convert to a data frame
plot_rest_time <- as.data.frame(plot_rest_time)

# Summarize data to get median 'time_difference' and its log-transform for each 'panelist_id', 'ideology', and 'age_group'
data_table <- plot_rest_time %>%
  group_by(panelist_id, ideology, age_group) %>%
  summarise(median_timedifference = median(time_difference, na.rm = T),
            median_logplus1_timedifference = log(median(time_difference, na.rm = T)+1))

# Convert summary table to a data frame
data_table <- as.data.frame(data_table)

# Display the head of the resulting summary data
head(data_table)


# Welch's test & Effect size by Ideology --------------------------

# Filter by group
group1 <- data_table[data_table$ideology %in% c(1,2),"median_timedifference"]
group2 <- data_table[data_table$ideology %in% c(6,7),"median_timedifference"]

# Function to calculate Welch's t-statistic
welch_t_stat <- function(x, y) {
  nx <- length(x)
  ny <- length(y)
  mx <- mean(x)
  my <- mean(y)
  vx <- var(x)
  vy <- var(y)
  
  t_stat <- (mx - my) / sqrt((vx/nx) + (vy/ny))
  return(t_stat)
}


# Initialize
n_permutations <- 10000
perm_t_stats <- numeric(n_permutations)
obs_t_stat <- welch_t_stat(group1, group2)

# Permutation loop
for(i in 1:n_permutations) {
  pooled_data <- c(group1, group2)
  permuted_data <- sample(pooled_data, length(pooled_data))
  
  perm_group1 <- permuted_data[1:length(group1)]
  perm_group2 <- permuted_data[(length(group1) + 1):length(pooled_data)]
  
  perm_t_stats[i] <- welch_t_stat(perm_group1, perm_group2)
}

# Calculate p-value
p_value <- mean(abs(perm_t_stats) >= abs(obs_t_stat))

result <- wilcox.test(group1,group2)

# Calculate U statistic
U <- result$statistic

# Calculate sample sizes
n1 <- length(group1)
n2 <- length(group2)

# Calculate effect size r
r <- U / sqrt(n1 * n2)

# Print effect size
rank_biserial <- 1 - (2 * U) / (n1 * n2)
group1 <- data_table[data_table$ideology %in% c(1,2),"median_timedifference"]
group2 <- data_table[data_table$ideology %in% c(6,7),"median_timedifference"]
cliff_delta_result <- cliff.delta(group1, group2)
cliff_delta_result$estimate
result_cohen <- cohen.d(group1, group2)

# Data measures per ideological group

data_mesures <- data.frame("Group" = c("Conservatives vs. Progressives"),
                           "Median Conservatives" = median(group2),
                           "Median Progressives" = median(group1),
                           "Welch's Test (statistics)" = obs_t_stat,
                           "Welch's Test (p-value)" = paste0(p_value, ifelse(p_value < 0.05, " (significant)", " (not significant)")),
                           "Cohen Test" = paste0(round(result_cohen$estimate,3), " (", ifelse( abs(result_cohen$estimate) <= 0.147, "negligible effect size",
                                                                                               ifelse(abs(result_cohen$estimate) <= 0.33, "small effect size",
                                                                                                      ifelse(abs(result_cohen$estimate) <= 0.474, "medium effect size", "large effect size"))) , ")"),
                           
                           "Mann-Whitney U Test with Rank-biserial Correlation" = paste0(round(rank_biserial,3), " (", ifelse( abs(rank_biserial) <= 0.147, "negligible",
                                                                                                                               ifelse(abs(rank_biserial) <= 0.33, "small",
                                                                                                                                      ifelse(abs(rank_biserial) <= 0.474, "medium", "large"))) , ")"),
                           
                           "Cliff Delta" = paste0(round(cliff_delta_result$estimate,3), " (", ifelse( abs(cliff_delta_result$estimate) <= 0.147, "negligible",
                                                                                                      ifelse(abs(cliff_delta_result$estimate) <= 0.33, "small",
                                                                                                             ifelse(abs(cliff_delta_result$estimate) <= 0.474, "medium", "large"))),  ")")             )

data_mesures

write.csv(data_mesures, "C:/Users/User/Documents/GitHub/ideology_internet_consumption/03_y3_local_coefficient/data_mesures.csv", row.names = F)



# Welch's test & Effect size by Age and Ideology --------------------------

# Initialize an empty data frame to store results

data_empty <- data.frame("Group" = NA,
                         "Median Conservatives" = NA,
                         "Median Progressives" = NA,
                         "Welch's Test (statistics)" = NA,
                         "Welch's Test (p-value)" = NA,
                         "Cohen Test" =NA,
                         "Mann-Whitney U Test with Rank-biserial Correlation" = NA,
                         "Cliff Delta" = NA)
data_empty <- data_empty[0]

# Loop through unique age groups to perform various statistical tests

for(k in unique(data_table$age_group)[1:6]){
  
  # Extract data for specific age group and ideology
  group1 <- data_table[data_table$ideology %in% c(1,2) & data_table$age_group == k,"median_timedifference"]
  group2 <- data_table[data_table$ideology %in% c(6,7) & data_table$age_group == k,"median_timedifference"]
  
  # Define a function to calculate the Welch t-statistic
  welch_t <- function(x, y) {
    n1 <- length(x)
    n2 <- length(y)
    m1 <- mean(x)
    m2 <- mean(y)
    v1 <- var(x)
    v2 <- var(y)
    
    t_stat <- (m1 - m2) / sqrt((v1 / n1) + (v2 / n2))
    return(t_stat)
  }
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Calculate observed Welch t-statistic
  obs_t_stat <- welch_t(group1, group2)
  
  # Initialize variables for permutation test
  n_permutations <- 10000  # Number of permutations
  perm_t_stats <- numeric(n_permutations)  # To store permutation t-stats
  
  # Perform the permutation test
  for(i in 1:n_permutations) {
    combined_data <- c(group1, group2)
    permuted_data <- sample(combined_data, size=length(combined_data), replace=FALSE)
    
    perm_group1 <- permuted_data[1:length(group1)]
    perm_group2 <- permuted_data[(length(group1) + 1):length(combined_data)]
    
    perm_t_stats[i] <- welch_t(perm_group1, perm_group2)
  }
  
  # Calculate the p-value
  p_value <- mean(abs(perm_t_stats) >= abs(obs_t_stat))
  
  result <- wilcox.test(group1,group2)
  
  # Calculate U statistic
  U <- result$statistic
  
  # Calculate sample sizes
  n1 <- length(group1)
  n2 <- length(group2)
  
  # Calculate effect size r
  r <- U / sqrt(n1 * n2)
  
  # Print effect size
  rank_biserial <- 1 - (2 * U) / (n1 * n2)
  cliff_delta_result <- cliff.delta(group1, group2)
  result_cohen <- cohen.d(group1, group2)
  
  
  # Combine all results into a data frame
  data_mesures1 <- data.frame("Group" = c(paste0("Conservatives (age ", k, ") ",  "vs." ," Progressives (age ", k, ")")),
                              "Median Conservatives" = median(group2),
                              "Median Progressives" = median(group1),
                              "Welch's Test (statistics)" = obs_t_stat,
                              "Welch's Test (p-value)" = paste0(p_value, ifelse(p_value < 0.05, " (significant)", " (not significant)")),
                              "Cohen Test" = paste0(round(result_cohen$estimate,3), " (", ifelse( abs(result_cohen$estimate) <= 0.147, "negligible effect size",
                                                                                                  ifelse(abs(result_cohen$estimate) <= 0.33, "small effect size",
                                                                                                         ifelse(abs(result_cohen$estimate) <= 0.474, "medium effect size", "large effect size"))) , ")"),
                              
                              "Mann-Whitney U Test with Rank-biserial Correlation" = paste0(round(rank_biserial,3), " (", ifelse( abs(rank_biserial) <= 0.147, "negligible",
                                                                                                                                  ifelse(abs(rank_biserial) <= 0.33, "small",
                                                                                                                                         ifelse(abs(rank_biserial) <= 0.474, "medium", "large"))) , ")"),
                              
                              "Cliff Delta" = paste0(round(cliff_delta_result$estimate,3), " (", ifelse( abs(cliff_delta_result$estimate) <= 0.147, "negligible",
                                                                                                         ifelse(abs(cliff_delta_result$estimate) <= 0.33, "small",
                                                                                                                ifelse(abs(cliff_delta_result$estimate) <= 0.474, "medium", "large"))),  ")")             )
  # Append results to the empty data frame
  data_empty <- rbind(data_mesures1, data_empty)
  
}
# Display the first few rows of the result data frame


head(data_empty)

# Write the result to a CSV file
write.csv(data_empty, "C:/Users/User/Documents/GitHub/ideology_internet_consumption/03_y3_local_coefficient/table_permutation_effect_size_age_groups.csv", row.names = F)

