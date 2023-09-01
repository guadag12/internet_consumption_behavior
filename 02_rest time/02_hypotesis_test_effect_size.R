#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Calculate hypotesis test & effect sizes for rest time        #
#################################################################

# Packages -----------------------------------------------------------------
library(tidyverse)
library(effsize)
library(RESI)

set.seed(123)
rm(list =ls())

# Import database ---------------------------------------------------------

#import survey

data <-  read.csv("C:/Users/User/Documents/GitHub/ideology_internet_consumption/01_data/join_flow_argentina_wave_measures.csv")
group1 <- data[data$ideology %in% c(1,2),"median_logplus1_rest_time"]
group2 <- data[data$ideology %in% c(6,7),"median_logplus1_rest_time"]

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
group1 <- data[data$ideology %in% c(1,2),"median_rest_time"]
group2 <- data[data$ideology %in% c(6,7),"median_rest_time"]
cliff_delta_result <- cliff.delta(group1, group2)
cliff_delta_result$estimate
result_cohen <- cohen.d(group1, group2)


# Table no.5. -------------------------------------------------------------


# generates table no.5.

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
write.csv(data_mesures, "table_permutation_effect_size_all.csv", row.names = F)

# Table no.6. -------------------------------------------------------------

# generate for loop by age group to calculate the tests and the effect size
data_empty <- data.frame("Group" = NA,
                         "Median Conservatives" = NA,
                         "Median Progressives" = NA,
                         "Welch's Test (statistics)" = NA,
                         "Welch's Test (p-value)" = NA,
                         "Cohen Test" =NA,
                         "Mann-Whitney U Test with Rank-biserial Correlation" = NA,
                         "Cliff Delta" = NA)
data_empty <- data_empty[0]

## generate table no.6.
for(k in unique(data$age_group)[1:6]){
  
  group1 <- data[data$ideology %in% c(1,2) & data$age_group == k,"median_logplus1_rest_time"]
  group2 <- data[data$ideology %in% c(6,7) & data$age_group == k,"median_logplus1_rest_time"]
  
  
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
  
  set.seed(123)
  
  obs_t_stat <- welch_t(group1, group2)
  
  # Initialize
  n_permutations <- 10000  # Number of permutations
  perm_t_stats <- numeric(n_permutations)  # To store permutation t-stats
  
  # Conduct the permutation test
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
  
  data_empty <- rbind(data_mesures1, data_empty)
  
}

#save table no.6.
write.csv(data_empty, "table_permutation_effect_size_age_groups.csv", row.names = F)




