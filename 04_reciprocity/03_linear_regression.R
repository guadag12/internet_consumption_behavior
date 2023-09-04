#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Linear Regression of y variable = "time interevent"          #
#################################################################


# Packages -----------------------------------------------------------------

library(RESI)
library(effsize)
library(tidyverse)
library(readr)
library(relaimpo)
library(igraph)
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


# Wrangling data ----------------------------------------------------------

# Filter original data set to include only specific ideology values
data_prueba<- data_network[data_network$ideology %in% c(1,2,6,7), ]

# Filter out rows where rest_time is zero or negative
data_prueba <- data_prueba %>% filter(rest_time > 0)

# Group the data by source and target to calculate new variables
data_prueba<- data_prueba %>% group_by(source, target) %>%
  mutate(frequency = n(),
         weight = frequency * (1 / time_interevent ),
         weight_log = log(weight)
  )
# Convert back to a regular data frame
data_prueba <- as.data.frame(data_prueba)

# Group by panelist_id and calculate new variables based on conditions
dataset_time <- data_prueba %>%
  arrange(panelist_id, datetime_first) %>% # Sort by id and datetime_first, if not already sorted
  group_by(panelist_id) %>% # Group by id, if you have multiple ids
  mutate(condition = case_when(
    (lag(source, order_by = datetime_first) == target) & (source != target) ~ 1, 
    (lag(source, order_by = datetime_first) == target) & (source == target) ~ 0, 
    T ~ 0),
    time_difference =
      case_when(
        (lag(source, order_by = datetime_first) == target) & (source != target) ~  (amount_seconds_first+rest_time)+1, 
        (lag(source, order_by = datetime_first) == target) & (source == target) ~ 0, 
        T ~ 0)
  ) %>%
  mutate(weight = condition * (1 / time_difference ),
         weight = weight*2 + 0.3,
         weight = (ifelse(is.na(weight ), 0, weight))*2,
         weight = ifelse(weight ==0, 0, weight),
         weight_log = log(weight+1)*1.2) 
head(dataset)

# Join additional variables from read_wave
dataset_time <- dataset_time %>% left_join(read_wave[, c("panelist_id", "age_group", "ideology","education", "gender",  "dummy_working", "state", "people_home")]) 

# Filter and modify data for plotting
plot_rest_time <- dataset_time %>%
  filter(weight>0.0) %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))
plot_rest_time <- as.data.frame(plot_rest_time)

# Group by panelist_id, ideology, and age_group to calculate median_timedifference
data_table <- plot_rest_time %>%
  group_by(panelist_id, ideology, age_group) %>%
  summarise(median_timedifference = median(time_difference, na.rm = T),
            median_logplus1_timedifference = log(median(time_difference, na.rm = T)+1))

# Convert back to a regular data frame
data_table <- as.data.frame(data_table)

# Join additional variables from read_wave
data_table <- data_table %>% left_join(read_wave[, c("panelist_id", "education", "gender",  "dummy_working", "state", "people_home")])

# Add new variables based on conditions and categorize variables as factors
data <- data_table %>%
  mutate(region = case_when(
    state %in% c(23) ~  "CABA",
    state %in% c(1) ~ "Buenos Aires",
    state %in% c(37,  27, 41, 38,44) ~  "Patagonia",
    state %in% c(42, 28,33) ~  "Centro",
    state %in% c(46, 35, 40) ~  "Cuyo",
    state %in% c(32, 43, 25, 39,
                 45, 34) ~  "NOA",
    state %in% c(29,  26, 36, 36,
                 31) ~  "NEA",
    T ~as.character(state)),
    dummy_working  = ifelse(dummy_working == 1, "Working", "Not working"),
    gender = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      gender == 4 ~ "Other",
      T ~ "NA"
    ),
    
    education = case_when(
      education %in% c(1,2) ~ "Elementary school (complete & incomplete)",
      education %in% c(3,9) ~ "High school (complete & incomplete)",
      education %in% c(5,6) ~ "University (complete & incomplete)",
      education %in% c(7,8) ~ "Postgrad school (complete & incomplete)",
      T ~ as.character(education)
    ),
    people_home = ifelse(is.na(people_home), 0, people_home)
    
  )

# Convert certain variables to factors
data$age_group_ <- as.factor(data$age_group)
data$education <- as.factor(data$education)
data$gender <- as.factor(data$gender)
data$dummy_working <- as.factor(data$dummy_working)
data$region <- as.factor(data$region)
data$people_home <- as.factor(data$people_home)


# Linear Regression -------------------------------------------------------
# Load the texreg package
library(texreg)

# Filter the data to include specific ideology values and create a new variable dummy_ideology
data_lm <- data %>% 
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(dummy_ideology = ifelse(ideology %in% c(1,2), 1, 0))



# Model 1 (age as continuos) ----------------------------------------------

# Define predictors (x) and response (y)
x <- data_lm[, c( "age_group", "dummy_ideology", "education", 
                  "gender", "dummy_working", "region")]

y <- data_lm[, c("median_logplus1_timedifference")]

# Perform linear regression (model 1)
lm.1 <- lm(median_logplus1_timedifference ~ age_group +dummy_ideology+education+gender+dummy_working+region,
           data = data_lm)
# Display summary statistics of the first model
summary(lm.1)

# Extract summary and performance metrics for model 1
summary_stats <- summary(lm.1)

# Extract R-squared and adjusted R-squared
r_squared <- summary_stats$r.squared
adj_r_squared <- summary_stats$adj.r.squared

# Calculate and display RMSE for model 1
predictions <- predict(lm.1)
actuals <- y
rmse_value <- rmse(actuals, predictions)
# Print results
cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n")
cat("RMSE:", rmse_value, "\n")


# Model 2 (age as factor) ----------------------------------------------

# Perform another linear regression with age as factor
lm.2 <- lm(median_logplus1_timedifference ~ age_group_+dummy_ideology+education+gender+dummy_working+region,
           data = data_lm)

# Display summary statistics of the second model
summary(lm.2)

# Extract summary and performance metrics for model 2
summary_stats <- summary(lm.2)

# Extract R-squared and adjusted R-squared
r_squared2 <- summary_stats$r.squared
adj_r_squared2 <- summary_stats$adj.r.squared

# Calculate and display RMSE for model 2
predictions2 <- predict(lm.2)
actuals2 <- y
rmse_value2 <- rmse(actuals2, predictions2)

# Print results
cat("R-squared:", r_squared2, "\n")
cat("Adjusted R-squared:", adj_r_squared2, "\n")
cat("RMSE:", rmse_value2, "\n")


#Save summary of both models
library(textreg)
wordreg(l = list(lm.1,lm.2), file = "linear_regression_repetitive_behavior.doc",
        single.row = TRUE, 
        custom.model.names = c("Model 1 (age as  continuous)",
                               "Model 2 (age as factor)"),
        custom.coef.names	= c( "Intercept",
                               "Age Group",
                               "Dummy Ideology",
                               "Education High school (complete & incomplete)",
                               "Education Postgrad school (complete & incomplete)",
                               "Education University (complete & incomplete)",
                               "Gender Male",
                               "Gender Other",
                               "Dummy Working Situation",
                               "Region Buenos Aires",
                               "Region CABA",
                               "Region Centro",
                               "Region Cuyo",
                               "Region NEA",
                               "Region NOA",
                               "Region Patagonia",
                               
                               "Age between 26 and 35",
                               "Age between 36 and 45",
                               "Age between 46 and 55",
                               "Age between 56 and 65",
                               "Age older than 66"),
        stars = c(0.001, 0.01, 0.05, 0.1)
        
        
)

# Calculate relative importance of predictors for both models

metrics <- calc.relimp(lm.1, type = c("lmg", "first", "last"))
metrics

metrics <- calc.relimp(lm.2, type = c("lmg", "first", "last"))
metrics


# Random Forest plot ------------------------------------------------------



# Import and clean data for Random Forest feature importance plot

importance_rf <- read_csv("C:/Users/User/Documents/GitHub/ideology_internet_consumption/03_y3_local_coefficient/importance_rf.csv")
importance_rf <- importance_rf %>% rename(Feature=Features)
importance_rf$Feature <- gsub( "_", " ", importance_rf$Feature)
importance_rf$Feature <- toTitleCase(importance_rf$Feature)
importance_rf <- as.data.frame(importance_rf)
head(importance_rf)
ggplot( )+
  geom_bar(data = importance_rf, aes(y = reorder(Feature, Importance), x = Importance), stat = "identity", fill = "grey60") +
  labs(x = "Importance", y = "Feature", title = "Plot no.A2.9: Feature importance according to Random Forest Model for hypotesis 3",
       caption= "ntree = 1100, max_nodes = 100") +
  theme_bw() +
  geom_text(data=importance_rf,aes(label=round(Importance,2),x=Importance+0.01,y=Feature),inherit.aes=FALSE)

geom_text(data = importance_rf, aes(x =  reorder(Feature, Importance), y = Importance, label = round(Importance,2))) 

