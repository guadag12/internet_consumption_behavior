#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Linear Regression Analysis for y1 = rest time                #
#################################################################


# Packages -----------------------------------------------------------------

library(tidyverse)
library(readr)
library(relaimpo)
library(tools)
library(Metrics)
library(texreg)


rm(list = ls())
options(scipen = 999)



# Databases ---------------------------------------------------------------

#import dataset

data = read.csv("C:/Users/User/Documents/GitHub/ideology_internet_consumption/01_data/join_flow_argentina_wave_measures.csv")
data <- as.data.frame(data)

head(data)

# transform to factor numeric variables:
data <- data %>%
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
data$age_group_ <- as.factor(data$age_group)
data$education <- as.factor(data$education)
data$gender <- as.factor(data$gender)
data$dummy_working <- as.factor(data$dummy_working)
data$region <- as.factor(data$region)
data$people_home <- as.factor(data$people_home)


# Linear Regression -------------------------------------------------------

# filter the data that we need: groups 1,2,6,7 and create the dummy variable for ideology:
data_lm <- data %>% 
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(dummy_ideology = ifelse(ideology %in% c(1,2), 1, 0))

# Model 1 -----------------------------------------------------------------

# calculate based on "y"="median_logplus1_rest_time" and "age group" as a continuos variable


lm.1 <- lm(median_logplus1_rest_time ~ age_group +dummy_ideology+education+gender+dummy_working+region,
           data = data_lm)

summary_stats <- summary(lm.1)

# Extract R-squared and adjusted R-squared
r_squared <- summary_stats$r.squared
adj_r_squared <- summary_stats$adj.r.squared

# Calculate the RMSE
predictions <- predict(lm.1)
actuals <- y
rmse_value <- rmse(actuals, predictions)

# Print results
cat("R-squared:", r_squared, "\n")
cat("Adjusted R-squared:", adj_r_squared, "\n")
cat("RMSE:", rmse_value, "\n")



# Model 2 -----------------------------------------------------------------

#calculate the second model with the variation of "age_group_" that is a factor variable
lm.2 <- lm(median_logplus1_rest_time ~ age_group_+dummy_ideology+education+gender+dummy_working+region,
           data = data_lm)
summary(lm.2)

summary_stats <- summary(lm.2)

# Extract R-squared and adjusted R-squared
r_squared2 <- summary_stats$r.squared
adj_r_squared2 <- summary_stats$adj.r.squared

# Calculate the RMSE
predictions2 <- predict(lm.2)
actuals2 <- y
rmse_value2 <- rmse(actuals2, predictions2)

# Print results
cat("R-squared:", r_squared2, "\n")
cat("Adjusted R-squared:", adj_r_squared2, "\n")
cat("RMSE:", rmse_value2, "\n")


# Save both models --------------------------------------------------------

# use wordreg to save the output of the model

# Table no.A2.2. -----------------------------------------------------------

wordreg(l = list(lm.1,lm.2), file = "linear_regression_rest_time.doc",
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


# Calculate relative importance for both models  -------------------------------------------

# Table no.A2.3 -----------------------------------------------------------

#for model 1 :
metrics <- calc.relimp(lm.1, type = c("lmg", "first", "last"))
metrics


# Table no.A2. -----------------------------------------------------------

#for model 2:
metrics <- calc.relimp(lm.2, type = c("lmg", "first", "last"))
metrics



# After run 04_other_models.ipnyb run this -------------------------------------------------

# Table no.5 -----------------------------------------------------------

results_models <- data.frame(
  model = c("Linear Regression (model1)", "Linear Regression (model2)",
            "Random Forest (model2)",  "XGBOOST (model2)", "SVM (linear kernel) (model 2)",
            "SVM (rbf kernel) (model 2)"),
  R2 = c( 0.04201614, 0.05468822, -0.00467, -0.00228, -0.090, -0.145 ),
  AdjustR2 = c(0.02577913, 0.03430125 ,-0.0978,-0.0951, -0.191, 0.2516),
  RMSE = c(1.776385 , 1.786321 , 2.007, 1.881, 1.9621, 2.0112)
)



# Plot no.A2.5. -----------------------------------------------------------

importance_rf <- read_csv("~importance_rf.csv")

importance_rf$Feature <- gsub( "_", " ", importance_rf$Feature)
importance_rf$Feature <- toTitleCase(importance_rf$Feature)
importance_rf <- as.data.frame(importance_rf)
head(importance_rf)

ggplot( )+
  geom_bar(data = importance_rf, aes(y = reorder(Feature, Importance), x = Importance), stat = "identity", fill = "grey60") +
  labs(x = "Importance", y = "Feature", title = "Plot no.A2.5: Feature importance according to Random Forest Model",
       caption= "ntree = 1100, max_nodes = 100") +
  theme_bw() +
  geom_text(data=importance_rf,aes(label=round(Importance,2),x=Importance+0.01,y=Feature),inherit.aes=FALSE)

geom_text(data = importance_rf, aes(x =  reorder(Feature, Importance), y = Importance, label = round(Importance,2))) 
