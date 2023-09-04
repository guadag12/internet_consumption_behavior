#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Ratio duration/rest time wrangling and plot                  #
#################################################################

# Packages -----------------------------------------------------------------

rm(list = ls())
options(scipen = 999)
library(tidyverse)
library(readr)
library(relaimpo)
library(tools)
library(Metrics)
library(texreg)


# Databases ---------------------------------------------------------------

data <-  read.csv("join_flow_argentina_wave_ratio.csv")

read_wave <- read_csv("~/GitHub/ideology_consumption_network/01_data/read_wave.csv")
read_wave <- as.data.frame(read_wave)

read_wave <- read_wave %>% rename(age_group=Edad, education = Educacion, gender=Genero, dummy_working=QSDTrabaja, state=Provincia, people_home=  personasHogar) 

data <- data %>% left_join(read_wave[, c("panelist_id", "age_group", "ideology","education", "gender",  "dummy_working", "state", "people_home")])
head(data)

# Replace name of categories:
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
# Convert into factor the columns:
data$age_group_ <- as.factor(data$age_group)
data$education <- as.factor(data$education)
data$gender <- as.factor(data$gender)
data$dummy_working <- as.factor(data$dummy_working)
data$region <- as.factor(data$region)
data$people_home <- as.factor(data$people_home)


# Linear Regression -------------------------------------------------------


# Model 1 (age as continuos variable) -------------------------------------

# filter ideology groups 1,2,6,7 and convert into dummy
data_lm <- data %>% 
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(dummy_ideology = ifelse(ideology %in% c(1,2), 1, 0))

# Create X and Y variables
x <- data_lm[, c( "age_group", "dummy_ideology", "education", 
                  "gender", "dummy_working", "region")]

y <- data_lm[, c("median_logplus1_ratio")]

# Model:
lm.1 <- lm(median_logplus1_ratio ~ age_group +dummy_ideology+education+gender+dummy_working+region,
           data = data_lm)
summary(lm.1)

## save summary
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


# Model 2 (age group as factor) -------------------------------------------

lm.2 <- lm(median_logplus1_ratio ~ age_group_+dummy_ideology+education+gender+dummy_working+region,
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


# Save the results in a .doc file
wordreg(l = list(lm.1,lm.2), file = "linear_regression_ratio.doc",
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

metrics <- calc.relimp(lm.1, type = c("lmg", "first", "last"))
metrics



metrics <- calc.relimp(lm.2, type = c("lmg", "first", "last"))
metrics


# Plot no.A2.10. Importance Feature plot for Random Forest model --------------------


importance_rf <- read_csv("C:/Users/User/Documents/GitHub/ideology_internet_consumption/03_y2_ratio_duration_restime/importance_rf.csv")
importance_rf <- importance_rf %>% rename(Feature=Features)
importance_rf$Feature <- gsub( "_", " ", importance_rf$Feature)
importance_rf$Feature <- toTitleCase(importance_rf$Feature)
importance_rf <- as.data.frame(importance_rf)
head(importance_rf)
ggplot( )+
  geom_bar(data = importance_rf, aes(y = reorder(Feature, Importance), x = Importance), stat = "identity", fill = "grey60") +
  labs(x = "Importance", y = "Feature", title = "Plot no.A2.10.: Feature importance according to Random Forest Model in the ratio duration/rest time",
       caption= "ntree = 1100, max_nodes = 100") +
  theme_bw() +
  geom_text(data=importance_rf,aes(label=round(Importance,2),x=Importance+0.01,y=Feature),inherit.aes=FALSE)

geom_text(data = importance_rf, aes(x =  reorder(Feature, Importance), y = Importance, label = round(Importance,2))) 


