#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Calculate & Plot rest time per interaction                   #
#################################################################

# Packages -----------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
rm(list = ls())
options(scipen = 999)

# Import database ---------------------------------------------------------

#import survey
read_wave <- read_csv("~/GitHub/ideology_consumption_network/01_data/read_wave.csv")
read_wave <- as.data.frame(read_wave)

read_wave <- read_wave %>% rename(age_group=Edad, education = Educacion, gender=Genero, dummy_working=QSDTrabaja, state=Provincia, people_home=  personasHogar) 

#import database with network
flow_argentina <- read_csv("C:/Users/User/Documents/GitHub/ideology_internet_consumption/01_data/flow_interaction_arg.csv")
flow_argentina <- as.data.frame(flow_argentina)

head(flow_argentina)


flow_argentina %>% arrange(panelist_id, datetime_first)
ymd_hms("2021-11-10 00:00:37", tz = "UTC") + seconds(3)
ymd_hms("2021-11-10 00:00:37", tz = "UTC") + seconds(3) + seconds(181)



# set "rest time" as the seconds between the person left one app and enter into another one. In that context, "seconds_between" it is transformated in "rest time"
flow_argentina <- flow_argentina %>% rename(rest_time = seconds_between)

# delete duplicate values
unique_rows <- !duplicated(flow_argentina[, c("panelist_id", "datetime_first","datetime_second","first_interaction", "second_interaction",  
                                              "first_type",  "second_type","rest_time")])
flow_argentina <- flow_argentina[unique_rows, ]

# filter the rest time >= 0, there is a data issue that some interactions appear as minor cero interaction
flow_argentina <- flow_argentina %>% filter(rest_time >= 0)


# So, I have one rest time per interaccion. Now we need to get one 
# rest time per person (the mean and the variance will help too) per group (conservatives and progressive)
#join later with the survey dataframe

join_flow_argentina_wave_measures <- flow_argentina %>%
  group_by(panelist_id) %>%
  summarise(mean_rest_time = mean(rest_time, na.rm = T), 
            median_rest_time = median(rest_time, na.rm = T), 
            max_rest_time = max(rest_time, na.rm = T), 
            min_rest_time = min(rest_time, na.rm = T), 
            var_rest_time = var(rest_time, na.rm = T), 
            sd_rest_time = sqrt(var(rest_time, na.rm = T)), 
            mean_logplus1_rest_time = log(mean(rest_time, na.rm = T))+1, 
            median_logplus1_rest_time = log(median(rest_time, na.rm = T))+1, 
            var_logplus1_rest_time = log(var(rest_time, na.rm = T))+1, 
            
  )  %>% 
  left_join(read_wave[, c("panelist_id", "age_group", "ideology","education", "gender",  "dummy_working", "state", "people_home")])

join_flow_argentina_wave_measures <- as.data.frame(join_flow_argentina_wave_measures)


# Plot no.1. --------------------------------------------------------------

#Plot the distribution of the rest time by Ideological Prefferences (log)

plot_rest_time <- join_flow_argentina_wave_measures %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))


median_progre <- median(join_flow_argentina_wave_measures[join_flow_argentina_wave_measures$ideology %in% c(1,2), "median_logplus1_rest_time" ], na.rm = T)
median_conserva <- median(join_flow_argentina_wave_measures[join_flow_argentina_wave_measures$ideology %in% c(6,7), "median_logplus1_rest_time" ], na.rm = T)

dat_text <- data.frame(
  group_ideology = c("Progressives", "Conservatives"),
  label   = c(paste0("Median: ", round(median_progre,2)), paste0("Median: ",round(median_conserva, 2))
  )
)

p1 <- plot_rest_time %>%
  ggplot() +
  geom_density(aes( x= median_logplus1_rest_time, fill = group_ideology)) +
  theme_bw() +
  scale_fill_manual(values = c( "#F21A0080", "#3B9AB280")) +
  facet_wrap(~ group_ideology) +
  theme_bw() +
  labs( y = "Probability", x = "Median Inter-event",
        title = "Plot no.1.: Distribution of the rest time by Ideological Prefferences (log)") +
  geom_vline(data=filter(plot_rest_time, group_ideology=="Progressives"), aes(xintercept=median_progre), 
             colour="grey", linetype="dashed", size = 0.8) + 
  geom_vline(data=filter(plot_rest_time, group_ideology=="Conservatives"), aes(xintercept=median_conserva), 
             colour="grey", linetype="dashed", size = 0.8) + 
  geom_text(
    data    = dat_text,
    mapping = aes(x = 2.6, y = 0.25, label = label),
    inherit.aes = FALSE,
    hjust   = -0.1,
    vjust   = -1
  ) +
  theme(legend.position = "none")
p1



png(filename = "C:/Users/User/Documents/GitHub/ideology_internet_consumption/05_plots/Distribution of the rest time by Ideological Prefferences.png",
    width = 15, height = 10, units = "in", res = 300)
print(p1)
dev.off()


# Plot no.A2.1. --------------------------------------------------------------

# Plot the distribution of the rest time by Ideological Prefferences


median_progre <- median(join_flow_argentina_wave_measures[join_flow_argentina_wave_measures$ideology %in% c(1,2), "median_rest_time" ], na.rm = T)
median_conserva <- median(join_flow_argentina_wave_measures[join_flow_argentina_wave_measures$ideology %in% c(6,7), "median_rest_time" ], na.rm = T)

var(join_flow_argentina_wave_measures[join_flow_argentina_wave_measures$ideology %in% c(1,2), "median_rest_time" ], na.rm = T)
var(join_flow_argentina_wave_measures[join_flow_argentina_wave_measures$ideology %in% c(6,7), "median_rest_time" ], na.rm = T)

dat_text <- data.frame(
  group_ideology = c("Progressives", "Conservatives"),
  label   = c(paste0("Median: ", round(median_progre,2)), paste0("Median: ",round(median_conserva, 2))
  )
)

p1 <- plot_rest_time %>%
  ggplot() +
  geom_density(aes( x= median_rest_time, fill = group_ideology)) +
  theme_bw() +
  scale_fill_manual(values = c( "#F21A0080", "#3B9AB280")) +
  facet_wrap(~ group_ideology) +
  theme_bw() +
  labs( y = "Probability", x = "Median Inter-event",
        title = "Plot no.A2.1.: Distribution of the rest time by Ideological Prefferences") +
  geom_vline(data=filter(plot_rest_time, group_ideology=="Progressives"), aes(xintercept=median_progre), 
             colour="grey", linetype="dashed", size = 0.8) + 
  geom_vline(data=filter(plot_rest_time, group_ideology=="Conservatives"), aes(xintercept=median_conserva), 
             colour="grey", linetype="dashed", size = 0.8) + 
  geom_text(
    data    = dat_text,
    mapping = aes(x = 2.6, y = 0.25, label = label),
    inherit.aes = FALSE,
    hjust   = -0.1,
    vjust   = -1
  ) +
  theme(legend.position = "none")
p1



# Plot no.2. --------------------------------------------------------------

# Plot the distribution of the rest time by Ideological Prefferences and age (log)

dat_text <- data.frame(
  group_ideology = c("Progressives", "Conservatives"),
  label   = c(paste0("Median: ", round(median_progre,2)), paste0("Median: ",round(median_conserva, 2))
  )
)

plot_rest_time <- join_flow_argentina_wave_measures %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))


median_data <- plot_rest_time %>% 
  group_by(group_ideology, age_group) %>% 
  summarise(median_x = median(median_logplus1_rest_time, na.rm = T)) %>% 
  ungroup()

plot_rest_time$age_group <- as.factor(as.character(plot_rest_time$age_group))
plot_rest_time$age_group <- factor(plot_rest_time$age_group, labels = c("Between 18 and 25", 
                                                                        "Between 26 and 35", 
                                                                        "Between 36 and 45", "Between 46 and 55",
                                                                        "Between 56 and 65", "Older than 65"),
                                   levels = c("20", "32", "41", "50", "59", "68")
)
plot_rest_time %>%
  ggplot() +
  geom_density(aes( x= median_logplus1_rest_time, fill = group_ideology)) +
  theme_bw() +
  scale_fill_manual(values = c( "#F21A0080", "#3B9AB280")) +
  facet_grid(age_group~ group_ideology
  ) +
  theme_bw() +
  geom_text(data = median_data, 
            aes(x = median_x+1.3, y = Inf, label = paste("Median:", round(median_x, 2))),
            vjust = 2,
            hjust = 0.5) +
  
  labs( y = "Proportion", x = "Median Inter-event",
        title = "Plot no.2.: Distribution of the rest time by Ideological Prefferences and age (log)") +
  geom_vline(median_data,mapping =aes(xintercept = median_x-0.2),colour="grey", linetype="dashed", size = 0.8) +
  theme(legend.position = "none")



# Plot no.A2.2 ------------------------------------------------------------

# Plot the distribution of the rest time by Ideological Prefferences and gender (log)

median_data <- plot_rest_time %>% 
  group_by(gender) %>% 
  summarise(median_x = median(median_logplus1_rest_time, na.rm = T)) %>% 
  ungroup()


plot_rest_time %>%
  ggplot() +
  geom_density(aes( x= median_logplus1_rest_time#, fill = group_ideology
  )) +
  theme_bw() +
  scale_fill_manual(values = c( "#F21A0080", "#3B9AB280")) +
  facet_wrap(~gender#~ group_ideology
  ) +
  theme_bw() +
  geom_text(data = median_data, 
            aes(x = median_x+1.5, y = Inf, label = paste("Median:", round(median_x, 2))),
            vjust = 2,
            hjust = 0.5) +
  
  labs( y = "Proportion", x = "Median Inter-event",
        title = "Plot no.A2.2.: Distribution of the rest time by Ideological Prefferences and gender (log)") +
  geom_vline(median_data,mapping =aes(xintercept = median_x-0.2),colour="grey", linetype="dashed", size = 0.8) +
  theme(legend.position = "none")


# Plot no.A2.3 ------------------------------------------------------------

# Plot the distribution of the rest time by Ideological Prefferences and gender (log)

plot_rest_time <- join_flow_argentina_wave_measures %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"),
         gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female",
                            gender == 4 ~"Other"))

head(plot_rest_time)


median_data <- plot_rest_time %>% 
  group_by(group_ideology, gender) %>% 
  summarise(median_x = median(median_logplus1_rest_time, na.rm = T)) %>% 
  ungroup()


plot_rest_time %>%
  ggplot() +
  geom_density(aes( x= median_logplus1_rest_time, fill = group_ideology)) +
  theme_bw() +
  scale_fill_manual(values = c( "#F21A0080", "#3B9AB280")) +
  facet_grid(gender~ group_ideology
  ) +
  theme_bw() +
  geom_text(data = median_data, 
            aes(x = median_x+1, y = Inf, label = paste("Median:", round(median_x, 2))),
            vjust = 2,
            hjust = 0.5) +
  
  labs( y = "Proportion", x = "Median Inter-event",
        title = "Plot no.A2.3.: Distribution of the rest time by Ideological Prefferences and gender (log)") +
  geom_vline(median_data,mapping =aes(xintercept = median_x-0.2),colour="grey", linetype="dashed", size = 0.8) +
  theme(legend.position = "none")





# Plot no.A2.3 ------------------------------------------------------------

# generate a 3d plot with the information of the variables of ideology, age and rest time

#install.packages("plot3D")

library("plot3D")

head(plot_rest_time)
#x <-ifelse(plot_rest_time$gender == "Male",1,2)
x <- plot_rest_time$ideology
y <-plot_rest_time$age_group
z <-plot_rest_time$median_logplus1_rest_time

scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,bty = "g",
          xlab = "Ideology",  ticktype = "detailed", #type = "h", 
          ylab ="Age", zlab = "Median Rest time (log)",
          main = "Plot no.A2.3.: 3D Scatter Plot Analysis of Ideology, Age and Rest Time")


# Plot no.A2.4. -----------------------------------------------------------

# generate a 3d plot with the information of the variables of ideology, age and rest time


fit <- lm(z ~ x + y)
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)

scatter3D(x, y, z, pch = 18,  theta = 1, phi = 15,bty = "g",
          xlab = "Ideology",  ticktype = "detailed", #type = "h", 
          ylab ="Age", zlab = "Median Rest time (log)",
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints),
          main = "Plot no.A2.4.: 3D Scatter Plot Analysis of Ideology, Age and Rest Time")



# Entropy calculation -----------------------------------------------------

# Import dataset
flow_interaction_arg <- read_csv("~/GitHub/ideology_consumption_network/01_data/flow_interaction_arg.csv")
flow_interaction_arg <- as.data.frame(flow_interaction_arg)

# Import Survey
read_wave <- read_csv("~/GitHub/ideology_consumption_network/01_data/read_wave.csv")
read_wave <- as.data.frame(read_wave)

# Join dataset
flow_interaction_arg <- flow_interaction_arg %>% left_join(read_wave[,c("panelist_id", "Genero", "Edad", "ideology",
                                                                        "QSDTrabaja", "Provincia")])
flow_interaction_arg[,c("panelist_id", "first_type", "second_type", "ideology")]

# Calculate Entropy for Progressives
transition_counts <- flow_interaction_arg %>%
   filter(ideology %in% c(1,2)) %>%
  group_by(first_type, second_type) %>%
  summarize(count = n())

total_transitions <- transition_counts %>%
  group_by(first_type) %>%
  summarize(total = sum(count))
transition_probabilities <- transition_counts %>%
  left_join(total_transitions, by = "first_type") %>%
  mutate(probability = count / total)
entropy_left <- transition_probabilities %>%
  group_by(first_type) %>%
  summarize(entropy = -sum(probability * log2(probability)))

# Calculate Entropy for Conservatives

transition_counts <- flow_interaction_arg %>%
  filter(ideology %in% c(6,7)) %>%
  group_by(first_type, second_type) %>%
  summarize(count = n())

total_transitions <- transition_counts %>%
  group_by(first_type) %>%
  summarize(total = sum(count))
transition_probabilities <- transition_counts %>%
  left_join(total_transitions, by = "first_type") %>%
  mutate(probability = count / total)
entropy_right <- transition_probabilities %>%
  group_by(first_type) %>%
  summarize(entropy = -sum(probability * log2(probability)))
