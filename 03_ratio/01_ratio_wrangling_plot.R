#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Ratio duration/rest time wrangling and plot                  #
#################################################################

# Packages -----------------------------------------------------------------

library(tidyverse)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
rm(list = ls())
options(scipen = 999)

# Databases ---------------------------------------------------------------

read_wave <- read_csv("~/GitHub/ideology_consumption_network/01_data/read_wave.csv")
read_wave <- as.data.frame(read_wave)

read_wave <- read_wave %>% rename(age_group=Edad, education = Educacion, gender=Genero, dummy_working=QSDTrabaja, state=Provincia, people_home=  personasHogar) 

flow_argentina <- read_csv("C:/Users/User/Documents/GitHub/ideology_internet_consumption/01_data/flow_interaction_arg.csv")

head(flow_argentina)


# Wrangling ---------------------------------------------------------------


flow_argentina %>% arrange(panelist_id, datetime_first)

flow_argentina <- as.data.frame(flow_argentina)

flow_argentina <- flow_argentina %>% rename(rest_time = seconds_between)

# Subset the original dataframe to get unique row
unique_rows <- !duplicated(flow_argentina[, c("panelist_id", "datetime_first","datetime_second","first_interaction", "second_interaction",  
                                              "first_type",  "second_type","rest_time")])

flow_argentina <- flow_argentina[unique_rows, ]


# filter when rest time minor zero
flow_argentina <- flow_argentina %>% filter(rest_time > 0)

# rename column names and calculate ratio and log
flow_argentina <- flow_argentina %>%
  rename(duration=amount_seconds_first ) %>%
  mutate(ratio_duration_resttime = (duration/rest_time)+1,
         log_ratio_duration_resttime = log(ratio_duration_resttime))

head(flow_argentina)


# calculate per user mean, median, maximum, minimum, variance and standard deviation
# for the ration variable. Also, calculate the log+1:
join_flow_argentina_wave_ratio <- flow_argentina %>%
  group_by(panelist_id) %>%
  summarise(mean_ratio = mean(ratio_duration_resttime, na.rm = T), 
            median_ratio = median(ratio_duration_resttime, na.rm = T), 
            max_ratio = max(ratio_duration_resttime, na.rm = T), 
            min_ratio = min(ratio_duration_resttime, na.rm = T), 
            var_ratio = var(ratio_duration_resttime, na.rm = T), 
            sd_ratio = sqrt(var(ratio_duration_resttime, na.rm = T)), 
            mean_logplus1_ratio = log(mean(ratio_duration_resttime, na.rm = T))+1, 
            median_logplus1_ratio = log(median(ratio_duration_resttime, na.rm = T))+1, 
            var_logplus1_ratio = log(var(ratio_duration_resttime, na.rm = T))+1, 
            
  )  %>% 
  left_join(read_wave[, c("panelist_id", "age_group", "ideology","education", "gender",  "dummy_working", "state", "people_home")])
join_flow_argentina_wave_ratio <- as.data.frame(join_flow_argentina_wave_ratio)

#export the dataset:
write.csv(join_flow_argentina_wave_ratio, "join_flow_argentina_wave_ratio.csv", row.names = F )



# Plots -------------------------------------------------------------------

# Filter by users with ideology in 1,2,6,7
plot_rest_time <- join_flow_argentina_wave_ratio %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))

# Calculate the median of the 'median_logplus1_ratio' variable for Progressive (ideology 1 and 2) and Conservative groups (ideology 6 and 7)
median_progre <- median(join_flow_argentina_wave_ratio[join_flow_argentina_wave_ratio$ideology %in% c(1,2), "median_logplus1_ratio" ], na.rm = T)
median_conserva <- median(join_flow_argentina_wave_ratio[join_flow_argentina_wave_ratio$ideology %in% c(6,7), "median_logplus1_ratio" ], na.rm = T)

# Create a data frame to hold text labels for plotting
dat_text <- data.frame(
  group_ideology = c("Progressives", "Conservatives"),
  label   = c(paste0("Median: ", round(median_progre,2)), paste0("Median: ",round(median_conserva, 2))
  )
)


# Plot no.3. --------------------------------------------------------------

# Plot
p1 <- plot_rest_time %>%
  ggplot() +
  geom_density(aes( x= median_logplus1_ratio, fill = group_ideology)) +
  theme_bw() +
  scale_fill_manual(values = c( "#F21A0080", "#3B9AB280")) +
  facet_wrap(~ group_ideology) +
  theme_bw() +
  labs( y = "Probability", x = "Median Inter-event",
        title = "Plot no.3.: Distribution of the ratio duration/rest_time by Ideological Prefferences (log)") +
  geom_vline(data=filter(plot_rest_time, group_ideology=="Progressives"), aes(xintercept=median_progre), 
             colour="grey", linetype="dashed", linewidth = 0.8) + 
  geom_vline(data=filter(plot_rest_time, group_ideology=="Conservatives"), aes(xintercept=median_conserva), 
             colour="grey", linetype="dashed", linewidth = 0.8) + 
  geom_text(
    data    = dat_text,
    mapping = aes(x = 2.6, y = 0.25, label = label),
    inherit.aes = FALSE,
    hjust   = -0.1,
    vjust   = -1
  ) +
  theme(legend.position = "none")
p1

exp(2.05)
exp(2.1)

# Save files
png(filename = "C:/Users/User/Documents/GitHub/ideology_internet_consumption/05_plots/Distribution of the ratio time by Ideological Prefferences.png",
    width = 15, height = 10, units = "in", res = 300)
print(p1)
dev.off()


# Plot no.4. --------------------------------------------------------------

# Calculate the median of the 'median_logplus1_ratio' variable for Progressive (ideology 1 and 2) and Conservative groups (ideology 6 and 7)

median_progre <- median(join_flow_argentina_wave_ratio[join_flow_argentina_wave_ratio$ideology %in% c(1,2), "median_logplus1_ratio" ], na.rm = T)
median_conserva <- median(join_flow_argentina_wave_ratio[join_flow_argentina_wave_ratio$ideology %in% c(6,7), "median_logplus1_ratio" ], na.rm = T)

# Calculate the variance of the 'median_logplus1_ratio' for the same groups

var(join_flow_argentina_wave_ratio[join_flow_argentina_wave_ratio$ideology %in% c(1,2), "median_logplus1_ratio" ], na.rm = T)
var(join_flow_argentina_wave_ratio[join_flow_argentina_wave_ratio$ideology %in% c(6,7), "median_logplus1_ratio" ], na.rm = T)

# Create a data frame to hold text labels for plotting

dat_text <- data.frame(
  group_ideology = c("Progressives", "Conservatives"),
  label   = c(paste0("Median: ", round(median_progre,2)), paste0("Median: ",round(median_conserva, 2))
  )
)

# Filter the data to only include specific ideologies (1,2,6,7) and categorize them into "Progressives" and "Conservatives"

plot_rest_time <- join_flow_argentina_wave_ratio %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))

# Update age groups with more descriptive labels
plot_rest_time <- plot_rest_time %>% mutate(age_group = case_when(
  age_group == 20 ~ "Between 18 and 25",
  age_group == 32 ~ "Between 26 and 35",
  age_group == 41 ~ "Between 36 and 45",
  age_group == 50 ~ "Between 46 and 55",
  age_group == 59 ~ "Between 56 and 65",
  age_group == 68 ~ "Older than 65"
  
))
# Calculate median for each group and age

median_data <- plot_rest_time %>% 
  group_by(group_ideology, age_group) %>% 
  summarise(median_x = median(median_logplus1_ratio, na.rm = T)) %>% 
  ungroup()
median_data <-as.data.frame(median_data)

# Create the density plot using ggplot2
plot_rest_time %>%
  ggplot() +
  geom_density(aes( x= median_logplus1_ratio, fill = group_ideology)) +
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
        title = "Plot no.4.: Distribution of the rest time by Ideological Prefferences and age (log)") +
  geom_vline(median_data,mapping =aes(xintercept = median_x-0.2),colour="grey", linetype="dashed", size = 0.8) +
  theme(legend.position = "none")



# Plot no.A2.6. -----------------------------------------------------------

# Filter the data to only include specific ideologies (1,2,6,7) and categorize them into "Progressives" and "Conservatives", and "Male", "Female", "Other"
plot_rest_time <- join_flow_argentina_wave_ratio %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"),
         gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female",
                            gender == 4 ~"Other"))

# Calculate median for each group and gender
median_data <- plot_rest_time %>% 
  group_by(group_ideology, gender) %>% 
  summarise(median_x = median(median_logplus1_ratio, na.rm = T)) %>% 
  ungroup()

# Plot
plot_rest_time %>%
  ggplot() +
  geom_density(aes( x= median_logplus1_ratio, fill = group_ideology)) +
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
        title = "Plot no.A2.6.: Distribution of the rest time by Ideological Prefferences and gender (log)") +
  geom_vline(median_data,mapping =aes(xintercept = median_x-0.2),colour="grey", linetype="dashed", size = 0.8) +
  theme(legend.position = "none")




# Plot no.A2.7. -----------------------------------------------------------

#install.packages("plot3D")

library("plot3D")
# Filter the data to only include specific ideologies (1,2,6,7) and categorize them into "Progressives" and "Conservatives"
plot_rest_time <- join_flow_argentina_wave_ratio %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))

# Extract columns 'ideology', 'age_group', and 'median_logplus1_ratio' from plot_rest_time DataFrame into vectors x, y, z
x <- plot_rest_time$ideology
y <-plot_rest_time$age_group
z <-plot_rest_time$median_logplus1_ratio

# Create a 3D scatter plot
scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,bty = "g",
          xlab = "Ideology",  ticktype = "detailed", #type = "h", 
          ylab ="Age", zlab = "Median Rest time (log)",
          main = "Plot no.A2.7.: 3D Scatter Plot Analysis of Ideology, Age and Rest Time")



# Plot A2.8. --------------------------------------------------------------

# Fit a linear model predicting 'z' (Median Rest Time) based on 'x' (Ideology) and 'y' (Age)
fit <- lm(z ~ x + y)

# Prepare data for the surface (fitting plane)
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid( x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)

# Actual fitted points from the linear model
fitpoints <- predict(fit)

# Create another 3D scatter plot but with the fitting plane

scatter3D(x, y, z, pch = 18,  theta = 1, phi = 15,bty = "g",
          xlab = "Ideology",  ticktype = "detailed", #type = "h", 
          ylab ="Age", zlab = "Median Rest time (log)",
          surf = list(x = x.pred, y = y.pred, z = z.pred,  
                      facets = NA, fit = fitpoints),
          main = "Plot no.A2.8.: 3D Scatter Plot Analysis of Ideology, Age and Rest Time")
