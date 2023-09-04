#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Reciprocity and Temporal Order Weight                        #
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



# Calculate reciprocity ---------------------------------------------------

# Calculating and storing the reciprocity of relationships for specific panelists
# based on their 'ideology'. Reciprocity in a network measures the extent to which relationships
# between nodes (here, panelists) are mutual.

# Loop over unique panelist IDs from data_network where ideology is either 1, 2, 6, 7 and panelist_id is not 'cd4404487bfbc38b'
for(p in unique(data_network[((data_network$ideology %in% c(1,2,6,7)) &
                              (data_network$panelist_id !="cd4404487bfbc38b")), "panelist_id"])){
  
  # Initialize an empty data frame to store results
  empty <- data.frame(
    "panelist_id"= NA,
    "reciprocity"= NA)
  empty <- empty[0,] # Remove the initial NA row
  
  # Print panelist ID
  message("USER:", p)
  
  # Filter data for the current panelist and only keep rows where 'rest_time' is greater than 0
  data_prueba <- data_network[data_network$panelist_id == p, ] %>% filter(rest_time > 0)
  
  # Group data by 'source' and 'target', then calculate frequency, weight, and its logarithm
  data_prueba <- data_prueba %>% group_by(source, target) %>%
    mutate(frequency = n(),
           weight = frequency * (1 / time_interevent),
           weight_log = log(weight))
  
  # Count the number of relationships by 'source' and 'target'
  d <- data_prueba %>%  group_by(source, target) %>% count()
  
  # Combine 'source' and 'target' into a single set of unique actors
  d <- rbind(d[, "source"], d[, "target"])
  d$source <- ifelse(is.na(d$source), d$target, d$source)
  d$target <- ifelse(is.na(d$target), d$source, d$target)
  actors <- data.frame(id = unique(d$source))
  
  # Create a directed graph object from 'source' and 'target'
  relations <- data_prueba[, c("source", "target")]
  g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
  
  # Calculate reciprocity of the graph
  user_reciprocity <- reciprocity(g)
  
  # Store panelist_id and calculated reciprocity in a new data frame
  data_recipro <- data.frame("panelist_id" = p,
                             "reciprocity" = user_reciprocity)
  
  # Add the calculated reciprocity to the 'empty' data frame
  empty <- rbind(data_recipro, empty)
  
  # Uncomment to save results to CSV
  # write.csv(empty, paste0("path_to_save/data_reciprocity_", p, ".csv"), row.names = FALSE)
  
  # Print completion message for the current panelist
  message("ready", p)
}

# Calculate temporal order weight -------------------------------------------------------------

# Filter 'data_network' based on certain ideology values and rest_time greater than 0
data_prueba <- data_network[data_network$ideology %in% c(1,2,6,7), ]
data_prueba <- data_prueba %>% filter(rest_time > 0)

# Group by source and target, then calculate frequency, weight, and weight_log
data_prueba <- data_prueba %>% group_by(source, target) %>%
  mutate(frequency = n(),
         weight = frequency * (1 / time_interevent),
         weight_log = log(weight))

# Convert to a regular data frame
data_prueba <- as.data.frame(data_prueba)

# Sort, group, and create new variables 'condition' and 'time_difference'
dataset <- data_prueba %>%
  arrange(panelist_id, datetime_first) %>%
  group_by(panelist_id) %>%
  mutate(
    condition = case_when(
      (lag(source, order_by = datetime_first) == target) & (source != target) ~ 1,
      (lag(source, order_by = datetime_first) == target) & (source == target) ~ 0,
      T ~ 0),
    time_difference = case_when(
      (lag(source, order_by = datetime_first) == target) & (source != target) ~ (amount_seconds_first+rest_time)+1,
      (lag(source, order_by = datetime_first) == target) & (source == target) ~ 0,
      T ~ 0)
  ) %>%
  mutate(weight = condition * (1 / time_difference),
         weight = weight * 2 + 0.3,
         weight = (ifelse(is.na(weight), 0, weight)) * 2,
         weight = ifelse(weight == 0, 0, weight),
         weight_log = log(weight + 1) * 1.2)

# Summarize weights by 'panelist_id', 'source', and 'target'
dataset <- dataset %>%
  ungroup() %>%
  group_by(panelist_id, source, target) %>%
  summarise(weight = ifelse(source == target, 0, log(sum(weight) + 1) / 2))

# Convert to a regular data frame and remove rows with NA in rownames
dataset <- as.data.frame(dataset)
dataset <- dataset[!grepl("^NA", rownames(dataset)),]

# Remove duplicate rows
unique_rows <- !duplicated(dataset[, c("panelist_id", "source", "target", "weight")])
dataset <- dataset[unique_rows,]

# Count the number of interactions per panelist
d_count <- dataset %>%
  group_by(panelist_id) %>% 
  summarise(count = n())

# Count the number of interactions with non-zero weight per panelist
d_condition <- dataset %>%
  filter(weight > 0.0) %>%
  group_by(panelist_id) %>% 
  summarise(count_condition_yes = n())

# Calculate the proportion of non-zero weight interactions
d_count <- d_count %>% 
  left_join(d_condition) %>%
  group_by(panelist_id) %>% 
  summarise(proportion = count_condition_yes / count)

# Join with other attributes like age, education, etc.
d_count <- d_count %>% left_join(read_wave[, c("panelist_id", "age_group", "ideology", "education", "gender", "dummy_working", "state", "people_home")]) 

# Final dataset for plotting or further analysis
plot_reciprocity <- d_count



# Plot no.8.  -------------------------------------------------------------------

# Calculate the median weight of interactions for each panelist
d_median_weight <- dataset %>%
  filter(weight > 0.0) %>%
  group_by(panelist_id) %>% 
  summarise(median_user = median(weight))

# Join the dataset with additional panelist information
d_median_weight <- d_median_weight %>% left_join(read_wave[, c("panelist_id", "age_group", "ideology","education", "gender",  "dummy_working", "state", "people_home")]) 

# Rename for future use
plot_reciprocity <- d_median_weight 

# Filter the data by specific ideologies and label them as either "Progressives" or "Conservatives"
plot_rest_time <- plot_reciprocity %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))

# Convert to a data frame
plot_rest_time <- as.data.frame(plot_rest_time)

# Calculate the median for both Progressives and Conservatives
median_progre <- median(plot_rest_time[plot_rest_time$ideology %in% c(1,2), "median_user" ], na.rm = T)
median_conserva <- median(plot_rest_time[plot_rest_time$ideology %in% c(6,7), "median_user" ], na.rm = T)

# Prepare text labels for the plot
dat_text <- data.frame(
  group_ideology = c("Progressives", "Conservatives"),
  label = c(paste0("Median: ", round(median_progre, 2)), paste0("Median: ", round(median_conserva, 2)))
)

# Generate the density plot
p1 <- plot_rest_time %>%
  ggplot() +
  geom_density(aes(x = median_user, fill = group_ideology)) +
  theme_bw() +
  scale_fill_manual(values = c("#F21A0080", "#3B9AB280")) +
  facet_wrap(~ group_ideology) +
  labs(y = "Probability", x = "New measure", 
       title = "Plot no.8.: Distribution of the median of new measure by Ideological Prefferences") +
  geom_vline(data = filter(plot_rest_time, group_ideology == "Progressives"), aes(xintercept = median_progre), 
             colour = "grey", linetype = "dashed", linewidth = 0.8) + 
  geom_vline(data = filter(plot_rest_time, group_ideology == "Conservatives"), aes(xintercept = median_conserva), 
             colour = "grey", linetype = "dashed", linewidth = 0.8) + 
  geom_text(data = dat_text, mapping = aes(x = 0.3, y = 0.25, label = label),
            inherit.aes = FALSE, hjust = -0.5, vjust = -0.5) +
  theme(legend.position = "none")

# Display the plot
p1

# Plot no.9. --------------------------------------------------------------------

plot_rest_time <- plot_reciprocity %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))
plot_rest_time <- as.data.frame(plot_rest_time)


median_progre <- median(plot_rest_time[plot_rest_time$ideology %in% c(1,2), "proportion" ], na.rm = T)
median_conserva <- median(plot_rest_time[plot_rest_time$ideology %in% c(6,7), "proportion" ], na.rm = T)

dat_text <- data.frame(
  group_ideology = c("Progressives", "Conservatives"),
  label   = c(paste0("Median: ", round(median_progre,2)), paste0("Median: ",round(median_conserva, 2))
  )
)

p1 <- plot_rest_time %>%
  ggplot() +
  geom_density(aes( x= proportion, fill = group_ideology)) +
  theme_bw() +
  scale_fill_manual(values = c( "#F21A0080", "#3B9AB280")) +
  facet_wrap(~ group_ideology) +
  theme_bw() +
  labs( y = "Probability", x = "Proportion",
        title = "Plot no.9.: Distribution of the proportion of cases with repetead behavior by Ideological Prefferences") +
  geom_vline(data=filter(plot_rest_time, group_ideology=="Progressives"), aes(xintercept=median_progre), 
             colour="grey", linetype="dashed", linewidth = 0.8) + 
  geom_vline(data=filter(plot_rest_time, group_ideology=="Conservatives"), aes(xintercept=median_conserva), 
             colour="grey", linetype="dashed", linewidth = 0.8) + 
  geom_text(
    data    = dat_text,
    mapping = aes(x = 0.6, y = 0.25, label = label),
    inherit.aes = FALSE,
    hjust   = 2,
    vjust   = -1
  ) +
  theme(legend.position = "none")
p1



# Plot no.12. -------------------------------------------------------------------


# Filter data by ideologies (1,2,6,7) and label them as "Progressives" or "Conservatives"
plot_rest_time <- plot_reciprocity %>%
  filter(ideology %in% c(1,2,6,7)) %>%
  mutate(group_ideology = ifelse(ideology %in% c(1,2), "Progressives", "Conservatives"))

# Convert to a data frame
plot_rest_time <- as.data.frame(plot_rest_time)

# Calculate the median proportion for both Progressives and Conservatives
median_progre <- median(plot_rest_time[plot_rest_time$ideology %in% c(1,2), "proportion"], na.rm = TRUE)
median_conserva <- median(plot_rest_time[plot_rest_time$ideology %in% c(6,7), "proportion"], na.rm = TRUE)

# Create a data frame for text labels showing the median proportions
dat_text <- data.frame(
  group_ideology = c("Progressives", "Conservatives"),
  label = c(paste0("Median: ", round(median_progre, 2)), paste0("Median: ", round(median_conserva, 2)))
)

# Create the density plot
p1 <- plot_rest_time %>%
  ggplot() +
  geom_density(aes(x = proportion, fill = group_ideology)) +  # Density plot
  theme_bw() +
  scale_fill_manual(values = c("#F21A0080", "#3B9AB280")) +  # Manual color scale
  facet_wrap(~ group_ideology) +  # Separate plot for each ideological group
  labs(y = "Probability", x = "Proportion", 
       title = "Plot no.9.: Distribution of the proportion of cases with repeated behavior by Ideological Preferences") +
  geom_vline(data = filter(plot_rest_time, group_ideology == "Progressives"), aes(xintercept = median_progre), 
             colour = "grey", linetype = "dashed", linewidth = 0.8) +  # Median line for Progressives
  geom_vline(data = filter(plot_rest_time, group_ideology == "Conservatives"), aes(xintercept = median_conserva), 
             colour = "grey", linetype = "dashed", linewidth = 0.8) +  # Median line for Conservatives
  geom_text(data = dat_text, mapping = aes(x = 0.6, y = 0.25, label = label),
            inherit.aes = FALSE, hjust = 2, vjust = -1) +  # Text labels for medians
  theme(legend.position = "none")  # Hide legend

# Display the plot
p1



# Plot no.A2.4. -------------------------------------------------------------

# Load the plot3D package
library("plot3D")

# Display the first few rows of plot_rest_time dataset
head(plot_rest_time)

# Assign columns to variables for easier access
x <- plot_rest_time$ideology
y <- plot_rest_time$age_group
z <- plot_rest_time$time_difference_log

# Create the initial 3D scatter plot
scatter3D(x, y, z, pch = 18, theta = 20, phi = 20, bty = "g",
          xlab = "Ideology", ticktype = "detailed",
          ylab = "Age", zlab = "Median Rest time (log)",
          main = "Plot no.A2.3.: 3D Scatter Plot Analysis of Ideology, Age and Rest Time")

# Fit a linear model to the data
fit <- lm(z ~ x + y)

# Generate prediction grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
xy <- expand.grid(x = x.pred, y = y.pred)
z.pred <- matrix(predict(fit, newdata = xy),
                 nrow = grid.lines, ncol = grid.lines)
fitpoints <- predict(fit)

# Create the 3D scatter plot with fitted surface
scatter3D(x, y, z, pch = 18, theta = 1, phi = 15, bty = "g",
          xlab = "Ideology", ticktype = "detailed",
          ylab = "Age", zlab = "Median Rest time (log)",
          surf = list(x = x.pred, y = y.pred, z = z.pred,
                      facets = NA, fit = fitpoints),
          main = "Plot no.A2.4.: 3D Scatter Plot Analysis of Ideology, Age and Rest Time")
