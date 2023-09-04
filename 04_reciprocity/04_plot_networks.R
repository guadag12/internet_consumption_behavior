#################################################################
## GV993 - Dissertation in MSc Social Data Science              #
##                                                              #
## Plot Networks                                                #
#################################################################



# Packages -----------------------------------------------------------------
library(igraph)
library(tidyverse)
library(readr)
rm(list = ls())
options(scipen = 999)



# Databases ---------------------------------------------------------------

read_wave <- read_csv("~/GitHub/ideology_consumption_network/01_data/read_wave.csv")
read_wave <- as.data.frame(read_wave)

read_wave <- read_wave %>% rename(age_group=Edad, education = Educacion, gender=Genero, dummy_working=QSDTrabaja, state=Provincia, people_home=  personasHogar) 

data_network <- read.csv("C:/Users/User/Documents/GitHub/ideology_internet_consumption/01_data/flow_interaction_argentina_new_classification.csv")
data_network <- data_network %>% left_join(read_wave[,c("panelist_id", "age_group", "education", "gender", "dummy_working", "state", "people_home")])



# Transformation of the data ----------------------------------------------

# Subset the original data to include only ideology values 1 and 2
data_prueba<- data_network[data_network$ideology %in% c(1,2), ]

# Filter out rows where rest_time is zero or less
data_prueba <- data_prueba %>% filter(rest_time > 0)

# Group data by source and target and calculate new features
data_prueba<- data_prueba %>% group_by(source, target) %>%
  mutate(frequency = n(),
         weight = frequency * (1 / time_interevent ),
         weight_log = log(weight)
  )
# Convert the data to a data frame
data_prueba <- as.data.frame(data_prueba)

# Create a new dataset with additional features
dataset <- data_prueba %>%
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
         weight_log = log(weight+1)*1.2) %>%
  ungroup() %>%
  group_by(source, target) %>%
  summarise(weight = ifelse(source ==  target,0 ,log(sum(weight)+1)/2))

# Convert to a data frame and remove rows with NA values in rownames
dataset <- as.data.frame(dataset)
dataset <- dataset[!grepl("^NA", rownames(dataset)),]

# Create a subset 'd' containing only 'source', 'target', and 'weight' columns
d<- dataset %>%
  select(source, target,weight)

# Save the histogram of the 'weight' column as a JPEG file

jpeg(file="D:/Guada/03_y3_local_coefficient/saving_plot1.jpeg")
hist(dataset$weight, col="darkgreen")
dev.off()



# Plot Progressives ----------------------------------------------------------------

# Create a list of unique IDs from both 'source' and 'target' columns in 'dataset'
unique_ids <- unique(c(dataset$source, dataset$target))

# Extract relevant columns for creating a graph
relations <- dataset[, c("source", "target", "weight")]

# Create a directed graph using igraph's graph_from_data_frame function
g <- graph_from_data_frame(relations, directed=TRUE, vertices=unique_ids)

# Set edge weights; replace weights <= 0 with NA (missing)
E(g)$weight <- ifelse(dataset$weight <= 0, NA, dataset$weight)

# Delete edges with missing weights
g <- delete_edges(g, E(g)[is.na(E(g)$weight)])

# Compute in-degree for each vertex (number of incoming edges, marked as "flechas que entran" in Spanish)
ind<-degree(g, mode="in") #flechas que entran

# Summarize graph properties
summary(g)

# Create vertex labels; label is empty for vertices with in-degree log-transform less than 3.0
my.label<- V(g)$name
my.label[which(log(ind)/3 < 3.0)]<- ""
my.label2<- my.label

# Show unique labels
unique(my.label2)

# Save a histogram of log-transformed in-degrees as a JPEG file
jpeg(file="D:/Guada/03_y3_local_coefficient/saving_plot1.jpeg")
hist(log(ind)/3, col="darkgreen")
dev.off()

# Generate a nicely layout for the graph
l2 <- layout_nicely(g)


# Generate and save a graph visualization as a PDF, with vertex labels
pdf(file = "D:/Guada/03_y3_local_coefficient/plots/Network Plot-Jerarquico Patria_V2_progre_empty_logind_t_change_v2_unique2_cero4_labels.pdf",
    10, 10, pointsize=6, compress=FALSE)
plot.igraph(g,
            vertex.label=my.label2, layout=l2, 
            vertex.size=log(ind)/3, 
            edge.color = "#33879c20",
            edge.size = E(g)$weight,
            vertex.color= "#3B9AB280", vertex.frame.color="#3B9AB280", 
            edge.width= .01, edge.arrow.size=.05, vertex.label.cex=.1,  
            edge.curved=TRUE)
dev.off()


# Generate and save another graph visualization as a PDF, without vertex labels
pdf(file = "D:/Guada/03_y3_local_coefficient/plots/Network Plot-Jerarquico Patria_V2_progre_empty_logind_t_change_v2_unique2_cero4.pdf",
    10, 10, pointsize=6, compress=FALSE)
plot.igraph(g,
            vertex.label=NA, layout=l2, 
            vertex.size=log(ind)/2, 
            vertex.label.color="Black",
            edge.color = "#33879c20",
            edge.size = E(g)$weight,
            vertex.color= "#3B9AB280", vertex.frame.color="#3B9AB280", 
            edge.width= .01, edge.arrow.size=.05, vertex.label.cex=.1,  
            edge.curved=TRUE, vertex.label.dist=rnorm(length(ind),.1,.03))
dev.off()

# Plot Conservatives ------------------------------------------------------

# Clear the workspace to start fresh
rm(list = ls())

# Read and preprocess 'read_wave' dataset
read_wave <- read_csv("~/GitHub/ideology_consumption_network/01_data/read_wave.csv")
read_wave <- as.data.frame(read_wave)

read_wave <- read_wave %>% rename(age_group=Edad, education = Educacion, gender=Genero, dummy_working=QSDTrabaja, state=Provincia, people_home=  personasHogar) 

# Read and join 'data_network' dataset with 'read_wave' on selected columns
data_network <- read.csv("C:/Users/User/Documents/GitHub/ideology_internet_consumption/01_data/flow_interaction_argentina_new_classification.csv")
data_network <- data_network %>% left_join(read_wave[,c("panelist_id", "age_group", "education", "gender", "dummy_working", "state", "people_home")])

# Data filtering and group-wise computations similar to the previous example
data_prueba<- data_network[data_network$ideology %in% c(6,7), ]
data_prueba <- data_prueba %>% filter(rest_time > 0)

data_prueba<- data_prueba %>% group_by(source, target) %>%
  mutate(frequency = n(),
         weight = frequency * (1 / time_interevent ),
         weight_log = log(weight)
  )
data_prueba <- as.data.frame(data_prueba)

# Additional computations on 'dataset' similar to the previous example
dataset <- data_prueba %>%
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
         weight_log = log(weight+1)*1.2) %>%
  ungroup() %>%
  group_by(source, target) %>%
  summarise(weight = ifelse(source ==  target,0 ,log(sum(weight)+1)/2))

dataset <- as.data.frame(dataset)
dataset <- dataset[!grepl("^NA", rownames(dataset)),]


class(dataset)
d<- dataset %>%
  select(source, target,weight)


# Creating a directed graph using igraph package
unique_ids <- unique(c(dataset$source, dataset$target))


relations <- dataset[, c("source", "target", "weight")]

g <- graph_from_data_frame(relations, directed=TRUE, vertices=unique_ids)

E(g)$weight <- ifelse(dataset$weight <= 0, NA, dataset$weight)

# delete edges in the case that it is not repetead behavior, so it is "Missing data":
g <- delete_edges(g, E(g)[is.na(E(g)$weight)])
summary(g)

# Calculate in-degree and label vertices for graph visualization
ind<-degree(g, mode="in") #flechas que entran

my.label<- V(g)$name
my.label[which(log(ind)/3 < 2.5)]<- ""
my.label2<- my.label

# Save a histogram of log-transformed in-degree as JPEG
jpeg(file="D:/Guada/03_y3_local_coefficient/saving_plot1.jpeg")
hist(log(ind)/3, col="darkgreen")
dev.off()

# Generate and save graph visualizations as PDFs
l2 <- layout_nicely(g)

# With Labels
pdf(file = "D:/Guada/03_y3_local_coefficient/plots/Network Plot-Jerarquico Patria_V2_conserv_empty_logind_t_change_v2_unique2_cero4_labels.pdf",
    10, 10, pointsize=6, compress=FALSE)
plot.igraph(g,
            vertex.label=my.label2, layout=l2, 
            vertex.size=log(ind)/3, 
            vertex.label.color="Black",
            edge.color = "#8a332920",
            edge.size = E(g)$weight,
            vertex.color= "#a83e3250", vertex.frame.color="#a83e3250", 
            edge.width= .01, edge.arrow.size=.05, vertex.label.cex=.1,  
            edge.curved=TRUE)
dev.off()

# Without Labels
pdf(file = "D:/Guada/03_y3_local_coefficient/plots/Network Plot-Jerarquico Patria_V2_conserv_empty_logind_t_change_v2_unique2_cero4.pdf",
    10, 10, pointsize=6, compress=FALSE)
plot.igraph(g,
            vertex.label=NA, layout=l2, 
            vertex.size=log(ind)/3, 
            vertex.label.color="Black",
            edge.color = "#8a332920",
            edge.size = E(g)$weight,
            vertex.color= "#a83e3250", vertex.frame.color="#a83e3250", 
            edge.width= .01, edge.arrow.size=.05, vertex.label.cex=.1,  
            edge.curved=TRUE, vertex.label.dist=rnorm(length(ind),.1,.03))
dev.off()


# Save datasets for further analysis
edges_more_weight_conservatives <- dataset %>% distinct(source, target, weight)
write.csv(edges_more_weight_conservatives, "C:/Users/User/Documents/GitHub/ideology_internet_consumption/03_y3_local_coefficient/edges_more_weight_conservatives.csv", row.names = F)
data_ind_conserva<- as.data.frame(ind)
data_ind_conserva$nodes <- rownames(data_ind_conserva)
write.csv(data_ind_conserva, "C:/Users/User/Documents/GitHub/ideology_internet_consumption/03_y3_local_coefficient/data_indegree_conservatives.csv", row.names = F)




