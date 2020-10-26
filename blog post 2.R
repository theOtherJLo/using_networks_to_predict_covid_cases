# Feature generation and model creation 

# LA = Local Authority. These are sub regions in the UK
# MSOA = Middle Layer Super Output Area. These are sub regions inside LAs

# Set up ----
# Load packages
library(dplyr)
library(readr) # Loading the data
library(tidyr)
library(sf) # For the maps
library(ggplot2)
library(viridis)
library(cowplot)
library(glue)

# Network packages
library(igraph) # build network
library(spdep) # builds network
library(tidygraph)
library(ggraph) # for plotting networks

# Load mso map data. Downloaded from, and then extact all files into data/maps:
# https://opendata.arcgis.com/datasets/efeadef72f3745df86edc1c146006fc0_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D
msoa_sf <- sf::st_read("data/maps/Middle_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BFE.shp")

# Load msoa case data
# Note we have had to change the URL from the first blog post as it is now saved in a new location.
# I have saved the raw data into the data folder so this analysis can be reproduced exactly. 
url <- 'https://coronavirus.data.gov.uk/downloads/msoa_data/MSOAs_latest.csv'
msoa <- readr::read_csv(url, col_types = readr::cols()) %>%
  select(msoa11_cd, starts_with("wk_")) %>%
  tidyr::pivot_longer(cols = -msoa11_cd, names_to = "week", names_prefix = "wk_",
                      values_to = "cases") %>%
  mutate(week = as.integer(week),
         cases = case_when(is.na(cases) ~ 0,
                           cases < 0 ~ 0,
                           TRUE ~ cases),
         cases = as.integer(cases)) %>% 
  # We also drop all weeks which don't have any cases in them
  group_by(week) %>% 
  filter(!all(cases==0)) %>% 
  ungroup
  
# Network for all of England ----
# In part 1 we showed how to create the network, but we only did this for Leicester. 
# So the first thing we need to do, is recreate the network for all of the UK. 

# We don't want to create the network from scratch every time, so we use this code to only create it
# if we haven't already
if(file.exists("data/msoa_network_object.rds")) {
  msoa_network <- readRDS("data/msoa_network_object.rds")
} else{
  msoa_neighbours <- spdep::poly2nb(msoa_sf)
  
  # Use nb2mat to turn this into an adjacency matrix
  # (We get an error because some MSOAs have no neigbours)
  adj_mat <- spdep::nb2mat(msoa_neighbours, style = "B", zero.policy = TRUE)
  rownames(adj_mat) <- msoa_sf$MSOA11CD
  colnames(adj_mat) <- msoa_sf$MSOA11CD
  
  # Use graph_from_adjacency_matrix to create a graph object from the adjacency matrix
  # Use as_tbl_graph to turn this into a tidygraph
  msoa_network <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected") %>% 
    tidygraph::as_tbl_graph()
  
  # Like in part 1, we can join on extra data to the nodes or edge tibble. 
  msoa_network <- msoa_network %>% 
    activate("nodes") %>% 
    left_join(msoa %>%  select(contains("nm"), msoa11_cd) %>% distinct, 
              by = c("name" = "msoa11_cd"))
  
  # Converting LONG LAT coordinates into Uk grid 
  # (This is only necessary if you want to overlay the network on the map)
  coords <- msoa_sf %>% 
    as.data.frame() %>% 
    select(LONG, LAT) %>% 
    sp::SpatialPoints(proj4string=CRS("+init=epsg:4326")) %>% # LAT LONG code
    sp::spTransform(CRS("+init=epsg:27700")) %>%  # UK grid code
    as.data.frame() %>% 
    bind_cols(MSOA11CD = msoa_sf$MSOA11CD) # to allow us to join on 
  
  
  # Join on cordinates and sf geometry
  msoa_network <- msoa_network %>% 
    activate("nodes") %>% 
    left_join(coords, by = c("name" = "MSOA11CD"))
  
  # We save the network object so that we don't need to run this code again
  saveRDS(msoa_network, "data/msoa_network_object.rds")

}

# Printing this shows that there are 7,201 nodes, 20,946 edges and 
# 4 connected components 
msoa_network


# Network features ----
# In this section, we create features in the netwrork that we can use later for modelling. 
msoa_network %>% 
  as_tibble() %>% 
  head

# However, the real power of networks comes from being able to easily create Network features
# degree tells us how many neighbours each node has
msoa_network <- msoa_network %>% 
  activate("nodes") %>% 
  mutate(degree = igraph::degree(.G()))

msoa_network %>% 
  as_tibble() %>% 
  head

# Plot
# Throughout this blog, we will replot our map of Leicester, but with different 
# features highlighted. 
# We therefore made a function to help us do this
source("R/map_making_functions.R")

leicester_degree_plots <- plot_map_network_feature(msoa_network, msoa_sf, feature = "degree", lad = "Leicester", save = TRUE, combine = TRUE)

# View plots:
leicester_degree_plots$network_plot
leicester_degree_plots$sf_plot


# Create more features: 
msoa_network <- msoa_network %>% 
  activate("nodes") %>% 
  mutate(betweeness = igraph::betweenness(.G()),
         triangles = igraph::count_triangles(.G()),
         transitivity = igraph::transitivity(.G(), type = "local"))

features <- c("betweeness", "triangles", "transitivity")

# We don't combine these so we can just choose the one that best represents what we are trying to show.
# Both the maps will be saved 
leicester_plots <- lapply(features, function(feature) plot_map_network_feature(msoa_network, msoa_sf, 
                                                                               feature = feature, 
                                                                               lad = "Leicester", save = TRUE, combine = TRUE))

# Features based on cases ----
# So far we have just created features based on the intrinsic structure of the network. 
# While motivating the use of networks, we saw that cases in neighbouring MSOAs seem to 
# influence eachother. We can try and capture this by creating newtwork features based 
# on Covid cases 

# A simple feature of this type is just "what is the average number of cases in neighbouring
# MSOAs?" 

# In order to create features like this, we need to join on the cases data. 
# Let's start with week 25 to see this in action

# First we need to derive some lagged variables which we can use as our predictor v
# variables (i.e. how many cases an MSOA has next week)

msoa_features <- msoa %>% 
  # mutate(cases = ifelse(cases < 0, NA_integer_, cases)) %>% # deal with -99 (did this at the start)
  arrange(msoa11_cd, week) %>% 
  group_by(msoa11_cd) %>% 
  mutate(cases_next_week = lead(cases)) %>% 
  ungroup %>% 
  select(msoa11_cd, week, cases, cases_next_week)

msoa_cases <- msoa_network %>%  
  activate("nodes") %>% # The nodes are already acitvated, but I thought it was useful to include here to highlight its use
  left_join(msoa_features %>% filter(week == 25), by = c("name" = "msoa11_cd"))

# Let's look at this for Leicester
cases_week_25 <- plot_map_network_feature(msoa_cases, msoa_sf, feature = "cases", lad = "Leicester", save = TRUE, combine = TRUE)
cases_week_26 <- plot_map_network_feature(msoa_cases, msoa_sf, feature = "cases_next_week", lad = "Leicester", save = TRUE, combine = TRUE)

# In order to easily create features based on neighbours, we can use the adacency matrix. 
# We have defined this earlier (in order to create the network) but it is easy to create 
# one using igraph::get.adjacency
adj_matrix <- igraph::get.adjacency(msoa_cases)

# Let's create a feature which is "average cases in neighbours"
# .N()$cases is how we get the vector which says how many cases there are for each MSOA
msoa_cases <- msoa_cases %>% 
  mutate(cases_in_neighbourhood = as.vector(adj_matrix %*% .N()$cases),
         cases_in_neighbourhood_average = cases_in_neighbourhood/degree)


# Let's see what this looks like
# We rename the feature to improve the plot
average_cases_neighbourhood <- plot_map_network_feature(msoa_cases %>% rename(`cases nb` = cases_in_neighbourhood_average), 
                                                        msoa_sf, feature = "cases nb", lad = "Leicester", save = TRUE, combine = TRUE)


# Creating data for predictions ----
# We now set up some code to generate these features for every week, 
# extract the feature tibble and then bind them all together to get 
# the dataset we can use for our modelling
data_for_predictions <- lapply(msoa_features %>%  distinct(week) %>% pull(week), 
                                function(wk){
                                  
                                  message(paste0("Creating features for week ", wk))
                                  
                                  # Filter to the correct week
                                  weekly_data <- msoa_features %>% 
                                    filter(week == wk)
                                  
                                  # join onto the network and create network features 
                                  weekly_network <- msoa_network %>%  
                                    activate("nodes") %>% # The nodes are already acitvated, but I thought it was useful to include here to highlight its use
                                    left_join(weekly_data, by = c("name" = "msoa11_cd")) %>% 
                                    mutate(cases_in_neighbourhood = as.vector(adj_matrix %*% .N()$cases),
                                           cases_in_neighbourhood_average = cases_in_neighbourhood/degree)
                                  
                                  # Extract table
                                  weekly_network %>% 
                                    as_tibble()
                                  
                                }) %>% 
  bind_rows


# Let's see all the data in there about a specific MSOA in Leicester 
data_for_predictions %>% 
  filter(lad19_nm == "Leicester") %>%
  mutate(max_cases = max(cases, na.rm = T)) %>% 
  # Filter to the MSOA who ahd the max cases
  group_by(name) %>% 
  filter(max(cases, na.rm = T) == max_cases) %>% 
  ungroup %>% 
  filter(week >= 20) %>% 
  select(name, week, degree:transitivity, starts_with("cases"), -cases_next_week, cases_next_week)



# Creating predictions ----
# Hopefully you can see how this "data for predictions" table is a very recognisable dataset 
# for predictive modelling, where the outcome variable is "cases_next_week"

# This blog post doesn't delve into this with two much detail, but just to show what this might 
# look like, we create a linear model to find the linear relationship between all of the variables 
# and the outcome variable 

# Just so we can see how such a model performs, we will still split the data into test and training 
# data. We will do this by splitting the weeks so we don't have any data leakage. 
processed_df <- data_for_predictions %>% 
  select(name, week, degree:cases_in_neighbourhood_average) %>% 
  drop_na()  # This is a bit of a cheat to get a results quickly
  
training_threshold <- 0.7 
training_weeks <- processed_df %>% 
  distinct(week) %>% 
  slice(1:round(nrow(.) * training_threshold)) %>% 
  pull()

train_df <- processed_df %>% 
  filter(week %in% training_weeks)

test_df <- processed_df %>% 
  filter(!week %in% training_weeks)


# Create model on training data 
linear_model <- lm(cases_next_week ~ .,
                   data = train_df %>% select(-name, -week))

summary(linear_model)

test_pred <- stats::predict(linear_model, 
                            test_df %>%  select(-name, -week, -cases_next_week))

pred.obs <- tibble(pred = test_pred,
                   obs = test_df$cases_next_week)

rmse <- pred.obs %>% 
  summarise(sqrt(mean((obs-pred)^2)))

eval_df <- test_df %>% 
  bind_cols(tibble(pred = test_pred)) %>% 
  arrange(desc(pred)) %>% 
  select(cases, cases_next_week, pred, everything())

# baseline
# If we just use current cases to predict next week cases, what is the RMSE?
baseline.pred.obs <- tibble(pred = test_df$cases,
                            obs = test_df$cases_next_week)

rmse <- baseline.pred.obs %>% 
  summarise(sqrt(mean((obs-pred)^2)))


# Visualising model outputs on a map
# We can use our previous function to do this
results_network <- msoa_network %>% 
  left_join(eval_df %>% filter(week == 40) %>% select(name, `predicted cases` = pred, `actual cases` = cases_next_week))

plot_map_network_feature(results_network, 
                         msoa_sf, feature = "model pred", lad = "Leicester", save = TRUE, combine = TRUE)

plot_map_network_feature(results_network, 
                         msoa_sf, feature = "actual cases", lad = "Leicester", save = TRUE, combine = TRUE)

# i want to do a custom plot here, which shows the actual and predicted cases side by side with the same scale. 
# This is the code for that:

# We find the max value so we can scale both our graphs equally 
max_scale <- results_network %>% 
  as_tibble() %>%  
  filter(lad19_nm == "Leicester") %>% 
  select(`predicted cases`, `actual cases`) %>% 
  max(na.rm = T) %>% 
  round(-1)

lapply(list("predicted cases", "actual cases"), function(feature){
  network <- results_network %>% 
    activate("nodes") %>% 
    mutate(network_feature = get(feature)) %>% 
    filter(lad19_nm == "Leicester")
  
  sf <- msoa_sf %>% 
    inner_join(network %>% as_tibble(), 
               by = c("MSOA11CD" = "name"))
  
  sf_plot <- ggplot(sf, aes(fill = network_feature)) +
    geom_sf() + 
    theme_void() +
    scale_fill_viridis_c(limits = c(0, max_scale)) +
    ggtitle(glue::glue("MSOA map for Leicester with fill based on '{feature}'")) +
    guides(fill=guide_legend(title="Cases", reverse = TRUE))
  
  # Save 
  ggsave(sf_plot, file = glue("outputs/network features/special sf plot of {feature} for Leicester.png"))
})

# Things to try out ----
# There are lots of changes you can try to this process. Some ideas are below:
  # Change the outcome variable to "change in covid cases" or a categorical variable for over 
  # a certain threshold 
  #
  # Create edge weights based on distance, and use this to create weighted edges and features based
  # off of the weighted edges
  # 
  # Try more advanced modelling techniques 
  # 
  # Create more features