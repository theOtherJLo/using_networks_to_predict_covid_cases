# Creating an animated map of MSOAs within an LA to show changing Covid cases over time

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
library(gganimate) # For the animated map

# Network packages
library(igraph) # build network
library(spdep) # builds network
library(tidygraph)
library(ggraph) # for plotting networks

# Load mso map data. Downloaded from, and then extact all files into data/maps:
# https://opendata.arcgis.com/datasets/efeadef72f3745df86edc1c146006fc0_0.zip?outSR=%7B%22latestWkid%22%3A27700%2C%22wkid%22%3A27700%7D
msoa_sf <- sf::st_read("data/maps/Middle_Layer_Super_Output_Areas__December_2011__Boundaries_EW_BFE.shp")

# Load msoa case data
msoa <- readr::read_csv('https://c19downloads.azureedge.net/downloads/msoa_data/MSOAs_latest.csv', 
                col_types = cols()) %>% 
  # Drop columns that are all NA
  select(where(~!all(is.na(.)))) %>% 
  # Pivot longer
  pivot_longer(dplyr::starts_with("wk_"), names_to = "week", values_to = "cases", names_prefix = "wk_") %>% 
  # Turn week to numeric
  mutate(week = as.integer(week)) %>% 
  # Turn NAs into 0s
  mutate(across(c(last_7_days, cases), .fns = ~ifelse(is.na(.), 0, .)))

# Which LA has had the most cases in a week? ----
la <- msoa %>% 
  group_by(lad19_nm, week) %>% 
  summarise(cases = sum(cases))

la %>% 
  arrange(desc(cases)) %>% 
  head(100) %>% 
  View

# Let's focus on Leicester, as it has had a high number in more recent weeks

# Let's see a time series
la %>% 
  filter(lad19_nm == "Leicester") %>% 
  ggplot(aes(x = week, y = cases)) +
  geom_line(size = 1) +
  theme_minimal() +
  ggtitle("Time series gaph of cases for Leicester")

ggsave(file = "outputs/Leicester cases.png")

# How does this look if we break it down my MSOA within Leicster (just top 10)?
msoa %>% 
  filter(lad19_nm == "Leicester") %>% 
  group_by(msoa11_hclnm) %>% 
  summarise(max_cases = max(cases)) %>% 
  arrange(desc(max_cases)) %>% 
  slice(1:10) %>% 
  inner_join(msoa) %>% 
  mutate(msoa11_hclnm = factor(msoa11_hclnm) %>% reorder(desc(max_cases))) %>% 
  ggplot(aes(x = week, y = cases, group = msoa11_hclnm, color = msoa11_hclnm)) +
  geom_line(size = 1) +
  scale_color_viridis(discrete = TRUE) +
  theme_minimal() +
  ggtitle("Time series gaph of cases for MSOAs within Leicester") +
  guides(color=guide_legend(title="MSOA"))

ggsave("outputs/Leciester MSOA cases.png")

# Maps -----
# First lets create a map for week 27, when the biggest peak was
msoa_data_wk25 <- msoa %>% 
  filter(lad19_nm == "Leicester", 
         week == 25) 

# We see that covid cases are clustered in several MSOAs
msoa_sf %>% 
  inner_join(msoa_data_wk25 %>% distinct(msoa11_cd, cases), by = c("MSOA11CD" = "msoa11_cd")) %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = cases)) +
  scale_fill_viridis_c() + # you can use direction = -1 to switch the colours, but I actually think lighter = more cases looks better
  theme_void() +
  ggtitle(label = "MSOA map for Leicester",
          subtitle = "The colour shows Covid cases during week 25")

ggsave(file = "outputs/maps/leicester_week_25.png")

# gganimate 
# Need to install transformr (from https://github.com/thomasp85/transformr or CRAN)
p <- msoa_sf %>% 
  inner_join(msoa %>% filter(lad19_nm == "Leicester"),
             by = c("MSOA11CD" = "msoa11_cd")) %>% 
  ggplot(aes(group = week)) +
  geom_sf(aes(fill = cases)) +
  scale_fill_viridis_c() +
  transition_time(week) +
  labs(title = paste0("New covid cases for MSOAs within Leicester"),
       subtitle = "Week = {frame_time}") +
  theme_void() 

# We define this to help us pick the right number of frames
num_weeks <- n_distinct(msoa$week)

animate(p, nframes = num_weeks, fps = 1, end_pause = 4)

# Save  
anim_save(file = "outputs/maps/animated_map_leicester.gif")




# Network ---- 
# Now we turn our data into a network.This blog post was really useful in doing this:
# https://mikeyharper.uk/calculating-neighbouring-polygons-in-r/

# First we need to extract Neighbours from our geo data (takes a while on all of England, so we will start with just Leicester)
leicester_sf <- msoa_sf %>% 
  inner_join(msoa %>% filter(lad19_nm == "Leicester", week == 27) , 
             by = c("MSOA11CD" = "msoa11_cd"))

# Use the poly2nb to get the neighbourhood of each area
leicester_msoa_neighbours <- spdep::poly2nb(leicester_sf)

# Use nb2mat to turn this into an adjacency matrix
adj_mat <- spdep::nb2mat(leicester_msoa_neighbours, style = "B")
rownames(adj_mat) <- leicester_sf$msoa11_hclnm
colnames(adj_mat) <- leicester_sf$msoa11_hclnm

# Use graph_from_adjacency_matrix to create a graph object from the adjacency matrix
# Use as_tbl_graph to turn this into a tidygraph
leicester_network <- igraph::graph_from_adjacency_matrix(adj_mat, mode = "undirected") %>% 
  tidygraph::as_tbl_graph()

# We can use activate() to see and interact with the tibbles that sit behind the nodes and edgeses.
# We can use mutate, filter and joins like normal
leicester_network %>% 
  activate("nodes") %>% 
  as_tibble()

# We can also plot this using ggraph, but this doesn't match up to the actual geographic locations
ggraph(leicester_network)  +
  geom_edge_link(colour = 'black', width = 2) + 
  geom_node_point(size = 5, colour = 'steelblue') +
  theme_void() +
  ggtitle("Network plot of MSOAs in Leicester", 
          subtitle = "Each node is an MSOA, and an edge joins two nodes if they are neighbours")

ggsave(file = "outputs/maps/Leicester MSOA network simple.png")

# Converting LONG LAT coordinates into Uk grid 
# (This is only necessary if you want to overlay the network on the map)
coords <- leicester_sf %>% 
  as.data.frame() %>% 
  select(LONG, LAT) %>% 
  sp::SpatialPoints(proj4string=CRS("+init=epsg:4326")) %>% # LAT LONG code
  sp::spTransform(CRS("+init=epsg:27700")) %>%  # UK grid code
  as.data.frame() %>% 
  bind_cols(msoa11_hclnm = leicester_sf$msoa11_hclnm) # to allow us to bind on 


# Join on cordinates and sf geometry
leicester_network <- leicester_network %>% 
  activate("nodes") %>% 
  left_join(coords, by = c("name" = "msoa11_hclnm"))

# Show this has worked
leicester_network %>% 
  as_tibble() %>% 
  head

# This lets us plot the network on the same map as our sf using ggraph
ggraph(leicester_network, layout = "manual", x = LONG, y = LAT)  +
  geom_sf(data = leicester_sf, fill = "white") +
  theme_void() +
  geom_edge_link(colour = 'black', width = 2) + 
  geom_node_point(size = 5, colour = 'steelblue') +
  ggtitle("MSOA network overlaying shapefile")

ggsave(file = "outputs/maps/Leicester MSOA network overlaying map.png")
