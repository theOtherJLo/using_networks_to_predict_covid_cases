# Functions to help us plot maps



# This function helps us plot a map of an LA, where the colour of the
# node or polygon is based on a node feature. We create this function so
# we can make these plots easily for a variety of features
plot_map_network_feature <- function(msoa_network, msoa_sf, feature, lad = "Leicester", save = FALSE, combine = TRUE){
  
  network <- msoa_network %>% 
    activate("nodes") %>% 
    mutate(network_feature = get(feature)) %>% 
    filter(lad19_nm == lad)
  
  sf <- msoa_sf %>% 
    inner_join(network %>% as_tibble(), 
               by = c("MSOA11CD" = "name"))
  
  network_plot <- ggraph(network, layout = "manual", x = LONG, y = LAT)  +
    geom_sf(data = sf, fill = "white", color = "grey") +
    theme_void() +
    geom_edge_link(colour = 'black', width = 1) + 
    geom_node_point(aes(color = network_feature), size = 4) +
    scale_color_viridis_c() +
    ggtitle(glue::glue("MSOA network for {lad} with node size and colour based on '{feature}'")) +
    guides(color=guide_legend(title=feature, reverse = TRUE))
  
  sf_plot <- ggplot(sf, aes(fill = network_feature)) +
    geom_sf() + 
    theme_void() +
    scale_fill_viridis_c() +
    ggtitle(glue::glue("MSOA map for {lad} with fill based on '{feature}'")) +
    guides(fill=guide_legend(title=feature, reverse = TRUE))
  
  if(combine){
    # We use cowplot to combine the plots into one
    # We create a single over arching title
    title <- cowplot::ggdraw() + 
      cowplot::draw_label(
        glue("Visualisations of '{feature}' for {lad}"),
        fontface = 'bold',
        x = 0,
        hjust = 0
      ) +
      theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(0, 0, 0, 7)
      )
    
    plot_row <- cowplot::plot_grid(network_plot + ggtitle(""), 
                                   sf_plot  + ggtitle(""), ncol=2)
    combined <- cowplot::plot_grid(
      title, plot_row,
      ncol = 1,
      # rel_heights values control vertical title margins
      rel_heights = c(0.1, 1)
    )
    
    if(save) ggsave(combined, file = glue("outputs/network features/combined plot of {feature} for {lad}.png"))
    
  } else{
    # if save is true, and combined FALSE then we save the objects too
    if(save){
      ggsave(network_plot, file = glue("outputs/network features/network plot of {feature} for {lad}.png"))
      ggsave(sf_plot, file = glue("outputs/network features/sf plot of {feature} for {lad}.png"))
    }
  
  }
  
  # return both these objects in a list
  list(network_plot = network_plot,
       sf_plot = sf_plot)
  
}
