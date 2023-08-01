
# libraries -------------------------------------------------------

# library(rnaturalearth) # for some data sets
# library(sf) # for working with shapefiles
# library(tidyverse) # for manipulating data
# library(here) # for helping with file paths
# library(cowplot) # for adding extra functions to allow for insets
# library(raster) # to read in raster data
# library(ggspatial) # allows adding scale bars and compasses


# a hacky way of adding thick borders to certain parts of the coastline....
clip_ne_world <- function(ne_world, country, xmin, ymin, xmax, ymax){
    ne_world %>% 
        filter(name == country) %>% 
        st_crop(y= c(xmin = xmin, 
                    ymin = ymin, 
                    xmax = xmax, 
                    ymax = ymax))
}

make_map <- function(ne_world, 
                      depth, 
                      iran_clipped1, 
                      iran_clipped2, 
                      russia_clipped,
                      rivers,
                      rivers_europe,
                      lakes, 
                      lakes_europe) {
  # set params for figures -----------------------------------------------
  theme_set(theme_bw())

  # shared theme for the main figure and the inset
  ptheme <- theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#4A79DD"),
    text = element_text(family = "Arial"),
    axis.text = element_text(color = "black")
  )

  # set the lat, long boundaries for main figure
  sea_x_lim <- c(46, 56)
  sea_y_lim <- c(36, 48)

  # this sets the coordinates for the box that will go on the inset figure to mark where the sea is
  sea_bbox <- crossing(x = sea_x_lim, y = sea_y_lim) %>%
    mutate(order = c(1, 2, 4, 3)) %>%
    arrange(order)

  # lat, long limits for inset figure
  world_x_lim <- c(-10, 80)
  world_y_lim <- c(20, 70)

  # names and coordinates to mark countries and the sea on the main figure
  names <- tribble(
    ~name, ~lat, ~long,
    "Iran", 36, 53,
    "Azerbaijan", 40.5, 47.5,
    "Russia", 46.75, 47,
    "Kazakhstan", 43.5, 54,
    "Turkmenistan", 40.3, 54.75,
    "Caspian Sea", 42.5, 50.25
  )

  # plotting -------------------------------------------------------------

  # inset figure
  w <- ggplot(data = ne_world) +
    geom_sf(fill = "#BBE2C6", color = "black", size = 0.15) +
    coord_sf(xlim = world_x_lim, ylim = world_y_lim) +
    geom_polygon(data = sea_bbox, aes(x = x, y = y), color = "red", size = 0.35, alpha = 0) +
    theme_void() +
    ptheme +
    theme(
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.border = element_rect(size = 0.5, fill = NA)
    )

  # main figure
  cs <- ggplot(data = ne_world) +
      # geom_sf(color = "black", size = 0.15, alpha = 0) +
    geom_raster(data = depth, aes(x = x, y = y, fill = bathymetry)) +
    
    # the "clipped" layers are just there so their borders show up on the coast
    # they mark were sampling occurred
    geom_sf(data = iran_clipped1, color = "red", alpha = 0, size = 2) +
    geom_sf(data = iran_clipped2, color = "red", alpha = 0, size = 2) +
    geom_sf(data = russia_clipped, color = "red", alpha = 0, size = 2) +
    
    # these are the main layers: national borders, coastlines, rivers, lakes
    geom_sf(fill = "#BBE2C6") +
    geom_sf(data = rivers_europe, size = 0.25, color = "#4A79DD", alpha = 0.7) +
    geom_sf(data = rivers, size = 0.25, color = "#4A79DD", alpha = 0.7) +
    geom_sf(data = lakes, color = "black", fill = "#4A79DD", size = 0.15) +
    geom_sf(data = lakes_europe, color = "black", fill = "#4A79DD", size = 0.15) +
    
    # makes the borders look a little nicer
    geom_sf(color = "black", size = 0.15, alpha = 0) +

    # limits the coordinates
    coord_sf(xlim = sea_x_lim, ylim = sea_y_lim) +
    
    # adds scalebar and compass
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(
      pad_x = unit(0.25, "in"), pad_y = unit(0.22, "in"),
      height = unit(0.4, "in"), width = unit(0.4, "in"),
      style = north_arrow_fancy_orienteering) +
    
    # the begin parameter makes the lowest value a nice color of blue
    scale_fill_viridis_c(option = "turbo", name = "Depth (m)", direction = 1, begin = 0.12, end = 1) +
    
    # adds the place names to the map
    geom_text(data = names, aes(long, lat, label = name), size = 3.5, family = "Arial") +
    
    # adds the shared theme
    ptheme +
    xlab("Longitude") +
    ylab("Latitude")

  # combines the two figures and shrinks the inset
  # note that it might look weird when plotted in RStudio/VSCode
  # what matters is how the final file looks after using ggsave
  final_plot <- cs %>% 
    ggdraw() +
    draw_plot(w, x = 0.77, y = 0.815, width = 0.2, height = 0.2) +
    theme(panel.border = element_rect(NA),
          panel.background = element_rect("white")) +
    panel_border(color = "white")

  
  file_path <- here("submission", "map.png")
  ggsave(file_path, plot = final_plot, width = 129, height = 141.9, dpi = 800, units = "mm")
  file_path
}