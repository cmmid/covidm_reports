#NOTE - rmapshaper::ms_simplify(snap_interval = 0.05) is a manually defined paramter
#if sliver polygons appear or islands are dropped - this will need to be defined 
#dynamically or updated manually.

#currently - throwing an error for regional plots when adding legend
#working on: get legend from a plot - apply it to others

#get_country_data stores files locally - need to specify the correct location to store geodata 
#in the environment generating the reports. 

suppressPackageStartupMessages({
  require(dplyr)
  require(sf)
  require(ggplot2)
  require(rgeos)
  require(rmapshaper)
  require(colorspace)
  library(docstring)
  require(ggpubr)
})

source('mapping/validate_geometry.R', local = T)
source('mapping/get_country_data.R', local = T)
source('mapping/scale_fill_attribute.R', local = T)
source('mapping/add_basemap.R', local = T)
source('mapping/scale_axes_input_bounds.R', local = T)

.args <- if (interactive()) c(
  "mapping/regionref.rds"
) else commandArgs(trailingOnly = TRUE)

#if maps will be produced on a continent level - mapping will start from a reference file like afrkey
#boundary data will be GADM

afrkey <- readRDS(.args[1])

countries <- lapply(afrkey %>% pull(iso), get_country_data)

countries <- lapply(countries, ms_simplify)

countries <- do.call(rbind, countries) %>% 
  rename(iso = GID_0,
         name_0 = NAME_0) %>% 
  as_tibble() %>% 
  st_as_sf() %>% 
  validate_geometry()

countries <- countries %>% 
  left_join(afrkey, by = c('iso' = 'iso')) %>% 
  #join attributes here
  mutate(dummy = runif(54, 0, 1))

regions <- countries %>% 
  group_by(region) %>% 
  group_split(keep = TRUE)

region_polys <- countries %>% 
  group_by(region) %>% 
  summarise() %>% 
  ms_simplify(snap_interval = 0.05) %>% 
  #join attributes here
  mutate(dummy = runif(5, 0, 1))

create_country_plot <- function(map_data, attribute, colour_scale){
  
  p <- ggplot() +
    add_basemap() +
    geom_sf(data = map_data, aes(fill = map_data %>% pull(!!dplyr::enquo(attribute))), size = 0.2, colour = 'black') +
    colour_scale +
    scale_x_input_bounds(map_data) +
    scale_y_input_bounds(map_data) +
    theme_void() +
    theme(legend.position = 'none')
  
  
  return(p)
  
}

create_region_plot <- function(region_polys, attribute, colour_scale){
  
  p <- ggplot() +
    add_basemap() +
    geom_sf(data = region_polys, aes(fill = region_polys %>% pull(!!dplyr::enquo(attribute))), size = 0.2, colour = 'black') +
    colour_scale +
    scale_x_input_bounds(region_polys) +
    scale_y_input_bounds(region_polys) +
    theme_void() +
    theme(legend.position = 'none')
  
  return(p)
  
}

#define colour scale for whatever attribute by overall dataset (continent) - not region
countries_fill_scale <- scale_fill_attribute(countries, 'dummy')
regional_fill_scale <- scale_fill_attribute(region_polys, 'dummy')

regional_plots <- lapply(regions, create_country_plot, attribute = 'dummy', colour_scale = countries_fill_scale)

region_plot <- create_region_plot(region_polys = region_polys, attribute = 'dummy', regional_fill_scale)

region_panel <- cowplot::plot_grid(plotlist = regional_plots)


