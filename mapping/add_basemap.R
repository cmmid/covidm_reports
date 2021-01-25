add_basemap <- function(scale = 'medium'){
  basemap_data <- rnaturalearth::ne_countries(scale = scale, returnclass = 'sf')
  
  return(geom_sf(data = basemap_data, size = 0.15, fill = '#EBEBEB'))
}