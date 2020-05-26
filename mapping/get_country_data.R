#wrapper for raster::getData to get admin level 0 data by iso3c code

get_country_data <- function(iso){
  
  country <- raster::getData(name = 'GADM', country = iso, level = 0, path = '~/Documents/GADM') %>% sf::st_as_sf()
  
  return(country)
  
}