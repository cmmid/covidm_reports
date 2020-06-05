define_fill_continuous_sequential <- function(data, attribute, palette){
  #Define the breaks, limits, and labels of a continuous fill scale on an entire dataset
  #data - data.frame, dataset to define scale attributes
  #attribute - character, column to be symbolised by the colour scale
  #palette - character, palette from colorspace::hcl_palettes
  #Useful for sharing a predefined colour scale between plots. 
  
  attr_max <- data %>% pull(!!dplyr::enquo(attribute)) %>% max()
  attr_min <- data %>% pull(!!dplyr::enquo(attribute)) %>% min()
  scale_breaks <- seq(attr_min, attr_max, length.out = 5)
  scale_labels <- as.character(round(scale_breaks, 2))
  scale_range <- c(attr_min, attr_max)
  
  scale_colours <- colorspace::sequential_hcl(5, palette = palette)
  
  scale <- ggplot2::scale_fill_gradientn(breaks = as.character(scale_breaks), labels = scale_labels,
                                limits = scale_range, na.value = "#AAAAAA", colors = scale_colours)
  
  return(scale)
  
}