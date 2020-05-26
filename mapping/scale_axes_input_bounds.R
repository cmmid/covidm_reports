scale_x_input_bounds <- function(data){
  bounds <- st_bbox(data)
  axes_scale <- ggplot2::xlim(bounds$xmin, bounds$xmax)
  return(axes_scale)
}

scale_y_input_bounds <- function(data){
  bounds <- st_bbox(data)
  axes_scale <- ggplot2::ylim(bounds$ymin, bounds$ymax)
  return(axes_scale)
}