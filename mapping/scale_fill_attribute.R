source('mapping/define_fill_continuous_sequential.R', local = T)

scale_fill_attribute <- function(data, attribute){
  #' scale_fill_attribute
  #' 
  #' @description Handle picking predefined colour scales for a given attribute.
  #' 
  #' @param data, data.frame, dataset to define scale attributes
  #' @param attribute, character, column to be symbolised by the colour scale
  #' 
  #' @details To add a new palette mapping, add a named item to palette_reference.

  palette_reference <- list()
  palette_reference[['dummy']] = 'Mint'
  
  if(!attribute %in% names(palette_reference)){
    stop(paste0('Unknown attribute scale: ', attribute, '. See scale_fill_attribute to add a new attribute scale.'))
  }
  
  pal <- palette_reference[[attribute]]
  
  s <- define_fill_continuous_sequential(data = data, attribute = attribute, palette = pal)
  
  return(s)
  
}
