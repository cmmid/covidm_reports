#validate the geometry of an sf object and test for invalid geometries
validate_geometry <- function(data){
  
  data <- data %>% 
    dplyr::mutate(valid_geom = st_is_valid(geometry, reason = T)) %>% 
    dplyr::mutate(geometry = st_make_valid(geometry)) %>% 
    dplyr::mutate(valid_geom = st_is_valid(geometry, reason = T))
  
  testthat::test_that('all la geometry is valid.', {
    
    vg <- data %>% pull(valid_geom) %>% unique()
    
    testthat::expect_equal(length(vg), 1)
    
    testthat::expect_equal(vg, 'Valid Geometry')
    
  })
  
  return(data)
  
}