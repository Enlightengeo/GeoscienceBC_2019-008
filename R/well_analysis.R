#' Function to be mapped and get the horizontal well spacing and wells within a certain
#' distance
#' 
#' @param i dataframe row iter
#' @param horiz_montney_wells dataframe of montney wells
#' @param nn_dist nearest neighbour distances (pre-calculated)
#' @return a row of a dataframe, to be reduced into a proper df
#' @export
mapped_horizontal_well_spacing <- function(i, horiz_montney_wells, nn_dist){
  current_well <- horiz_montney_wells[i,]
  
  prior_wells <- which(x = (horiz_montney_wells$on_prod_date <= current_well$on_prod_date) &
    (horiz_montney_wells$wa_num != current_well$wa_num))
    
  if(length(prior_wells) == 0){
    return(current_well %>%
             sf::st_drop_geometry() %>%
             dplyr::select(unique_surv_id, wa_num) %>%
             dplyr::mutate(no_prior_wells = TRUE, 
                           no_surrounding_wells = FALSE,
                           min_midpoint_dist = NA,
                           min_dist_well = NA,
                           horiz_wells_in_1km = 0,
                           horiz_wells_in_5km = 0,
                           horiz_wells_in_10km = 0,
                           horiz_wells_in_25km = 0)
    )
  }
  
  surrounding_wells <- nn_dist[[1]][i,][nn_dist[[1]][i,] %in% prior_wells]
  surrounding_dist <- nn_dist[[2]][i,][nn_dist[[1]][i,] %in% prior_wells]
  
  if(is_empty(surrounding_wells)){
    return(current_well %>%
             sf::st_drop_geometry() %>%
             dplyr::select(unique_surv_id, wa_num) %>%
             dplyr::mutate(no_prior_wells = FALSE, 
                           no_surrounding_wells = TRUE,
                           min_midpoint_dist = NA,
                           min_dist_well = NA,
                           horiz_wells_in_1km = 0,
                           horiz_wells_in_5km = 0,
                           horiz_wells_in_10km = 0,
                           horiz_wells_in_25km = 0)
    )
  }
  
  current_well %>%
    sf::st_drop_geometry() %>%
    dplyr::select(unique_surv_id, wa_num) %>%
    dplyr::mutate(
      no_prior_wells = FALSE, 
      no_surrounding_wells = FALSE,
      min_midpoint_dist = min(surrounding_dist),
      min_dist_well = 
        horiz_montney_wells$unique_surv_id[surrounding_wells[which.min(surrounding_dist)]],
      horiz_wells_in_1km = sum(surrounding_dist < 1000),
      horiz_wells_in_5km = sum(surrounding_dist < 5000),
      horiz_wells_in_10km = sum(surrounding_dist < 10000),
      horiz_wells_in_25km = sum(surrounding_dist < 25000)
    )
}

#' Calculate horizontal well spacing and join with horizontal well dataframe
#' 
#' @param horiz_dev_well_sf dataframe of montney wells
#' @return a joined sf dataframe with distances added
#' @export
add_horizontal_well_spacing <- function(all_wells_sf){
  horiz_wells <- all_wells_sf %>%
    dplyr::filter(survey_well_type == 'horizontal')
  
  nn_dist <- horiz_wells$midpoint %>% 
    st_coordinates() %>%
    as.data.frame() %>%
    drop_na() %>%
    nn2(., k = 25)
  
  horizontal_well_spacing <- map_dfr(.x = 1:nrow(horiz_wells),
                                     .f = mapped_horizontal_well_spacing,
                                     horiz_montney_wells = horiz_wells,
                                     nn_dist = nn_dist)
  
  left_join(all_wells_sf, horizontal_well_spacing, 
            by = c('unique_surv_id','wa_num'))
}


#' Filter extents of wells and add in production and well completions
#' @param sf_df an sf dataframe to be filtered and modified
#' @param montney_extents an sf polygon that will contain the study data
#' @param well_prod_df production data 
#' @param well_comp_df completion data
#' @return a filtered and modified dataframe
#' @export
filter_extents_add_on_prod_comp <- function(sf_df, montney_extents, well_prod_df, well_comp_df) {
  sf_df %>%
    dplyr::filter(sf::st_contains(montney_extents, sf_df$midpoint, sparse = FALSE)) %>%
    dplyr::left_join(., well_prod_df, by = 'wa_num') %>%
    dplyr::filter(!is.na(on_prod_date)) %>%
    dplyr::left_join(., well_comp_df, by = 'wa_num') %>%
    dplyr::filter(!is.na(frac_start_date))
}

#' Load and process geotiff
#' @param file full path to geotiff
#' @param crs_in input projection
#' @param crs_out output projection
#' @param extent_mask a shapefile that provides a mask for the maps to the proper extent
#' @return a raster object
#' @export
load_geotiff <- function(file, extent_rast) {
  raster::raster(file) %>%
    raster::projectRaster(., crs = crs(extent_rast)) %>%
    raster::resample(., extent_rast)
}

#' Load and process a folder of geotiffs
#' @param file_path full path to geotiffs
#' @param extent_sf the polygon used to mask and resample the rasters (the study extent)
#' @param res the desired resolution for resampling, 500m x 500m by default
#' @return a raster stack for property extract
#' @export
load_resample_geotiffs <- function(file_path, extent_sf, res = c(500,500)){
  extent_rast <- raster::raster(extent_sf, resolution = res)
  
  raster_list <- list.files(file_path, pattern = '.tif', full.names = TRUE) %>%
    map(.x = ., .f = load_geotiff, extent_rast = extent_rast)
  
  raster::stack(raster_list)
}

