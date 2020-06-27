#' Spatiotemporal Association Filter
#' @description Run the Schultz et al. (2017) spatiotemporal filter to flag wells & 
#' associated earthquakes that are a) within a cluster, b) within x days of injection 
#' starting, c) prior to x days after injection stops, d) within x m, and e) larger than 
#' a minimum magnitude (generally the magnitude of completeness)
#' @param well_sf the sf dataframe of the wells
#' @param seismic_catalog the filtered seismic catalog
#' @param dist distance to well
#' @param days_before days before start of frac
#' @param days_after days after the end of the frac
#' @param mw_min minimum magnitude
#' @return the input dataframe (well_sf) with seismogenic indicators
#' @export
run_spatiotemporal_filter <- function(well_sf, seismic_catalog,
                                      dist = 5000, days_before = 1,
                                      days_after = 30, mw_min = 1.1){
  
  # filter catalog to clustered events
  is_clustered_mwfilt <- seismic_catalog %>%
    dplyr::mutate(is_cat_id = as.numeric(row.names(.))) %>%
    dplyr::filter(cluster_bool == 1) %>%
    dplyr::filter(mw >= mw_min)
  
  # vectorized find earthquakes within distance of wells
  earthquake_bool <- st_contains(st_buffer(well_sf, dist = dist), 
                                 is_clustered_mwfilt, sparse = FALSE)
  
  # map function to get earthquakes within the specified time for each completion
  map_date_filter <- function(i, well_sf, earthquake_bool, is_clustered_mwfilt, 
                              days_before, days_after){

    is_clustered_mwfilt[(
      as.logical(earthquake_bool[i,]) &
        (is_clustered_mwfilt$date_time >= (well_sf[i,]$frac_start_date - days_before)) &
        (is_clustered_mwfilt$date_time <= (well_sf[i,]$frac_end_date + days_after))),]$is_cat_id
  }

  message('Running Seismogenic Association Filter - Hold On')
  seismogenic_earthquakes <- map(1:nrow(well_sf),
                                 map_date_filter,
                                 well_sf = well_sf,
                                 earthquake_bool = earthquake_bool,
                                 is_clustered = is_clustered_mwfilt,
                                 days_before = lubridate::days(days_before),
                                 days_after = lubridate::days(days_after))
  
  well_sf$seismogenic <- sapply(seismogenic_earthquakes, length) > 0
  well_sf$earthquakes <- enframe(seismogenic_earthquakes)$value
  
  well_sf
}

#' Mapped function to get max magnitude
#' @param earthquake the earthquake list in question
#' @param is_catalog the filtered seismic catalog
#' @export
get_max_mag <- function(earthquake, is_catalog){
  max(0, is_catalog[earthquake,]$mw, na.rm = TRUE)
}

#' Mapped function to get mean magnitude
#' @param earthquake the earthquake list in question
#' @param is_catalog the filtered seismic catalog
#' @export
get_mean_mag <- function(earthquake, is_catalog){
  mean <- mean(is_catalog[earthquake,]$mw, na.rm = TRUE)
  ifelse(is.na(mean), 0, mean)
}

#' Mapped function to count number of earthquakes for each well
#' @param earthquake the earthquake list in question
#' @param is_catalog the filtered seismic catalog
#' @export
get_n_earthquakes <- function(earthquake, is_catalog){
  length(is_catalog[earthquake,]$mw)
}

#' Function to map the max, mean earthquake magnitudes and count the number of earthquakes
#' @param well_sf sf dataframe for the well
#' @param is_catalog the filtered seismic catalog
#' @export
get_earthquake_stats <- function(well_sf, is_catalog){
  well_sf$max_mag <- map_dbl(.x = well_sf$earthquakes, 
                                         .f = get_max_mag, 
                                         is_catalog = is_catalog)
  
  well_sf$n_quakes <- map_dbl(.x = well_sf$earthquakes, 
                                          .f = get_n_earthquakes,
                                          is_catalog = is_catalog)
  
  well_sf$mean_mag <- map_dbl(.x = well_sf$earthquakes,
                                          .f = get_mean_mag, 
                                          is_catalog = is_catalog)
  
  well_sf
}
