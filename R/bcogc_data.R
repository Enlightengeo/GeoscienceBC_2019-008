## COMPLETIONS DATA ##
#' Load completions data from BCOGC Frac Database
#' 
#' @description loads from a file using fread, followed by some basic filtering and cleaning,
#' and then a lot of case_when to clean the input data to a consistent format for future
#' algorithms
#' @param file input file, downloaded from BCOGC (public)
#' @return dataframe
#' @export
load_process_ogc_frac_data <- function(file){
  frac_df <- fread(file) %>%
    janitor::clean_names() %>%
    dplyr::mutate(wa_num = base::as.numeric(wa_num)) %>%
    dplyr::mutate(unique_surv_id = stringr::str_c(wa_num,"-", drillng_event)) %>%
    dplyr::mutate(comp_date = lubridate::dmy(compltn_date), 
                  fluid_type = stringr::str_to_lower(base_fluid),
                  viscosity_gel_type = stringr::str_to_lower(viscosity_gel_type),
                  energizer = stringr::str_to_lower(energizer)) %>%
    dplyr::mutate(
      base_fluid_cat = case_when(
        fluid_type == 'fresh water'       ~ 'water',
        fluid_type == 'surfactant'        ~ 'surfactant',
        fluid_type == 'saline water'      ~ 'water',
        fluid_type == 'propane'           ~ 'propane',
        fluid_type == ''                  ~ 'unknown',
        is.na(fluid_type)                 ~ 'unknown',
        TRUE                              ~ 'other'),
      viscosity_cat = case_when(
        viscosity_gel_type == 'unknown'       ~ 'unknown',
        viscosity_gel_type == 'crosslinked'   ~ 'crosslinked',
        viscosity_gel_type == 'slickwater'    ~ 'slickwater',
        viscosity_gel_type == 'linear'        ~ 'linear',
        viscosity_gel_type == 'none'          ~ 'none',
        viscosity_gel_type == ''              ~ 'unknown',
        TRUE                                  ~ 'unknown'),
      energizer_bool = case_when(
        energizer == 'none'          ~ FALSE,
        energizer == 'energized'     ~ TRUE,
        energizer == 'foam'          ~ TRUE,
        TRUE                         ~ FALSE),
      proppant_cat_1 = case_when(
        proppant_type1 == '40/70 FloPROPTT'            ~ 'resin-coated',
        proppant_type1 == 'OptiProp G2 (20/40)'        ~ 'resin-coated',
        proppant_type1 == 'HydroProp 40/80'            ~ 'ceramic',
        proppant_type1 == 'FultonTec FTecLite (30/50)' ~ 'ceramic',
        proppant_type1 == 'Carbo-EconoProp (30/50)'    ~ 'ceramic',
        proppant_type1 == '20/40 resin coated sand'    ~ 'resin-coated',
        proppant_type1 == 'Resin Coat 30/50'           ~ 'resin-coated',
        proppant_type1 == 'SBExcel 20/40'              ~ 'resin-coated',
        proppant_type1 == 'PowerProp 40/70'            ~ 'resin-coated',
        proppant_type1 == 'TURBOprop'                  ~ 'resin-coated',
        proppant_type1 == '30/50 Ftec ISP'             ~ 'resin-coated',
        TRUE                                           ~ 'sand'),
      proppant_cat_2 = case_when(
        proppant_type2 == '40/70 FloPROPTT'            ~ 'resin-coated',
        proppant_type2 == 'OptiProp G2 (20/40)'        ~ 'resin-coated',
        proppant_type2 == 'HydroProp 40/80'            ~ 'ceramic',
        proppant_type2 == 'FultonTec FTecLite (30/50)' ~ 'ceramic',
        proppant_type2 == 'Carbo-EconoProp (30/50)'    ~ 'ceramic',
        proppant_type2 == '20/40 resin coated sand'    ~ 'resin-coated',
        proppant_type2 == 'Resin Coat 30/50'           ~ 'resin-coated',
        proppant_type2 == 'SBExcel 20/40'              ~ 'resin-coated',
        proppant_type2 == 'PowerProp 40/70'            ~ 'resin-coated',
        proppant_type2 == 'TURBOprop'                  ~ 'resin-coated',
        proppant_type2 == '30/50 Ftec ISP'             ~ 'resin-coated',
        TRUE                                           ~ 'sand'),
      proppant_bool = ifelse((proppant_type1_pumped_t > proppant_type2_pumped_t), TRUE, FALSE)
      ) %>%
    dplyr::mutate(
      proppant_bool = ifelse(is.na(proppant_bool), TRUE, proppant_bool),
      proppant_cat = case_when(
        proppant_cat_1 == 'sand' & proppant_cat_2 == 'sand'                           ~ 'sand',
        proppant_cat_1 == 'sand' & proppant_cat_2 == 'resin-coated' & proppant_bool   ~ 'hybrid',
        proppant_cat_1 == 'sand' & proppant_cat_2 == 'resin-coated' & !proppant_bool  ~ 'resin-coated',
        proppant_cat_1 == 'resin-coated' & proppant_cat_2 == 'sand' & proppant_bool   ~ 'resin-coated',
        proppant_cat_1 == 'resin-coated' & proppant_cat_2 == 'sand' & !proppant_bool  ~ 'hybrid',
        proppant_cat_1 == 'ceramic' & proppant_cat_2 == 'sand' & proppant_bool        ~ 'ceramic',
        proppant_cat_1 == 'ceramic' & proppant_cat_2 == 'sand' & !proppant_bool       ~ 'hybrid',
        proppant_cat_1 == 'sand' & proppant_cat_2 == 'ceramic' & proppant_bool        ~ 'hybrid',
        proppant_cat_1 == 'sand' & proppant_cat_2 == 'ceramic' & !proppant_bool       ~ 'ceramic',
        TRUE ~ 'unknown')
    ) %>%
    tidyr::replace_na(list(proppant_type1_pumped_t = 0, proppant_type2_pumped_t = 0,
                           proppant_type3_pumped_t = 0, proppant_type4_pumped_t = 0,
                           proppant_type1_placed_t = 0, proppant_type2_placed_t = 0,
                           proppant_type3_placed_t = 0, proppant_type4_placed_t = 0)) %>%
    dplyr::mutate(stage_proppant_pumped_total_t = proppant_type1_pumped_t + proppant_type2_pumped_t +
                    proppant_type3_pumped_t + proppant_type4_pumped_t) %>%
    dplyr::mutate(stage_proppant_placed_total_t = proppant_type1_placed_t + proppant_type2_placed_t +
                    proppant_type3_placed_t + proppant_type4_placed_t) %>%
    dplyr::mutate(stage_proppant_t = pmax(stage_proppant_pumped_total_t, 
                                          stage_proppant_placed_total_t, 
                                          na.rm = TRUE)) %>%
    dplyr::mutate(total_gas_injected_m3 = total_n2_pumped_scm + total_ch4_pumped_e3m3*1000) %>%
    dplyr::mutate(stage_duration_min = total_fluid_pumped_m3 / avg_rate_m3_min) %>%
    dplyr::mutate(stage_length_m = compltn_base_depth_m - compltn_top_depth_m) %>%
    dplyr::mutate(stage_midpoint_md = (compltn_base_depth_m + compltn_top_depth_m)/2)
}

#' Summarize BCOGC Frac Database at a well level
#' 
#' @description This function summarizes the Frac Database at a well level, in a consistent
#' way relative to the geoLOGIC data. Has lots of warnings due to missing values, but can't
#' explicitly remove errors without killing a lot of data
#' @param frac_df processed dataframe from load_process_bcogc_frac_data()
#' @return dataframe of well level statistics
#' @export
summarize_bcogc_frac_data <- function(frac_df){
  well_comp_df <- frac_df %>%
    dplyr::group_by(wa_num) %>%
    dplyr::summarise(
      n_stages = n(),
      frac_start_date = min(comp_date),
      frac_end_date = max(comp_date),
      calc_completed_length_m = max(compltn_base_depth_m) - 
        min(compltn_top_depth_m) + mean(stage_length_m, na.rm = TRUE),
      mean_proppant_per_stage_t = mean(stage_proppant_t, na.rm = TRUE),
      sd_proppant_per_stage_t = sd(stage_proppant_t, na.rm = TRUE),
      max_proppant_per_stage_t = max(stage_proppant_t, na.rm = TRUE),
      calc_total_proppant_t = sum(stage_proppant_t, na.rm = TRUE),
      total_gas_injected_m3 = mean(total_gas_injected_m3, na.rm = TRUE),
      mean_fluid_per_stage_m3 = mean(total_fluid_pumped_m3, na.rm = TRUE),
      sd_fluid_per_stage_m3 = sd(total_fluid_pumped_m3, na.rm = TRUE),
      max_fluid_per_stage_m3 = max(total_fluid_pumped_m3, na.rm = TRUE),
      calc_total_fluid_m3 = sum(total_fluid_pumped_m3, na.rm = TRUE),
      avg_stage_length = mean(stage_length_m, na.rm = TRUE),
      avg_stage_spacing = (max(stage_midpoint_md) - min(stage_midpoint_md))/n(),
      max_rate_m3_min = max(avg_rate_m3_min, na.rm = TRUE),
      mean_rate_m3_min = mean(avg_rate_m3_min, na.rm = TRUE),
      max_stage_duration_min = max(stage_duration_min, na.rm = TRUE),
      mean_stage_duration_min = mean(stage_duration_min, na.rm = TRUE),
      max_breakdown_mpa = max(break_down_pressure_m_pa, na.rm = TRUE),
      sd_breakdown_mpa = sd(break_down_pressure_m_pa, na.rm = TRUE),
      mean_breakdown_mpa = mean(break_down_pressure_m_pa, na.rm = TRUE),
      max_isip_mpa = max(inst_shut_in_pressure_m_pa, na.rm = TRUE),
      sd_isip_mpa = sd(inst_shut_in_pressure_m_pa, na.rm = TRUE),
      mean_isip_mpa = mean(inst_shut_in_pressure_m_pa, na.rm = TRUE)
    ) %>%
    dplyr::mutate(fluid_intensity_comp_length_m3_m = calc_total_fluid_m3 / calc_completed_length_m,
                  proppant_intensity_comp_length_t_m = calc_total_proppant_t / calc_completed_length_m,
                  completed_length_m = calc_completed_length_m)
  
  most_frequent_cat <- data.frame(
    wa_num = frac_df %>% group_by(wa_num) %>%  summarize(n()) %>% pull(wa_num),
    base_fluid_cat = most_freq_var(frac_df,'base_fluid_cat'),
    viscosity_cat = most_freq_var(frac_df,'viscosity_cat'),
    energizer_bool = most_freq_var(frac_df,'energizer_bool'),
    proppant_cat = most_freq_var(frac_df,'proppant_cat')
  )
  
  well_comp_df <- well_comp_df %>%
    left_join(., most_frequent_cat, by = 'wa_num') %>%
    dplyr::mutate(
      frac_duration_days = as.numeric(frac_end_date - frac_start_date),
      calc_proppant_intensity_kg_m = calc_total_proppant_t / completed_length_m * 1000,
      calc_fluid_intensity_m3_m = calc_total_fluid_m3 / completed_length_m,
      total_gas_injected_m3 = ifelse(is.na(total_gas_injected_m3), 0, total_gas_injected_m3),
      breakdown_isip_ratio = mean_breakdown_mpa / mean_isip_mpa
    )
}

#' Estimate skipped stages from BCOGC database
#' 
#' @description Calculates skipped stages based on low volume injections. Not perfect 
#' as need the 'planned' stages for each well which aren't available in the BCOGC 
#' catalogue (vs. the Geologic Frac Database)
#' @param frac_df processed dataframe from load_process_bcogc_frac_data()
#' @return column of skipped stages for input dataframe
#' @export
calc_skipped_stages <- function(frac_df){
  orig_stages <- frac_df %>%
    dplyr::group_by(wa_num) %>%
    dplyr::summarize(orig_stages= n())
  
  good_stages <- frac_df %>%
    dplyr::filter(total_fluid_pumped_m3 > 40) %>%
    dplyr::group_by(wa_num) %>%
    dplyr::summarise(good_stages = n())
  
  orig_stages %>%
    dplyr::left_join(., good_stages, by = 'wa_num') %>%
    dplyr::mutate(skipped_stages = orig_stages - good_stages) %>%
    dplyr::mutate(skipped_stages = ifelse(is.na(skipped_stages),0,skipped_stages)) %>%
    dplyr::pull(skipped_stages)
}

## PRODUCTION DATA ##
#' Load and process production data
#' @param file file for production data from BC OGC
#' @return granular (month by month) production data
#' @export
load_process_ogc_prod_data <- function(file){
  fread(file, skip = 1) %>%
    janitor::clean_names() %>%
    dplyr::mutate(year = str_sub(prod_period, 1,4),
                  month = str_sub(prod_period, 5,6)) %>%
    dplyr::mutate(date = lubridate::ymd(str_c(year,month,'01',sep = '-'))) %>%
    dplyr::mutate(wa_num = base::as.numeric(wa_num))
}
  
#' Summarize production data by well
#' @param prod_df dataframe of production data, from `load_process_ogc_prod_data()`
#' @return dataframe of well by well production data rollup
#' @export
summarize_prod_data <- function(prod_df){
  prod_df %>%
    dplyr::group_by(wa_num) %>%
    dplyr::summarise(on_prod_date = min(date),
                     last_reported_date = max(date),
                     cum_gas_to_date_e3m3 = max(gas_prod_cum_e3m3, na.rm=TRUE),
                     cum_oil_to_date_m3 = max(oil_prod_cum_m3, na.rm=TRUE),
                     cum_water_to_date_m3 = max(water_prod_cum_m3, na.rm=TRUE),
                     cum_cond_to_date_m3 = max(cond_prod_cum_m3, na.rm=TRUE))
}

## SURVEY DATA ##
#' Load and process BCOGC surface survey data
#' @param file file for survey data from BC OGC 
#' @return sf dataframe of well surface information
#' @export
load_process_ogc_surface_data <- function(file){
  fread(file, skip = 1) %>%
    janitor::clean_names() %>%
    dplyr::select(wa_num, well_surf_loc, well_name, surf_utm83_northng, surf_utm83_eastng,
                  surf_utm_zone_num, ground_elevtn, oper_id, oper_abbrev, oper_abbrev2,
                  optnl_unit, well_area_name, special_well_class_code) %>%
    dplyr::mutate(surf_utm_zone_num = as.numeric(surf_utm_zone_num)) %>%
    dplyr::filter(surf_utm_zone_num == 10) %>%
    dplyr::filter(!is.na(surf_utm_zone_num)) %>%
    dplyr::mutate(wa_num = as.numeric(wa_num)) %>%
    sf::st_as_sf(., coords = c('surf_utm83_eastng','surf_utm83_northng'), 
                 crs = 26910, remove = FALSE)
}

#' Load and process BCOGC downhole survey data for deviated and horizontal wells
#' @param file file for deviated / horizontal survey data from BC OGC 
#' @param well_surface_sf surface survey information from BCOGC from `load_process_ogc_surface_data()`
#' @return sf dataframe of well surveys with lines of deviated/horizontal wells
#' @export
load_process_ogc_survey_data <- function(file, well_surface_sf){
  
  cleaned_well_surf_sf <- well_surface_sf %>%
    dplyr::select(wa_num, surf_utm83_northng, 
                  surf_utm83_eastng, ground_elevtn) %>%
    sf::st_drop_geometry() %>% 
    as.data.frame()
  
  fread(file) %>%
    janitor::clean_names() %>%
    dplyr::mutate(wa_num = as.numeric(wa_num)) %>%
    dplyr::mutate(unique_surv_id = str_c(wa_num,"-", drilling_event)) %>%
    left_join(., cleaned_well_surf_sf, by = 'wa_num') %>%
    dplyr::mutate(tvd = ground_elevtn - tv_depth_m,
                  easting = surf_utm83_eastng + east_west_m,
                  northing = surf_utm83_northng + north_south_m) %>%
    dplyr::filter(!is.na(tvd))
}

#' Calculate the midpoint of lines for horizontal laterals, for mapping
#' @param sf_lines sf linestring
#' @return midpoint as sf point
#' @export
st_line_midpoints <- function(sf_lines = NULL) {
  
  g <- st_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    # this is just a copypaste of View(maptools:::getMidpoints):
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }
    
    mids <- st_point(get_mids(coords))
  })
  
  st_sfc(g_mids, crs = st_crs(sf_lines)) %>%
    st_sf(.) %>%
    st_geometry(.)
}


#' Calculate the lateral portion of horizontals, using an aggressive inclination cutoff 
#' to avoid deviated wells and get a good mean tvd
#' @param survey_df survey dataframe
#' @return horizontal laterals as sf linestrings
#' @export
get_horizontal_laterals <- function(survey_df){
  horiz_surv_linestring <- survey_df %>%
    dplyr::filter(inclination_deg >= 88) %>%
    dplyr::group_by(unique_surv_id) %>%
    dplyr::arrange(measured_depth_m, .by_group = TRUE) %>%
    dplyr::filter(n() >= 3) %>%
    dplyr::filter(tvd < 0) %>%
    st_as_sf(., coords = c('easting', 'northing'), dim = 'XY', 
             remove = FALSE, crs = 26910) %>%
    dplyr::summarise(wa_num = first(wa_num),
                     drilling_event = first(drilling_event),
                     ground_elevtn = mean(ground_elevtn),
                     mean_tvd = mean(tvd),
                     mean_easting = mean(easting),
                     mean_northing = mean(northing),
                     do_union = FALSE) %>%
    sf::st_cast(., 'LINESTRING') %>%
    dplyr::mutate(midpoint = st_line_midpoints(.),
                  survey_well_type = 'horizontal')
}

#' Get surveys for all wells that aren't 'horizontal'
#' @param well_survey_sf survey dataframe
#' @param horizontal_well_sf horizontal survey dataframe from `get_horizontal_laterals`
#' @return horizontal laterals as sf linestrings
#' @export
get_deviated_surveys <- function(well_survey_sf, horizontal_well_sf){
  well_survey_sf %>%
    dplyr::filter(!unique_surv_id %in% horizontal_well_sf$unique_surv_id) %>%
    dplyr::group_by(unique_surv_id) %>%
    dplyr::arrange(measured_depth_m, .by_group = TRUE) %>%
    dplyr::filter(n() >= 3) %>%
    dplyr::filter(tvd < 0) %>%
    st_as_sf(., coords = c('easting', 'northing'), dim = 'XY', 
             remove = FALSE, crs = 26910) %>%
    dplyr::summarise(wa_num = first(wa_num),
                     drilling_event = first(drilling_event),
                     ground_elevtn = mean(ground_elevtn),
                     mean_tvd = mean(tvd),
                     mean_easting = mean(easting),
                     mean_northing = mean(northing),
                     do_union = FALSE) %>%
    sf::st_cast(., 'LINESTRING') %>%
    dplyr::mutate(midpoint = st_line_midpoints(.),
                  survey_well_type = 'deviated')
}

#' Wrapper to get horizontal and deviated wells from the well survey sf
#' @param horiz_dev_sf processed surveys from `load_process_ogc_survey_data`
#' @return sf dataframe of horizontal and deviated wells 
#' @export
get_horiz_dev_sf <- function(horiz_dev_sf){
  horiz_sf <- get_horizontal_laterals(horiz_dev_sf)
  dev_sf <- get_deviated_surveys(horiz_dev_sf, horiz_sf)
  rbind(horiz_sf, dev_sf)
}

#' Get single points for the remaining wells based on surface location
#' @param file filename
#' @param well_surface_sf surface survey sf dataframe
#' @param horiz_dev_wa_num numeric vector of wa_num from horizontal wells
#' @return horizontal laterals as sf linestrings
#' @export
load_process_vertical_surveys <- function(file, well_surface_sf, horiz_dev_wa_num){
  
  cleaned_well_surf_sf <- well_surface_sf %>%
    dplyr::select(wa_num, surf_utm83_northng, 
                  surf_utm83_eastng, ground_elevtn) %>%
    sf::st_drop_geometry() %>% 
    as.data.frame()
  
  vert_df <- fread(file, skip = 1) %>%
    janitor::clean_names() %>%
    dplyr::mutate(wa_num = as.numeric(wa_num)) %>%
    dplyr::filter(!wa_num %in% horiz_dev_wa_num) %>%
    dplyr::mutate(unique_surv_id = str_c(wa_num,"-", drilling_event)) %>%
    dplyr::mutate(td_depth_m = base::as.numeric(td_depth_m),
                  td_north_m = base::as.numeric(td_north_m),
                  td_east_m = base::as.numeric(td_east_m),
                  survey_well_type = 'vertical') %>%
    dplyr::filter(!is.na(td_depth_m)) %>%
    dplyr::mutate(td_north_m = replace_na(td_north_m,0),
                  td_east_m = replace_na(td_east_m, 0)) %>%
    dplyr::mutate(spud_date = lubridate::ymd(spud_date)) %>% 
    dplyr::mutate(rig_rels_date = lubridate::ymd(rig_rels_date)) %>%
    dplyr::left_join(., cleaned_well_surf_sf, by = 'wa_num') %>%
    dplyr::mutate(tvd = ground_elevtn - td_depth_m,
                  easting = surf_utm83_eastng + td_east_m,
                  northing = surf_utm83_northng + td_north_m) %>%
    dplyr::filter(!is.na(tvd)) %>%
    sf::st_as_sf(., coords = c('easting', 'northing'), dim = 'XY', 
                 remove = FALSE, crs = 26910) %>%
    dplyr::select(unique_surv_id, wa_num, drilling_event, ground_elevtn, 
                  mean_tvd = tvd, mean_easting = easting, mean_northing = northing,
                  survey_well_type) 
  
  vert_df$midpoint <- st_geometry(vert_df)
  
  vert_df
}
