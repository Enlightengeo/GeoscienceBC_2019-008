## COMPLETIONS DATA ##
#' Load completions data from Geologic Frac Database
#' 
#' @description loads from a file using fread, followed by some basic filtering and cleaning,
#' and then a lot of case_when to clean the input data to a consistent format for future
#' algorithms
#' @param file input file, downloaded from Geologic Frac Database (proprietary)
#' @return dataframe
#' @export
load_process_geologic_frac_data <- function(file){
  frac_df <- fread(file, verbose = FALSE) %>%
    janitor::clean_names() %>%
    janitor::remove_empty(., which = c("rows", "cols")) %>%
    dplyr::filter(well_direction == 'H') %>%
    dplyr::filter(well_completed_zone %in% c('Montney', 'Doig', 'Belloy')) %>%
    dplyr::mutate(
      wa_num = as.numeric(str_remove_all(well_licence_number,'\'')),
      frac_start_date = lubridate::ymd(well_start_frac_date),
      frac_end_date = lubridate::ymd(well_end_frac_date),
      base_fluid_cat = case_when(
        well_base_fluid == 'Slickwater' ~ 'water',
        well_base_fluid == 'Surfactant' ~ 'surfactant',
        well_base_fluid == 'Water'      ~ 'water',
        well_base_fluid == 'Propane'    ~ 'propane',
        well_base_fluid == 'Oil/Water'  ~ 'water',
        well_base_fluid == ''           ~ 'unknown',
        well_base_fluid == 'Unknown'    ~ 'unknown',
        TRUE                            ~ 'other'),
      viscosity_cat = case_when(
        well_base_fluid == 'Slickwater'                ~ 'slickwater',
        well_stimulation_fluid == 'Hybrid-Slickwater'  ~ 'slickwater',
        well_stimulation_fluid == 'Sanjel-CO2 POLYjel' ~ 'crosslinked',
        well_stimulation_fluid == 'Sanjel-BORAjel'     ~ 'linear',
        well_stimulation_fluid == 'SLB-WideFrac (YF)'  ~ 'crosslinked',
        well_stimulation_fluid == 'Hybrid-SLB-WideFrac (YF)/HiWAY'  ~ 'crosslinked',
        well_foam_y_n == "Yes"                         ~ 'linear',
        TRUE                                           ~ 'water'),
      energizer_bool = case_when(
        well_energizer == 'CO2'      ~ TRUE,
        well_energizer == 'CO2/N2'   ~ TRUE,
        well_energizer == 'LPG'      ~ TRUE,
        well_energizer == 'N2'       ~ TRUE,
        TRUE                         ~ FALSE),
      proppant_cat = case_when(
        well_proppant_composition == 'Sand'                   ~ 'sand',
        well_proppant_composition == 'Resin-Coated'           ~ 'resin-coated',
        well_proppant_composition == 'Ceramic'                ~ 'ceramic',
        well_proppant_composition == 'Resin-Coated/Sand'      ~ 'hybrid',
        TRUE                                                  ~ 'unknown'),
      stage_duration_min = stage_total_pumped_fluid_m3 / stage_avg_rate_m3_min
    ) %>%
    dplyr::mutate(
      base_fluid_cat = as.factor(base_fluid_cat),
      viscosity_cat = as.factor(viscosity_cat),
      energizer_bool = as.logical(energizer_bool),
      stage_length_m = stage_stage_bottom_depth_mkb - stage_stage_top_depth_mkb,
      stage_midpoint_md = (stage_stage_bottom_depth_mkb + stage_stage_top_depth_mkb)/2)
}

#' Summarize Geologic Frac Database at a well level
#' 
#' @description This function summarizes the Frac Database at a well level, in a consistent
#' way relative to the BCOGC data. Has lots of warnings due to missing values, but can't
#' explicitly remove errors without killing a lot of data
#' @param frac_df processed dataframe from load_process_geologic_frac_data()
#' @return dataframe of well level statistics
#' @export
summarize_geologic_frac_data <- function(frac_df){
  well_comp_df <- frac_df %>%
    dplyr::filter(is.finite(frac_start_date), is.finite(frac_end_date)) %>%
    dplyr::group_by(wa_num, .drop = TRUE) %>%
    dplyr::summarise(
      n_stages = max(well_stages_actual_number, well_stages_attempted_number, na.rm = TRUE),
      skipped_stages = n_stages - 
        min(well_stages_attempted_number, well_stages_actual_number, na.rm = TRUE),
      frac_start_date = min(frac_start_date, na.rm = TRUE),
      frac_end_date = max(frac_end_date, na.rm = TRUE),
      calc_completed_length_m = max(stage_stage_bottom_depth_mkb, na.rm = TRUE) -
        min(stage_stage_top_depth_mkb, na.rm = TRUE),
      well_completed_length_m = mean(well_completed_length_m, na.rm = TRUE),
      mean_proppant_per_stage_t = mean(stage_proppant_placed_total_t, na.rm = TRUE),
      sd_proppant_per_stage_t = sd(stage_proppant_placed_total_t, na.rm = TRUE),
      max_proppant_per_stage_t = max(stage_proppant_placed_total_t, na.rm = TRUE),
      calc_total_proppant_t = sum(stage_proppant_placed_total_t, na.rm = TRUE),
      total_gas_injected_m3 = mean(well_total_pumped_gas_m3, na.rm = TRUE),
      mean_fluid_per_stage_m3 = mean(stage_total_pumped_fluid_m3, na.rm = TRUE),
      sd_fluid_per_stage_m3 = sd(stage_total_pumped_fluid_m3, na.rm = TRUE),
      max_fluid_per_stage_m3 = max(stage_total_pumped_fluid_m3, na.rm = TRUE),
      calc_total_fluid_m3 = sum(stage_total_pumped_fluid_m3, na.rm = TRUE),
      avg_stage_length = mean(stage_length_m, na.rm = TRUE),
      avg_stage_spacing = mean(well_avg_frac_spacing_m, na.rm = TRUE),
      max_rate_m3_min = max(stage_avg_rate_m3_min, na.rm = TRUE),
      mean_rate_m3_min = mean(stage_avg_rate_m3_min, na.rm = TRUE),
      max_stage_duration_min = max(stage_duration_min, na.rm = TRUE),
      mean_stage_duration_min = mean(stage_duration_min, na.rm = TRUE),
      max_breakdown_mpa = max(stage_break_down_pressure_mpa, na.rm = TRUE),
      sd_breakdown_mpa = sd(stage_break_down_pressure_mpa, na.rm = TRUE),
      mean_breakdown_mpa = mean(stage_break_down_pressure_mpa, na.rm = TRUE),
      max_isip_mpa = max(stage_isip_mpa, na.rm = TRUE),
      sd_isip_mpa = sd(stage_isip_mpa, na.rm = TRUE),
      mean_isip_mpa = mean(stage_isip_mpa, na.rm = TRUE)
    )
  
  most_frequent_cat <- data.frame(
    wa_num = frac_df %>% group_by(wa_num) %>% summarize(n()) %>% pull(wa_num),
    base_fluid_cat = most_freq_var(frac_df,'base_fluid_cat'),
    viscosity_cat = most_freq_var(frac_df,'viscosity_cat'),
    energizer_bool = most_freq_var(frac_df,'energizer_bool'),
    proppant_cat = most_freq_var(frac_df,'proppant_cat')
  )
  
  well_comp_df <- well_comp_df %>%
    left_join(., most_frequent_cat, by = 'wa_num') %>%
    dplyr::mutate(completed_length_m = well_completed_length_m) %>%
    dplyr::mutate(
      frac_duration_days = as.numeric(frac_end_date - frac_start_date),
      calc_proppant_intensity_kg_m = calc_total_proppant_t / completed_length_m * 1000,
      calc_fluid_intensity_m3_m = calc_total_fluid_m3 / completed_length_m,
      total_gas_injected_m3 = ifelse(is.na(total_gas_injected_m3), 0, total_gas_injected_m3),
      breakdown_isip_ratio = mean_breakdown_mpa / mean_isip_mpa
    )
  
  well_comp_df
}

#' Add extra Geologic Frac Database columns at a well level
#' 
#' @description This function adds columns that aren't available in the BCOGC database
#' to the well dataframe generated by `summarize_geologic_frac_data()`
#' 
#' @param well_comp_df well-level dataframe from `summarize_geologic_frac_data()`
#' @param frac_df stage-level dataframe from `load_process_geologic_frac_data()`
#' @return dataframe of well level statistics with extra columns
#' @export
add_extra_geologic_columns <- function(well_comp_df, frac_df){
  extra_cols_df <- frac_df %>%
    dplyr::filter(is.finite(frac_start_date), is.finite(frac_end_date)) %>%
    dplyr::group_by(wa_num, .drop = TRUE) %>%
    dplyr::mutate(sand_off_bool = ifelse(stage_sand_off_or_screen_out_y_n == 'Yes', TRUE, FALSE)) %>%
    dplyr::summarise(
      max_treating_mpa = max(stage_avg_pressure_mpa, na.rm = TRUE),
      sd_treating_mpa = sd(stage_avg_pressure_mpa, na.rm = TRUE),
      mean_treating_mpa = mean(stage_avg_pressure_mpa, na.rm = TRUE),
      number_sand_off_screen_out_stages = sum(sand_off_bool, na.rm = TRUE),
      mean_intervals_per_stage = mean(as.numeric(stage_number_intervals_per_stage), na.rm = TRUE)
    )
  
  most_frequent_cat <- data.frame(
    wa_num = frac_df %>% group_by(wa_num) %>% summarize(n()) %>% pull(wa_num),
    cased_well_bool = most_freq_var(frac_df,'well_open_or_cased'),
    job_objective = most_freq_var(frac_df,'well_job_objective'),
    interval_clusters_bool = most_freq_var(frac_df,'well_use_of_interval_clusters_y_n'),
    hybrid_frac_bool = most_freq_var(frac_df,'well_hybrid_frac_y_n'),
    interference_bool = most_freq_var(frac_df,'well_offset_well_interference_y_n'),
    well_comments = most_freq_var(frac_df,'well_comments'),
    stim_company = most_freq_var(frac_df,'stage_stim_company')
  ) %>%
    dplyr::mutate(
      cased_well_bool = ifelse(cased_well_bool == 'Open', FALSE, TRUE),
      interval_clusters_bool = ifelse(cased_well_bool == 'Yes', TRUE, FALSE),
      hybrid_frac_bool = ifelse(hybrid_frac_bool == 'Yes', TRUE, FALSE),
      interference_bool = ifelse(interference_bool == 'Yes', TRUE, FALSE),
      stim_company = case_when(
        stim_company == ''        ~ 'Unknown',
        is.na(stim_company)       ~ 'Unknown',
        is.null(stim_company)     ~ 'Unknown',
        TRUE                      ~ as.character(stim_company)
      )
    ) %>%
    dplyr::mutate(stim_company = as.factor(stim_company))
  
  well_comp_df <- well_comp_df %>%
    left_join(., extra_cols_df, by = 'wa_num') %>%
    left_join(., most_frequent_cat, by = 'wa_num')
  
  well_comp_df
}

