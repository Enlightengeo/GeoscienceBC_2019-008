#' Return most frequent variable from a dataframe
#' 
#' @description Meant to be used in a dplyr::mutate call to get most frequent occurence 
#' of a variable by wa_number
#' @param df dataframe to interogate
#' @param var string of variable name to roll up
#' @return column from summarized dataframe (using pull)
#' @export
most_freq_var <- function(df, var){
  df %>% 
    dplyr::group_by(wa_num) %>%
    dplyr::count_(var) %>% 
    dplyr::slice(which.max(n)) %>%
    dplyr::pull(var)
}