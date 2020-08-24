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

#' Calculate the bias and variance for a resample object on a 0-1 classification
#' 
#' @param resample resample object from mlr::resample, predict must be 'both'
#' @return list of train/test bias and variance + resample object for rds stash
#' @export
get_resample_class_res = function(resample){
  train = resample$pred$data %>%
    filter(set=='train') %>%
    mutate(bias = ifelse(truth==response,0,1),
           variance = ifelse(response==1, prob.0,prob.1))
  
  test = resample$pred$data %>%
    filter(set=='test') %>%
    mutate(bias = ifelse(truth==response,0,1),
           variance = ifelse(response==1, prob.0,prob.1))
  
  return(
    list(train_mean_bias=mean(train$bias), 
         train_mean_variance=mean(train$variance),
         test_mean_bias=mean(test$bias), 
         test_mean_variance=mean(test$variance),
         resample_obj = resample)
  )
}

#' Calculate the bias and variance for a resample object on a regression
#' 
#' @param resample resample object from mlr::resample, predict must be 'both'
#' @return list of train/test bias and variance + resample object for rds stash
#' @export
get_resample_regr_res = function(resample){
  train_pred = resample$pred$data %>%
    filter(set=='train') %>%
    group_by(id) %>%
    summarise(mean_pred = mean(response))
    
  train = resample$pred$data %>%
    filter(set=='train') %>%
    merge(train_pred, on='id') %>%
    mutate(ind_var = (response-mean_pred)^2,
           ind_bias = (response-truth)^2) %>%
    summarize(mean_bias = mean(ind_bias),
              mean_var = mean(ind_var)) %>%
    as.list()
  
  test_pred = resample$pred$data %>%
    filter(set=='test') %>%
    group_by(id) %>%
    summarise(mean_pred = mean(response))
  
  test = resample$pred$data %>%
    filter(set=='test') %>%
    merge(test_pred, on='id') %>%
    mutate(ind_var = (response-mean_pred)^2,
           ind_bias = (response-truth)^2) %>%
    summarize(mean_bias = mean(ind_bias),
              mean_var = mean(ind_var)) %>%
    as.list()
  
  return(
    list(train_mean_bias = train$mean_bias,
         train_mean_variance=train$mean_var,
         test_mean_bias=test$mean_bias,
         test_mean_variance=test$mean_var,
         resample_obj = resample)
  )
}