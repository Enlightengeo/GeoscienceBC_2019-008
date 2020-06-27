#' Calculate and Plot a PCA unit circle
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
pca_unit_circle_plot <- function(ml_df, output_path){
  
  num_ml_df <- ml_df %>% select_if(is.numeric)
  
  pca <- prcomp(scale(num_ml_df))
  
  correlations = as.data.frame(cor(num_ml_df,pca$x))
  
  # draw unit circle
  tt = seq(0, 2 * pi, length = 100)
  circle <- data.frame(x= 1 * cos(tt), y = 1 * sin(tt))
  
  # draw PCA arrows
  arrows <- data.frame(x1 = rep(0,nrow(correlations)), 
                       y1 = rep(0,nrow(correlations)),
                       x2 = correlations$PC1,
                       y2 = correlations$PC2)
  
  # scale PCA results to +/- 1 to fit on unit circle plot
  range <- apply(pca$x, 2, range)
  
  # pull coordinates of PCA
  pca_results <- as.data.frame(scale(pca$x, center = TRUE, scale = abs(range[1,])+abs(range[2,]))) 
  
  # custom ggplot of PCA results and unit circle
  ggplot() +
    geom_hline(yintercept = 0, colour = 'gray') +
    geom_vline(xintercept = 0, colour = 'gray') +
    geom_point(data = pca_results, 
               aes(x = PC1, y = PC2), alpha = 0.5) +
    geom_path(data = circle, aes(x = x, y = y), colour = "gray65") +
    geom_segment(data = arrows, 
                 aes(x = x1, y = y1, xend = x2, yend = y2), colour = "gray65") +
    geom_text(data = correlations, 
              aes(x = PC1, y = PC2, label = rownames(correlations)), 
              colour = 'black', size = 2) +
    xlim(-1.1, 1.1) + 
    ylim(-1.1, 1.1) +
    coord_fixed() +
    ggtitle("PCA Correlation Circle") +
    theme_minimal() +
    ggsave(paste(output_path,"pca_circle.jpeg",sep=""), width = 36, height = 24, units = 'cm') + 
    ggsave(paste(output_path,"pca_circle.eps",sep=""), width = 36, height = 24, units = 'cm')
}

#' Calculate and Plot a PCA scree plot
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
pca_scree_plot <- function(ml_df, output_path){
  
  num_ml_df <- ml_df %>% select_if(is.numeric)
  
  pca <- prcomp(scale(num_ml_df))
  
  pca_df <- data.frame(var = summary(pca)$sdev**2, pca = seq(1:length(summary(pca)$sdev)))
  
  ggplot(pca_df, aes(pca,var)) +
    geom_line() +
    geom_point() +
    ylab('Variance') +
    ggtitle("Principal Component Analysis Scree Plot") +
    scale_x_continuous(name = 'Principal Component', breaks = seq(0,max(pca_df$pca))) +
    theme_minimal() + 
    ggsave(paste(output_path,"pca_scree_plot.jpeg",sep=""), width = 12, height = 8, units = 'in') +
    ggsave(paste(output_path,"pca_scree_plot.eps",sep=""), width = 12, height = 8, units = 'in')
}

#' Make a correlation plot
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
ml_corrplot <- function(ml_df, output_path = '../output/', features, target){
  filt_df <- ml_df %>%
    dplyr::mutate(!!target := as.numeric(get(target))) %>%
    dplyr::select(features, target) %>%
    dplyr::rename('maximum_magnitude' = max_mag) %>%
    dplyr::select(-contains('sd_')) %>%
    dplyr::select(-contains('max_'))
  
  jpeg(paste(output_path,"corrplot.jpeg",sep=""),width = 24, height = 24, units = 'in', res = 300)
  corrplot(cor(filt_df), tl.col = 'black', method = 'ellipse', type = 'upper', order = 'alphabet', tl.srt = 45)
  dev.off()
}

target_boxplot <- function(ml_df, output_path = '../output/'){
  
  ml_df_melt <- melt(ml_df, 'seismogenic')
  
  ggplot(ml_df_melt, aes(x = seismogenic, y = value, group = seismogenic, color = seismogenic)) +
    geom_boxplot() +
    scale_color_manual(values = c('black','red'))+
    facet_wrap(. ~ variable, scales = "free_y", ncol = 6) +
    theme(legend.position = "none") +
    xlab("") +
    ylab("") + 
    ggsave(paste0(output_path,"boxplot.jpeg"), width = 24, height = 48, units = 'in') + 
    ggsave(paste0(output_path,"boxplot.eps"), width = 24, height = 48)
}

#' Make a target scatter plot
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
target_scatter_plot <- function(ml_df, output_path = '../output/'){
  
  ml_df_melt <- melt(ml_df, "max_mag")
  
  ggplot(ml_df_melt, aes(x = max_mag, y = value)) +
    geom_point() +
    facet_wrap(. ~ variable, scales = "free_y", ncol = 6) +
    ggsave(paste0(output_path,"scatter_plot.jpeg"), width = 24, height = 48, units = 'in') +
    ggsave(paste0(output_path,"scatter_plot.eps"), width = 24, height = 48, units = 'in')
}

#' Make a target scatter plot
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
plot_classif_filt_importance <- function(classif_tsk, output_path = '../output/'){
  classif_fv = generateFilterValuesData(
    classif_tsk, 
    method = c("FSelector_information.gain", "FSelector_symmetrical.uncertainty"))
  
  colnames(classif_fv$data) <- c("name", "type", "information_gain", "chi_squared")
  
  plot <- plotFilterValues(classif_fv)
  plot +
    ggtitle('Filter Based Feature Importance - Classification') +
    theme(plot.margin=unit(c(1,1,1.5,1.5),"in")) +
    ggsave(paste0(output_path,"classif_filter_feat_import.jpeg"), width = 11, height = 8, units = 'in') + 
    ggsave(paste0(output_path,"classif_filter_feat_import.eps"), width = 11, height = 8, units = 'in')
}


#' Make a target scatter plot
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
plot_regr_filt_importance <- function(regr_tsk, output_path = '../output/'){
  regr_fv = generateFilterValuesData(
    regr_tsk, 
    method = c("FSelector_information.gain", "FSelector_chi.squared"))
  
  colnames(regr_fv$data) <- c("name", "type", "information_gain", "chi_squared")
  
  plot <- plotFilterValues(regr_fv)
  plot +
    ggtitle('Filter Based Feature Importance - Regression') +
    theme(plot.margin=unit(c(1,1,1.5,1.5),"in")) +
    ggsave(paste0(output_path,"regr_filter_feat_import.jpeg"), width = 11, height = 8, units = 'in') + 
    ggsave(paste0(output_path,"regr_filter_feat_import.eps"), width = 11, height = 8, units = 'in')
}

#' Plot wrapper feature selection results
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
plot_classif_feat_seq <- function(path, 
                                  output_path = '../output/',
                                  n_best_val = 1000){
  top_df = path %>% dplyr::top_n(., -n_best_val, logloss.test.mean)
  
  top_opt_df <- top_df %>%
    dplyr::select(-logloss.test.mean) %>%
    dplyr::summarize_all(., sum) %>%
    dplyr::mutate_all(., function(x,n) {x/n}, n = n_best_val) %>%
    gather() %>%
    dplyr::arrange(value)
  
  ggplot(top_opt_df) +
    geom_col(aes(x = reorder(key, -value), y = value)) +
    ggtitle('Sequential (SBFS) Feature Importance - Classification') +
    xlab('Feature') +
    ylab(paste('Proportion in top',n_best_val,'best runs')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    theme(plot.margin=unit(c(1,1,1.5,1.5),"in")) +
    ggsave(paste0(output_path,"classif_sfbs_feat_import.jpeg"), width = 11, height = 8, units = 'in') + 
    ggsave(paste0(output_path,"classif_sfbs_feat_import.eps"), width = 11, height = 8, units = 'in')
}

#' Plot wrapper feature selection results
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @param n_best_val number of top values to compute
#' @return ggplot
#' @export
plot_classif_feat_rand <- function(path, 
                                  output_path = '../output/', 
                                  n_best_val = 10){
  
  top_df = path %>% dplyr::top_n(., -n_best_val, logloss.test.mean)
  
  top_opt_df <- top_df %>%
    dplyr::select(-logloss.test.mean) %>%
    dplyr::summarize_all(., sum) %>%
    dplyr::mutate_all(., function(x,n) {x/n}, n = n_best_val) %>%
    gather() %>%
    dplyr::arrange(value)
  
  ggplot(top_opt_df) +
    geom_col(aes(x = reorder(key, -value), y = value)) +
    ggtitle('Random Feature Importance - Classification') +
    xlab('Feature') +
    ylab(paste('Proportion in top',n_best_val,'best runs')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    theme(plot.margin=unit(c(1,1,1.5,1.5),"in")) +
    ggsave(paste0(output_path,"classif_rand_feat_import.jpeg"), width = 11, height = 8, units = 'in') + 
    ggsave(paste0(output_path,"classif_rand_feat_import.eps"), width = 11, height = 8, units = 'in')
}

#' Plot wrapper feature selection results
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
plot_regr_feat_seq <- function(path, 
                                  output_path = '../output/',
                                  n_best_val = 1000){
  
  top_df = path %>% dplyr::top_n(., n_best_val, rmse.test.rmse)
  
  top_opt_df <- top_df %>%
    dplyr::select(-mae.test.mean,-rmse.test.rmse) %>%
    dplyr::summarize_all(., sum) %>%
    dplyr::mutate_all(., function(x,n) {x/n}, n = n_best_val) %>%
    gather() %>%
    dplyr::arrange(value)
  
  ggplot(top_opt_df) +
    geom_col(aes(x = reorder(key, -value), y = value)) +
    ggtitle('Sequential (SFBS) Feature Importance - Regression') +
    xlab('Feature') +
    ylab(paste('Proportion in',n_best_val,'runs')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    theme(plot.margin=unit(c(1,1,1.5,1.5),"in")) +
    ggsave(paste0(output_path,"regr_sfbs_feat_import.jpeg"), width = 11, height = 8, units = 'in') + 
    ggsave(paste0(output_path,"regr_sfbs_rand_feat_import.eps"), width = 11, height = 8, units = 'in')
}

#' Plot wrapper feature selection results
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
plot_regr_feat_rand <- function(path, 
                                   output_path = '../output/', 
                                   n_best_val = 1000){
  
  top_df = path %>% dplyr::top_n(., -n_best_val, rmse.test.rmse)
  
  top_opt_df <- top_df %>%
    dplyr::select(-rmse.test.rmse, -mae.test.mean) %>%
    dplyr::summarize_all(., sum) %>%
    dplyr::mutate_all(., function(x,n) {x/n}, n = n_best_val) %>%
    gather() %>%
    dplyr::arrange(value)
  
  ggplot(top_opt_df) +
    geom_col(aes(x = reorder(key, -value), y = value)) +
    ggtitle('Random Search Feature Importance - Regression') +
    xlab('Feature') +
    ylab(paste('Proportion in top',n_best_val,'best runs')) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    theme(plot.margin=unit(c(1,1,1.5,1.5),"in")) +
    ggsave(paste0(output_path,"regr_rand_feat_import.jpeg"), width = 11, height = 8, units = 'in') + 
    ggsave(paste0(output_path,"regr_rand_rand_feat_import.eps"), width = 11, height = 8, units = 'in')
}


#' Plot feature importance
#' @param importance importance results
#' @param output_path output path for plot
#' @param label label for the file - 'regr' or 'classif'
#' @return ggplot
#' @export
plot_feat_imp <- function(importance,
                          output_path = '../output/',
                          label = 'classif'){
  feat_imp = as.data.frame(importance$res) %>%
    tidyr::gather()
  
  ggplot(feat_imp) +
    geom_col(aes(x = reorder(key, value), y = value)) +
    coord_flip() +
    ylab("Feature Importance") +
    xlab("") +
    ggsave(paste0(output_path,label,"_trained_feat_import.jpeg"), width = 8, height = 12, units = 'in')
}

#' Make a single PDP plot for mapping
#' @param i feature number
#' @param predictor predictor
#' @param feats feats
#' @return ggplot
#' @export
make_pdp_plot <- function(i, predictor, feats){
  pdp <- FeatureEffect$new(predictor, method = 'pdp+ice', 
                           feature = feats[i])
  
  plot(pdp) + theme_minimal() + theme(axis.title.y=element_blank())
}
