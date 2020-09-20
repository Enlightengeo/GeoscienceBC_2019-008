#' Calculate and Plot a PCA unit circle
#' @param num_ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
pca_unit_circle_plot <- function(ml_df, output_path){
  
  num_ml_df <- ml_df %>% 
    select_if(is.numeric) %>% 
    filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  
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
  
  num_ml_df <- ml_df %>% 
    select_if(is.numeric) %>%
    filter_if(~is.numeric(.), all_vars(!is.infinite(.)))
  
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
    dplyr::select(-contains('sd_')) %>%
    dplyr::select(-contains('max_'))
  
  jpeg(paste(output_path,"corrplot.jpeg",sep=""),width = 24, height = 24, units = 'in', res = 300)
  corrplot(cor(filt_df), tl.col = 'black', method = 'ellipse', type = 'upper', order = 'alphabet', tl.srt = 45)
  dev.off()
}

#' Make a boxplot of variables against the target
#' @param ml_df clean, numeric only dataframe of ml predictors
#' @param output_path output path for plot
#' @return ggplot
#' @export
target_boxplot <- function(ml_df, output_path = '../output/',
                           prefix =""){
  
  ml_df_melt <- melt(ml_df, 'seismogenic')
  
  ggplot(ml_df_melt, aes(x = seismogenic, y = value, group = seismogenic, color = seismogenic)) +
    geom_violin() +
    geom_boxplot(width = 0.1) +
    geom_jitter(shape=16, position=position_jitter(0.1), alpha = 0.25) +
    scale_color_manual(values = c('black','red'))+
    facet_wrap(. ~ variable, scales = "free_y", ncol = 6) +
    theme(legend.position = "none", panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
    xlab("") +
    ylab("") + 
    ggsave(paste0(output_path,prefix,"boxplot.jpeg"), width = 24, height = 48, units = 'in') + 
    ggsave(paste0(output_path,prefix,"boxplot.eps"), width = 24, height = 48)
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
  
  top_df = path %>% dplyr::top_n(., -n_best_val, rmse.test.rmse)
  
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

#' Evaluate b-value models and make a plot
#' @param cat earthquake catalogue
#' @param label string for labelling plot
#' @return ggplot
#' @export
cat_bvalue_plot <- function(cat, label){
  # Create a sorted column of magnitudes
  cat$sort <- sort(cat$mw)
  
  # Calculate bins & centres using 10% of Freedman - Diaconis Rule
  num_bins <- k_bins(cat$mw, 'rice')
  bins <- seq(min(cat$mw),max(cat$mw),length.out = num_bins)
  bw <- (max(cat$mw)-min(cat$mw))/num_bins
  centres <- (bins[2:length(bins)]-bins[1:length(bins)-1])/2+bins[1:length(bins)-1]
  
  # Calculate number of earthquakes per bin
  count <- rowSums(table(cut(cat$mw,bins),cat$mw))
  
  # Calculate bin ECDF
  ecdf <- data.frame(centres)
  ecdf$cdf <- (1-ecdf(cat$mw)(centres))*length(cat$mw)
  ecdf$log_cdf <- log10(ecdf$cdf)
  
  # Maximum curvature Mc, using a smoothing spline and predictor method
  spl <- smooth.spline(ecdf$centres,count)
  spl_pred <- predict(spl,deriv=2)
  Mmaxc <- spl_pred$x[which(abs(spl_pred$y) == max(abs(spl_pred$y)))]
  filt_maxc <- ecdf[ecdf$centres > Mmaxc,]
  
  # 5-fold cross validated linear model fit using caret::train
  # Fit lm model using 10-fold CV: model
  maxc_model <- caret::train(
    log_cdf ~ centres, filt_maxc,
    method = "lm",
    trControl = trainControl(
      method = "cv", number = 5,
      verboseIter = FALSE
    )
  )
  
  maxc_result <- data.frame('M'= Mmaxc,
                            'a'= maxc_model$finalModel$coefficients[1],
                            'b'= maxc_model$finalModel$coefficients[2],
                            'r' = unname(maxc_model$results[3]),
                            'r_sd' = unname(maxc_model$results[5]),
                            'bw'= bw) %>%
    dplyr::summarise_all(mean)
  
  # Maximum of histogram method, maximum likelihood method
  Maki <- max(ecdf$centres[count == max(count)])
  Mbar <- mean(cat$mw[cat$mw > Maki])
  b_aki <- log10(exp(1)) / (Maki - Mbar)
  
  # Use a vector of a values and determine best r^2 value
  ## Setup Empty Dataframe
  N = 100
  aki_df = data.frame('a'=rep(0,N), 'r'=rep(0,N))
  
  # Estimate ai values +/- 50 for best fit
  y1 <- min(ecdf$log_cdf)
  x1 <- max(ecdf$centres) 
  # x2 <- Maki
  y2 <- min(ecdf$log_cdf[count == max(count)])
  m <- (y2 - y1)/(Maki-x1)
  a <- -m*x1
  amin <- a*0.5
  amax <- a*1.5
  
  ## Predict data
  i <- 0
  for (ai in linspace(amin,amax,N)) {
    pred <- ai + b_aki * centres
    calc_r <- 1-sum(abs(pred - ecdf$log_cdf))/sum(ecdf$log_cdf)
    aki_df[i, ] <- c(ai, calc_r)
    i <- i + 1
  }
  
  a_aki <- aki_df[aki_df$r == max(aki_df$r),]$a
  r_aki <- aki_df[aki_df$r == max(aki_df$r),]$r
  
  # lm model fit, with slope set as b_aki and offset
  
  # store aki results
  aki_result <- data.frame('M' = Maki,
                           'a' = a_aki, 
                           'b' = b_aki,
                           'r' = r_aki,
                           'r_sd' = log(10) * -b_aki / sqrt(length(cat$mw[cat$mw > Maki])),
                           'bw' = bw) %>%
    dplyr::summarise_all(mean)
  
  ## Machine Learning Goodness of Fit Regression Analysis
  # Import Magnitude Data
  mag_all <- cat$mw
  
  # Setup Empty Dataframe
  N = length(bins)
  df = data.frame('M'=rep(0,N), 'a'=rep(0,N), 'b'=rep(0,N), 'r'=rep(0,N), 'bw'=rep(0,N))
  Mi <- linspace(quantile(cat$mw)[2],quantile(cat$mw)[4],N)
  
  i <- 1
  for (M in Mi) {
    # Filter magnitude values
    filt <- mag_all[mag_all > M]
    
    # Calculate bins & centres using Freedman - Diaconis Rule
    bw <- 2*IQR(filt) / length(filt)^(1/3)
    bins <- seq(min(filt),max(filt),round(bw,2))
    centres <- (bins[2:length(bins)]-bins[1:length(bins)-1])/2+bins[1:length(bins)-1]
    cdf <- data.frame(centres)
    
    # Calculate ECDF
    cdf$cdf <- (1-ecdf(filt)(centres))*length(filt)
    cdf$log_cdf <- log10(cdf$cdf)
    
    # Linear model regression using caret::lm package
    # Use entire data set since N is low
    model <- lm(log_cdf ~ centres, cdf)
    pred <- predict(model, cdf, number = 5)
    calc_r <- 1-sum(abs(pred - cdf$log_cdf))/sum(cdf$log_cdf)
    df[i, ] <- c(M, model$coefficients[1], model$coefficients[2], calc_r, bw)
    i <- i + 1
  }
  
  # Select best fit
  gof_result <- data.frame(lapply(cbind(df[df$r == max(df$r),],'r_sd' = sd(df$r)), FUN = mean))
  
  # Import a discrete colour vector for lines
  col_vect <- brewer.pal(5,'Set1')
  aki_text <- paste('Mc (ML): ',sprintf("%.1f", round(aki_result$M,1)),
                    ' b: ',sprintf("%.2f", round(-aki_result$b,2)),
                    ' r: ',sprintf("%.2f", round(aki_result$r,2),sep=''))
  gof_text <- paste('Mc (GOF): ',sprintf("%.1f", round(gof_result$M,1)),
                    ' b: ',sprintf("%.2f", round(-gof_result$b,2)),
                    ' r: ',sprintf("%.2f", round(gof_result$r,2),sep=''))
  maxc_text <- paste('Mc (MC): ',sprintf("%.1f", round(maxc_result$M,1)),
                     ' b: ',sprintf("%.2f", round(-maxc_result$b,2)),
                     ' r: ',sprintf("%.2f", round(maxc_result$r,2),sep=''))
  xrng = range(ecdf$centres)
  yrng = range(ecdf$cdf)
  # B-value plot 
  plt <- ggplot() +
    geom_histogram(data=cat, aes(x=mw),binwidth = maxc_result$bw) +
    geom_abline(intercept = aki_result$a, slope = aki_result$b, aes(col = 'Maximum\nLikelihood (ML)\n'), size = 1) +
    geom_abline(intercept = gof_result$a, slope = gof_result$b, aes(col = 'Goodness\nof Fit (GOF)\n'), size = 1) +
    geom_abline(intercept = maxc_result$a, slope = maxc_result$b, aes(col = 'Maximum\nCurvature (MC)\n'), size = 1) +
    scale_y_log10(breaks=c(1,10,100,1000,10000)) +
    geom_text(aes(x, y, label = aki_text), data = data.frame(x = xrng[2], y = yrng[2]), 
              hjust = 1, vjust = 0, size = 3.25, col = col_vect[1]) +
    geom_text(aes(x, y, label = gof_text), data = data.frame(x = xrng[2], y = yrng[2]), 
              hjust = 1, vjust = 2, size = 3.25, col = col_vect[2]) +
    geom_text(aes(x, y, label = maxc_text), data = data.frame(x = xrng[2], y = yrng[2]), 
              hjust = 1, vjust = 4, size = 3.25, col = col_vect[3]) +
    geom_point(aes(x=centres, y=cdf), data = ecdf) +
    ylab('Cumulative / Probability Density') +
    scale_fill_manual(name='My Lines', values=c("black", "blue")) +
    scale_color_manual('B-Value Models', values = c('Maximum\nLikelihood (ML)\n' = col_vect[1],
                                                    'Goodness\nof Fit (GOF)\n' = col_vect[2], 
                                                    'Maximum\nCurvature (MC)\n' = col_vect[3])) +
    xlab('Moment Magnitude') +
    ggtitle(label) +
    theme_minimal()
  
  return(plt)
}
