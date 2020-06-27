#' Function to determine bin numbers for catalog determination
#' 
#' @description The ECDF function requires binning earthquakes into discrete categories. This
#' complicates our modelling process by turning a continuous distribution into a 
#' discrete categorical series of bins. There are There are several methods for determining
#' this binning: 
#' Sturges, H. (1926) The choice of a class-interval. J. Amer. Statist. Assoc., 21, 65-66.
#' Scott, D.W. (1979) On optimal and data-based histograms. Biometrika, 66, 605-610.
#' Freedman, D. and Diaconis, P. (1981) On this histogram as a density estimator: L2 theory. Zeit. Wahr. ver. Geb., 57, 453-476.
#' Wand, M. P. (1997). Data-based choice of histogram bin width. The American Statistician, 51(1), 59-64.
#' We use three common methods to calculate the deterministic b-value of 
#' the catalogue. These functions are also used in the bootstrap estimation
#' of uncertainity.
#' @param mw_vector a mw_vector of magnitudes
#' @param method a character of methods: 'rice', 'sturges', 'freedman'
#' @return an integer number of bins
#' @export
k_bins <- function(mw_vector, method = 'rice') {
  if (method == 'sturges') {
    bins <- log2(length(mw_vector)) + 1
  } else if (method == 'freedman') {
    bins <- (max(mw_vector) - min(mw_vector)) / (IQR(mw_vector) / length(mw_vector)^(1/3))
  } else if (method == 'rice') {
    bins <- 2 * length(mw_vector) ^ (1/3)
  } else {
    warning('No binning method specified, defaulting to Rice (1951)')
    bins <- 2 * length(mw_vector) ^ (1/3)
  }
  bins %>% ceiling(.)
}

#' Function to determine the maximum magnitude of a catalog
#' @param centres bin centres
#' @param rev_ecdf reverse ecdf used for gutenberg richter relationship
#' @return double maximum magnitude of distribution
#' @export
mmaxc <- function(centres, rev_ecdf) {
  spl_pred <- smooth.spline(centres,rev_ecdf) %>% predict(.,deriv=2)
  spl_pred$x[which(abs(spl_pred$y) == max(abs(spl_pred$y)))]
}

#' Function to process an earthquake catalog, as a vector of magnitudes
#' @param mw_vector a mw_vector of magnitudes
#' @param method a character of methods: 'rice', 'sturges', 'freedman'
#' @return a dataframe with catalog centres and an ecdf, filtered to above the magnitude of completness
#' @export
process_cat <- function(mw_vector, method = 'rice') {
  num_bins <- k_bins(mw_vector, method)
  
  bins <- seq(min(mw_vector)-0.1,max(mw_vector)+0.1,length.out = num_bins+1)
  
  ecdf <- cut(mw_vector, bins) %>%
    table(mw_vector) %>%
    rowSums() %>%
    rev() %>%
    cumsum() %>%
    rev()
  
  centres <- (bins[2:length(bins)]-bins[1:length(bins)-1])/2+bins[1:length(bins)-1]

  print(paste0('mmaxc: ', mmaxc(centres, ecdf)))
  
  data.frame(centres = centres, ecdf = ecdf) %>%  filter(centres >= mmaxc(centres, ecdf))
}

#' Cluster earthquake catalog using hdbscan
#' @param seismic_catalog a mw_vector of magnitudes
#' @param npts The minimum number of points to start a cluster
#' @return a dataframe with catalog centres and an ecdf, filtered to above the magnitude of completness
#' @export
hdbscan_seismic_catalog <- function(seismic_catalog, npts = 5){
  hdb_cluster_res <- seismic_catalog %>%
    dplyr::mutate(date_time = as.numeric(.$date_time)) %>%
    dplyr::select(X, Y, date_time) %>%
    sf::st_drop_geometry() %>%
    scale() %>%
    as.matrix() %>%
    hdbscan(x = ., minPts = npts)
    
  seismic_catalog %>% 
    dplyr::mutate(cluster = hdb_cluster_res$cluster) %>%
    dplyr::mutate(cluster_bool = ifelse(cluster != 0, 1, 0))
}

#' Load preprocessed seismic catalogue
#' @param file string of file path
#' @return an sf dataframe
#' @export
load_preprocessed_catalogue <- function(file){
  test <- readr::read_csv(file) %>%
    dplyr::select(-X1) %>%
    sf::st_as_sf(coords = c('X','Y'), crs = 26910, remove = FALSE)
}
