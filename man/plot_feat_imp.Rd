% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{plot_feat_imp}
\alias{plot_feat_imp}
\title{Plot feature importance}
\usage{
plot_feat_imp(importance, output_path = "../output/",
  label = "classif")
}
\arguments{
\item{importance}{importance results}

\item{output_path}{output path for plot}

\item{label}{label for the file - 'regr' or 'classif'}
}
\value{
ggplot
}
\description{
Plot feature importance
}
