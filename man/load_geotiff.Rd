% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/well_analysis.R
\name{load_geotiff}
\alias{load_geotiff}
\title{Load and process geotiff}
\usage{
load_geotiff(file, extent_rast)
}
\arguments{
\item{file}{full path to geotiff}

\item{crs_in}{input projection}

\item{crs_out}{output projection}

\item{extent_mask}{a shapefile that provides a mask for the maps to the proper extent}
}
\value{
a raster object
}
\description{
Load and process geotiff
}
