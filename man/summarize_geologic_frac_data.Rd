% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geologic_data.R
\name{summarize_geologic_frac_data}
\alias{summarize_geologic_frac_data}
\title{Summarize Geologic Frac Database at a well level}
\usage{
summarize_geologic_frac_data(frac_df)
}
\arguments{
\item{frac_df}{processed dataframe from load_process_geologic_frac_data()}
}
\value{
dataframe of well level statistics
}
\description{
This function summarizes the Frac Database at a well level, in a consistent
way relative to the BCOGC data. Has lots of warnings due to missing values, but can't
explicitly remove errors without killing a lot of data
}
