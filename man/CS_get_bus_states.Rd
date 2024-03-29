% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling.R
\name{CS_get_bus_states}
\alias{CS_get_bus_states}
\title{Extract businesses state}
\usage{
CS_get_bus_states(vec)
}
\arguments{
\item{vec}{A vector with businesses address, preferably collected from CS_get_bus_df}
}
\value{
A character vector containing states location of these businesses
}
\description{
Process values, retire NAs and returns a vector with these states
}
\examples{
CS_get_total_ksoc(unique(df$Capital.social))

}
