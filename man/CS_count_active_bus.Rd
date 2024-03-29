% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling.R
\name{CS_count_active_bus}
\alias{CS_count_active_bus}
\title{Get active businesses}
\usage{
CS_count_active_bus(vec)
}
\arguments{
\item{vec}{A vector with businesses statuses, preferably collected from CS_get_bus_df}
}
\value{
An integer
}
\description{
Counts how many active businesses there is in a business owner data.frame
}
\examples{
CS_count_active_bus(df$Situação.Cadastral)

}
