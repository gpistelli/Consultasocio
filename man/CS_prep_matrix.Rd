% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/making_df.R
\name{CS_prep_matrix}
\alias{CS_prep_matrix}
\title{Turn vector into a matrix vector}
\usage{
CS_prep_matrix(vec)
}
\arguments{
\item{vec}{A character vector containing values to be transformed into boolean}
}
\value{
A named character vector containing the attributes present in our original vector
}
\description{
Turn a frequency vector into a named vector with boolean values like "y" or "n".
}
\details{
Recommended for building a MCA dataset, as a way to turn character or numeric values into a matrix-like categorical vector.
}
\examples{
CS_prep_matrix(c("Transf", "Transf", "Comerc"))

}
