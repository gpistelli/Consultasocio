% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrangling.R
\name{CS_CNAE_conv}
\alias{CS_CNAE_conv}
\title{Convert CNAE code}
\usage{
CS_CNAE_conv(vec)
}
\arguments{
\item{vec}{A vector with businesses CNAEs, preferably collected from CS_get_bus_df}
}
\value{
A named vector with CNAEs sector and character values indicating `yes` or `no`
}
\description{
Convert the business CNAE to our dictionary (check `lista_cod_CNAE` object)
}
\examples{
CS_CNAE_conv(CS_get_CNAE(df$Atividade.econômica))

}
