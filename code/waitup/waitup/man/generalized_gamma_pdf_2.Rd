% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/implement-generalized-gamma.R
\name{generalized_gamma_pdf_2}
\alias{generalized_gamma_cdf_2}
\alias{generalized_gamma_lcdf_2}
\alias{generalized_gamma_lpdf_2}
\alias{generalized_gamma_pdf_2}
\title{Implementation of the generalized Gamma density from Lawless (1992)}
\usage{
generalized_gamma_pdf_2(x, k, mu, sigma)

generalized_gamma_lpdf_2(x, k, mu, sigma)

generalized_gamma_cdf_2(x, k, mu, sigma)

generalized_gamma_lcdf_2(x, k, mu, sigma)
}
\arguments{
\item{x}{Value to evaluate density at.}

\item{k}{Shape parameter.}

\item{mu}{Location parameter.}

\item{sigma}{Scale parameter.}
}
\description{
Implementation of the generalized Gamma density from Lawless (1992)
}
\section{Functions}{
\itemize{
\item \code{generalized_gamma_lpdf_2}: Implementation of the generalized
Gamma log density from Lawless (1992)

\item \code{generalized_gamma_cdf_2}: Implementation of the generalized
Gamma cumulative density from Lawless (1992)

\item \code{generalized_gamma_lcdf_2}: Implementation of the generalized
Gamma cumulative log density from Lawless (1992)
}}

