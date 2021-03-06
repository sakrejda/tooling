% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api-tests.R
\name{expect_is_pdf}
\alias{expect_is_pdf}
\title{Verify requirements of a pdf.}
\usage{
expect_is_pdf(f, domain, arg_bounds, n_arg_points)
}
\arguments{
\item{f}{the putative probability density function.}

\item{domain}{c(lower, upper) indicating the pdf's domain.}

\item{arg_bounds}{a list with one named element per argument to f,
containing a 2-vector with the lower and upper bound for
parameter values of f to consider.}

\item{n_arg_points}{An integer indicating how many points to generate
within the parameter bounds.}
}
\description{
Verify requirements of a pdf.
}

