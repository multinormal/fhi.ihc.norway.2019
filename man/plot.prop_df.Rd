% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods_prop_df.R
\name{plot.prop_df}
\alias{plot.prop_df}
\title{\code{plot} method for \code{prop_df} objects.}
\usage{
\method{plot}{prop_df}(object, simple = FALSE, concept_subset = NULL)
}
\arguments{
\item{object}{an object of class \code{prop_df}.}

\item{simple}{if \code{TRUE}, make a simpler plot more suitable for presentations.}

\item{concept_subset}{a vector of concept names to include in the plot. By default, all
concepts are included for plots of concepts (as opposed to attitudes and intentions).}
}
\value{
A \code{ggplot2} object representing the figure.
}
\description{
This method makes a forest plot to display raw and estimated proportions. The reference
line shows what we would expect if participants perform no better than random on a
key concept with two questions with 3 and 2 options, respectively.
}
