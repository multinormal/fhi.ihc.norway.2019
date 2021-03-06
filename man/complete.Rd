% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_num_correct_survey.R
\name{complete}
\alias{complete}
\title{Return the *i*-th completed (imputed) data set}
\usage{
complete(data, i)
}
\arguments{
\item{data}{a value as returned by \code{mice}.}

\item{i}{the index to the completed data set required.}
}
\value{
a completed data set.
}
\description{
The function ensures that any factor variable is converted to a character variable,
permitting the various data sets to be joined without errors about differing types. We
add a probs column which by default is all set to 1; for the Norway data, this should
be rewritten manually.
}
