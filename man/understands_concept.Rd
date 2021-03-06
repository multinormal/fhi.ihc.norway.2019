% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_understanding.R
\name{understands_concept}
\alias{understands_concept}
\title{Test if a participant understands a concept}
\usage{
understands_concept(data)
}
\arguments{
\item{data}{a data frame of class \code{quiz_long} containing data for a single
participant. Specifically, it is expected that this has been generated within a
\code{group_by}-\code{ungroup} construct, and the function called using
\code{dplyr::summarize}.}
}
\value{
1 if the participant understands a concept or 0 otherwise.
}
\description{
Test if a participant understands a concept
}
