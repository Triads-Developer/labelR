% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sendComparisons.R
\name{sendComparisons}
\alias{sendComparisons}
\title{Send comparisons to Mechanical Turk}
\usage{
sendComparisons(
  hit_type = NULL,
  hit_layout = NULL,
  comparisons_to_send = NULL,
  expire_in_seconds = as.character(60 * 60 * 8),
  n_assignments = "3",
  hit_param_names = c("doc1", "doc2")
)
}
\arguments{
\item{hit_type}{The HITType ID from the MTurk requester dashboard.}

\item{hit_layout}{The Layout ID from the MTurk requester dashboard.}

\item{comparisons_to_send}{A data.frame of two columns, where each row
corresponds with a single comparison and each entry is the text of the
documents to be compared.}

\item{expire_in_seconds}{A string indicating the number of seconds until the
batch of HITs expires. The default is 8 hours (28,800 seconds). The value must
be between 30 and 31,536,000.}

\item{n_assignments}{A string indicating the number of times each unique
comparison is to be uploaded. The default is '1'.}

\item{hit_param_names}{A vector of strings with parameter names as they
appear in the HITformat.html file. These should not be modified unless
using a custom version of HITformat.html where alternate parameter names have
been specified.}
}
\value{
A vector of HIT ids corresponding to the most recent batch sent to MTurk.
}
\description{
\code{sendComparisons} uploads comparisons onto MTurk for labeling.
}
\details{
The function sends comparisons to MTurk for labeling.
}
\author{
Ryden Butler
}
