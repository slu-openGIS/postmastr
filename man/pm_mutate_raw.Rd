% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutate.R
\name{pm_mutate_raw}
\alias{pm_mutate_raw}
\title{Modify Raw Addresses}
\usage{
pm_mutate_raw(.data, uid, var, new_val)
}
\arguments{
\item{.data}{A source tibble that has already had identification
numbers added using \link{pm_identify}.}

\item{uid}{A \code{pm.uid} value to edit.}

\item{var}{A character variable containing address data to be edited}

\item{new_val}{The new value for the for the given address variable.}
}
\description{
Clean raw address data before parsing to improve the parsing process. This
    may be particularly advantageous for addresses identified as \code{"partial"} or
    \code{"unknown"} in \code{pm.type}.
}
\seealso{
\code{\link{pm_identify}}
}
