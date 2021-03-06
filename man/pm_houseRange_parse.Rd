% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/houseRange.R
\name{pm_houseRange_parse}
\alias{pm_houseRange_parse}
\title{Parse Fractional House Numbers}
\usage{
pm_houseRange_parse(.data, expand_range = TRUE, locale = "us")
}
\arguments{
\item{.data}{A postmastr object created with \link{pm_prep}}

\item{expand_range}{A logical scalar; if \code{TRUE} (default), house numbers that
contain a numerical range (i.e. \code{11-15 Main St}) will be expanded to specify
all integer values within the range. Ranges that contain an alphanumeric value
cannot be expanded and will be skipped.}

\item{locale}{A string indicating the country these data represent; the only
current option is "us" but this is included to facilitate future expansion.}
}
\value{
A tibble with a new column \code{pm.house} that contains the house range
    store in a list-column. The list-column will contain the low and
    high values for ranges, and can optionally be expanded to include all integer
    values within a range if \code{expand_range} is equal to \code{TRUE}.
}
\description{
Create a new column containing house range values as a list-column.
}
