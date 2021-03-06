% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intersection.R
\name{pm_intersect_wider}
\alias{pm_intersect_wider}
\title{Covert Intersections to Wide Form}
\usage{
pm_intersect_wider(.data, locale = "us")
}
\arguments{
\item{.data}{A postmastr object created with \link{pm_prep} that has also been
modified with \link{pm_intersect_longer} and parsed with the necessary functions.}

\item{locale}{A string indicating the country these data represent; the only
current option is \code{"us"} but this is included to facilitate future expansion.}
}
\description{
Convert a parsed intersection object into wide form.
}
