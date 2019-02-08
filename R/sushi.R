#' Sushi Resturants in the St. Louis Region
#'
#' @description A selection of sushi resturants in the St. Louis, Missouri region.
#'    These data are in long form, with multiple visits in a single year to some
#'    of the resturants. They also contain a degree of messiness, with a mix of
#'    upper and lower case tying, some street types abbreviated (i.e. "St") and
#'    others entered in full (i.e. "Street"), and an example of two ways in which
#'    unit numbers may appear. State names are given both in full and using the
#'    standard two-letter abbreviation, and city names as well as postal codes
#'    are included. Postal codes are given in both the standard five digit
#'    form as well as the elongated "zip+4" format.
#'
#' @docType data
#'
#' @usage data(sushi1)
#'
#' @format A tibble with 30 rows and 3 variables:
#' \describe{
#'   \item{name}{resturant name}
#'   \item{address}{street address include city, state, and postal code}
#'   \item{visit}{visit date}
#' }
#'
#' @examples
#' str(sushi1)
#' head(sushi1)
#'
"sushi1"

#' Sushi Resturants in the Ciy of St. Louis
#'
#' @description A selection of sushi resturants in the St. Louis, Missouri region.
#'    These data are in long form, with multiple visits in a single year to some
#'    of the resturants. They also contain a degree of messiness, with a mix of
#'    upper and lower case tying, some street types abbreviated (i.e. "St") and
#'    others entered in full (i.e. "Street"), and an example of two ways in which
#'    unit numbers may appear. Unlike \code{sushi1}, these data do not contain
#'    city, state, or postal code data.
#'
#' @docType data
#'
#' @usage data(sushi2)
#'
#' @format A tibble with 19 rows and 3 variables:
#' \describe{
#'   \item{name}{resturant name}
#'   \item{address}{street address}
#'   \item{visit}{visit date}
#' }
#'
#' @examples
#' str(sushi2)
#' head(sushi2)
#'
"sushi2"
