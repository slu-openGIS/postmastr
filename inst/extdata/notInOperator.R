#' Not In Operator
#'
#' Provides the compliment to the base R \code{\%in\%} operator. Included here instead of via import
#' due to stability issues with the source package, \href{https://github.com/harrelfe/Hmisc/blob/master/R/in.operator.s}{\code{Hmsic}},
#' during original package development in October, 2017. Used under terms of
#' \href{https://cran.r-project.org/web/packages/Hmisc/index.html}{\code{Hmisc}}'s
#' \href{https://cran.r-project.org/web/licenses/GPL-3}{GPL-3 License}.
#'
#' @param x vector or \code{NULL}: the values to be matched
#' @param y vector or \code{NULL}: the values to be matched against
#'
#' @source \href{https://github.com/harrelfe/Hmisc/blob/master/R/in.operator.s}{\code{Hmsic}}
#'
#' @examples
#' x <- 2
#' y <- 2
#' z <- 3
#'
#' x %in% y
#' x %nin% y
#'
#' x %in% z
#' x %nin% z
#'
#' @export
"%nin%" <- function(x, y) match(x, y, nomatch = 0) == 0
