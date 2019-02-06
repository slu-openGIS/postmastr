#' Detect Presence of City Name
#'
#' @description Determine the presence of city names in a string.
#'
#' @param .data A tbl or data frame
#' @param var A character variable that may contain city names
#'
#' @return A tibble with a new logical variable \code{pm.isCity} that is
#'     \code{TRUE} if a city name is found in the address and \code{FALSE}
#'     otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
#' @export
pm_isCity <- function(.data, var, directory){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # iterate over observations
  .data %>%
    dplyr::mutate(pm.isCity = purrr::map(!!varQ, ~pm_idCity(.x, directory = directory))) -> out

  return(out)

}

# iterate over directory items to identify city names
pm_idCity <- function(x, directory){

  # create pattern vector
  patternVector <- directory

  patternVector %>%
    base::split(patternVector) %>%
    purrr::map_lgl( ~ stringr::str_detect(x, pattern = .x)) %>%
    any() -> out

  return(out)

}
