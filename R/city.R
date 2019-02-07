#' Detect Presence of City Name
#'
#' @description Determine the presence of city names in a string.
#'
#' @usage pm_has_city(.data, dictionary)
#'
#' @param .data A postmastr object (\code{pm_subset})
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#'
#' @return A tibble with a new logical variable \code{pm.isCity} that is
#'     \code{TRUE} if a city name is found in the address and \code{FALSE}
#'     otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom stringr str_c
#'
#' @export
pm_has_city <- function(.data, var, dictionary){

  # check for object and key variables
  if (pm_has_uid(working_data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(working_data) == FALSE){
    stop("Error 3.")
  }

  # iterate over observations
  .data %>%
    dplyr::mutate(pm.hasCity = purrr::map(pm.address, ~ pm_has_pattern(.x, dictionary = dictionary))) %>%
    dplyr::mutate(pm.hasCity = as.logical(pm.hasCity)) -> out

  return(out)

}

#' Parse City Names
#'
#' @description Parse a city name or abbreviation from a string. These data
#'     should be at the end of the string (i.e. the last several words). If a
#'     state name or abbrevation follows the city, use \link{pm_parseState} first
#'     to remove those data from \code{pm.address}. Likewise, if a
#'     zip-code follows a name, use \link{pm_parseZip} first to remove those
#'     data from \code{pm.address}.
#'
#' @usage pm_parse_city(.data, dictionary)
#'
#' @param .data A postmastr object (\code{pm_subset})
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#'
#' @return A tibble with a new character variable \code{pm.city} that contains
#'     the city name. If a city name is not detected in the string, a value
#'     of \code{NA} will be returned. If it does not yet exist, a copy of the
#'     address variable will be created in \code{pm.address} and returned with
#'     state name or abbreviation removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringr word
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr unnest
#'
#' @export
pm_parse_city <- function(.data, dictionary){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # create pm.origAddress
  if ("pm.address" %in% names(.data) == FALSE){

    .data <- dplyr::mutate(.data, pm.address := !!varQ)

  }

  # identify state
  pm_isCity(.data, "pm.address", dictionary = dictionary) %>%
    tibble::rowid_to_column(var = "pm.id") -> isCity

  # subset
  yesCity <- dplyr::filter(isCity, pm.isCity == TRUE)
  noCity <- dplyr::filter(isCity, pm.isCity == FALSE)

  # iterate over observations
  yesCity %>%
    dplyr::mutate(pm.city = purrr::map(!!varQ, ~ pm_extract_pattern(.x, dictionary = dictionary))) -> yesCity

  # clean address data
  yesCity %>%
    tidyr::unnest(pm.city) %>%
    dplyr::filter(is.na(pm.city) == FALSE) %>%
    dplyr::mutate(pm.city = as.character(pm.city)) %>%
    dplyr::mutate(pm.address =
                    ifelse(stringr::str_count(pm.city, pattern = '\\w+') == 1,
                           stringr::word(pm.address, start = 1, end = -2),
                           pm.address)) %>%
    dplyr::mutate(pm.address =
                    ifelse(stringr::str_count(pm.city, pattern = '\\w+') == 2,
                           stringr::word(pm.address, start = 1, end = -3),
                           pm.address)) %>%
    dplyr::mutate(pm.address =
                    ifelse(stringr::str_count(pm.city, pattern = '\\w+') == 3,
                           stringr::word(pm.address, start = 1, end = -4),
                           pm.address)) %>%
    dplyr::mutate(pm.address =
                    ifelse(stringr::str_count(pm.city, pattern = '\\w+') == 4,
                           stringr::word(pm.address, start = 1, end = -5),
                           pm.address)) %>%
    dplyr::mutate(pm.address =
                    ifelse(stringr::str_count(pm.city, pattern = '\\w+') == 5,
                           stringr::word(pm.address, start = 1, end = -6),
                           pm.address)) %>%
    dplyr::mutate(pm.address =
                    ifelse(stringr::str_count(pm.city, pattern = '\\w+') == 6,
                           stringr::word(pm.address, start = 1, end = -7),
                           pm.address)) -> yesCity

  # combine with data missing states
  dplyr::bind_rows(yesCity, noCity) %>%
    dplyr::arrange(pm.id) %>%
    dplyr::select(-pm.id, -pm.isCity) -> out

  # return output
  return(out)

}

#' Standardize Parsed City Names
#'
#' @description Convert state names to the USPS approved two-letter abbreviation.
#'
#' @param .data A tbl or data frame
#' @param var A character variable that may contain city names
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#'
#' @return A tibble with an updated variable that contains the corrected city name.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
#' @export
pm_stdCity <- function(.data, var, dictionary){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # prepare data
  dictionary %>%
    dplyr::rename(!!varQ := cityGiven) -> cityData

  # standardize
  .data %>%
    dplyr::left_join(., cityData, by = varQN) %>%
    dplyr::mutate(!!varQ := ifelse(is.na(cityCorrect) == FALSE, cityCorrect, !!varQ)) %>%
    dplyr::select(-cityCorrect) -> out

  # return output
  return(out)

}

