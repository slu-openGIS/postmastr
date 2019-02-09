#' Detect Presence of City Name
#'
#' @description Determine the presence of city names in a string.
#'
#' @usage pm_has_city(.data, dictionary, scalar = TRUE)
#'
#' @param .data A postmastr object (\code{pm_subset})
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#' @param scalar If \code{TRUE}, a single logical scalar is returned; otherwise if
#'     \code{FALSE}, a logical vector is returned.
#'
#' @return If \code{scalar = TRUE}, a single logical scalar is returned that is
#'     \code{TRUE} if the data contains a city name from the given dictionary and
#'     \code{FALSE} if they do not. If \code{scalar = FALSE} a tibble with a new
#'     logical variable \code{pm.hasCity} that is \code{TRUE} if a city name from
#'     the given dictionary is found in the last word of the address and
#'     \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom purrr map
#' @importFrom stringr str_c
#'
#' @export
pm_has_city <- function(.data, dictionary, scalar = TRUE){

  # create bindings for global variables
   pm.address = pm.hasCity = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  #
  dict <- dictionary$city.input

  # iterate over observations
  .data %>%
    dplyr::mutate(pm.hasCity = purrr::map(pm.address, ~ pm_has_pattern(.x, dictionary = dict))) %>%
    dplyr::mutate(pm.hasCity = as.logical(pm.hasCity)) -> out

  # return scalar
  if (scalar == TRUE){
    out <- any(out$pm.hasCity)
  }

  # return output
  return(out)

}

#' Return Only Unmatched Observations From pm_has_city
#'
#' @description Automatically subset the results of \link{pm_has_city} to
#'    return only observations that were not found in the dictionary.
#'
#' @usage pm_has_city(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#'
#' @return A tibble containing only observations that were not found in
#'     the dictionary. The variable created by \link{pm_has_city},
#'     \code{pm.hasCity}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_no_city <- function(.data, dictionary){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # create output
  .data %>%
    pm_has_city(dictionary = dictionary, scalar = FALSE) %>%
    dplyr::filter(pm.hasCity == FALSE) %>%
    dplyr::select(-pm.hasCity) -> out

  # return output
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
#' @usage pm_parse_city(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object (\code{pm_subset})
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
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
#' @importFrom stringr str_count
#' @importFrom stringr word
#' @importFrom tidyr unnest
#'
#' @export
pm_parse_city <- function(.data, dictionary, locale = "us"){

  # create bindings for global variables
  pm.uid = pm.city = pm.address = pm.hasCity = NULL

  #
  dict <- dictionary$city.input

  # identify cities
  isCity <- pm_has_city(.data, dictionary = dictionary, scalar = FALSE)

  # subset
  yesCity <- dplyr::filter(isCity, pm.hasCity == TRUE)
  noCity <- dplyr::filter(isCity, pm.hasCity == FALSE)

  # iterate over observations
  yesCity %>%
    dplyr::mutate(pm.city = purrr::map(pm.address, ~ pm_extract_pattern(.x, dictionary = dict))) -> yesCity

  # clean address data
  yesCity %>%
    tidyr::unnest(pm.city) %>%
    dplyr::filter(is.na(pm.city) == FALSE) %>%
    dplyr::mutate(pm.city = as.character(pm.city)) %>%
    dplyr::mutate(pm.address =
                    stringr::word(pm.address, start = 1,
                                  end = -1-stringr::str_count(pm.city, pattern = "\\w+"))) -> yesCity

  # combine with data missing states
  dplyr::bind_rows(yesCity, noCity) %>%
    dplyr::arrange(pm.uid) %>%
    dplyr::select(-pm.hasCity) -> out

  # standardize if data available
  if ("city.output" %in% names(dictionary)){

    out <- pm_std_city(out, var = pm.city, dictionary = dictionary)

  }

  # re-order output
  if (locale == "us"){
    out <- dplyr::select(out, pm.uid, pm.address, pm.city, dplyr::everything())
  }

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
pm_std_city <- function(.data, var, dictionary){

  # create bindings for global variables
  . = city.input = city.output = NULL

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
    dplyr::rename(!!varQ := city.input) -> cityData

  # standardize
  .data %>%
    dplyr::left_join(., cityData, by = varQN) %>%
    dplyr::mutate(!!varQ := ifelse(is.na(city.output) == FALSE, city.output, !!varQ)) %>%
    dplyr::select(-city.output) -> out

  # return output
  return(out)

}

