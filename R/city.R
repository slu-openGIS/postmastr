#' Does City Name Dictionary Return Any Matches
#'
#' @description Determine whether the dictionary returns any matches.
#'
#' @usage pm_city_any(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#'
#' @return If \code{scalar = TRUE}, a single logical scalar is returned that is
#'     \code{TRUE} if the data contains at least one city name from the given
#'     dictionary and \code{FALSE} if they do not.
#'
#' @export
pm_city_any <- function(.data, dictionary){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test dictionary
  .data <- pm_city_detect(.data, dictionary = dictionary)

  # create output
  out <- any(.data$pm.hasCity)

  # return output
  return(out)

}

#' Does City Name Dictionary Return a Match for All Observations
#'
#' @description Determine whether the dictionary returns any matches.
#'
#' @usage pm_city_all(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#'
#' @return If \code{scalar = TRUE}, a single logical scalar is returned that is
#'     \code{TRUE} if the data contains at least one city name from the given
#'     dictionary and \code{FALSE} if they do not.
#'
#' @export
pm_city_all <- function(.data, dictionary){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test dictionary
  .data <- pm_city_detect(.data, dictionary = dictionary)

  # create output
  out <- all(.data$pm.hasCity)

  # return output
  return(out)

}

#' Detect Presence of City Name in Address
#'
#' @description Determine the presence of city names in a string.
#'
#' @usage pm_city_detect(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#'
#' @return A tibble with a new logical variable \code{pm.hasCity} that is
#'     \code{TRUE} if a city name from the given dictionary is found in the
#'     at the end the address and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#'
#' @export
pm_city_detect <- function(.data, dictionary){

  # create bindings for global variables
   pm.address = pm.hasCity = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # minimize dictionary
  dict <- paste(dictionary$city.input, collapse = "|")

  # check observations
  .data <- dplyr::mutate(.data, pm.hasCity = stringr::str_detect(pm.address,
                                                                 pattern = stringr::str_c("\\b(", dict, ")\\b$")))

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_city_detect
#'
#' @description Automatically subset the results of \link{pm_city_detect} to
#'    return only observations that were not found in the dictionary.
#'
#' @usage pm_city_none(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#'
#' @return A tibble containing only observations that were not found in
#'     the dictionary. The variable created by \link{pm_city_detect},
#'     \code{pm.hasCity}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_city_none <- function(.data, dictionary){

  # global bindings
  pm.hasCity = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # create output
  .data %>%
    pm_city_detect(dictionary = dictionary) %>%
    dplyr::filter(pm.hasCity == FALSE) %>%
    dplyr::select(-pm.hasCity) -> out

  # return output
  return(out)

}

#' Parse City Names
#'
#' @description Parse a city name or abbreviation from a string. These data
#'     should be at the end of the string (i.e. the last several words). If a
#'     state name or abbrevation follows the city, use \link{pm_state_parse} first
#'     to remove those data from \code{pm.address}. Likewise, if a
#'     postal code follows a name, use \link{pm_postal_parse} first to remove those
#'     data from \code{pm.address}.
#'
#' @usage pm_city_parse(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
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
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_count
#' @importFrom stringr word
#'
#' @export
pm_city_parse <- function(.data, dictionary, locale = "us"){

  # create bindings for global variables
  pm.uid = pm.city = pm.address = pm.hasCity = NULL

  # minimize dictionary
  dict <- paste(dictionary$city.input, collapse = "|")

  # parse
  .data <- dplyr::mutate(.data, pm.city =
                           stringr::str_extract(pm.address,
                                                pattern = stringr::str_c("\\b(", dict, ")\\b$")))

  # clean address data
  .data %>%
    dplyr::mutate(pm.address = ifelse(is.na(pm.city) == FALSE,
                                      stringr::word(pm.address, start = 1,
                                                    end = -1-stringr::str_count(pm.city, pattern = "\\w+")), pm.address)) -> .data

  # standardize if data available
  if ("city.output" %in% names(dictionary)){
    .data <- pm_city_std(.data, var = pm.city, dictionary = dictionary)
  }

  # re-order output
  if (locale == "us"){
    vars <- pm_reorder(.data)
    .data <- dplyr::select(.data, vars)
  }

  # return output
  return(.data)

}

#' Standardize Parsed City Names
#'
#' @description Convert state names to the USPS approved two-letter abbreviation.
#'
#' @usage pm_city_std(.data, var, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}, a tbl, or a data frame
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
pm_city_std <- function(.data, var, dictionary){

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

