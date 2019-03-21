#' Does Country Dictionary Return Any Matches
#'
#' @description Determine whether the country dictionary returns any matches.
#'
#' @usage pm_country_any(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for countries. If none is specified, the full default
#'     country dictionary will be used.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at
#'     least one country name or abbrevation in the given dictionary and \code{FALSE}
#'     if they do not.
#'
#' @export
pm_country_any <- function(.data, dictionary){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_country_detect(.data)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_country_any(.data, dictionary = dictionary)
  }

  # create output
  out <- any(.data$pm.hasCountry)

  # return output
  return(out)

}

#' Does Country Dictionary Return a Match for All Observations
#'
#' @description Determine whether the country dictionary returns matches for all observations.
#'
#' @usage pm_country_all(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for countries. If none is specified, the full default
#'     country dictionary will be used.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains a country
#'     name or abbreviation for every observation in the data set and \code{FALSE} otherwise.
#'
#' @export
pm_country_all <- function(.data, dictionary){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_country_detect(.data)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_country_any(.data, dictionary = dictionary)
  }

  # create output
  out <- all(.data$pm.hasCountry)

  # return output
  return(out)

}

#' Detect Presence of Country
#'
#' @description Determine the presence of country names or abbreviations
#'     at the end of a string.
#'
#' @usage pm_country_detect(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for countries. If none is specified, the full default
#'     country dictionary will be used.
#'
#' @return A tibble with a new logical variable \code{pm.hasCountry} that is
#'     \code{TRUE} if a country name or abbreviation from the given dictionary is
#'     found at the end of the address and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#'
#' @export
pm_country_detect <- function(.data, dictionary){

  # create bindings for global variables
  pm.address = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    dictionary <- pm_dictionary(type = "country")
  }

  # minimize dictionary
  dict <- paste(dictionary$con.input, collapse = "|")

  # check observations
  .data <- dplyr::mutate(.data, pm.hasCountry = stringr::str_detect(pm.address,
                                                                      pattern = stringr::str_c("\\b(", dict, ")\\b$")))

  # return output
  return(.data)

}


#' Return Only Unmatched Observations From pm_country_detect
#'
#' @description Automatically subset the results of \link{pm_country_detect} to
#'    return only observations that were not found in the dictionary.
#'
#' @usage pm_country_none(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for countries. If none is specified, the full default
#'     country dictionary will be used.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_country_none <- function(.data, dictionary){

  # global bindings
  pm.hasCountry = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    dictionary <- pm_dictionary(type = "country")
  }

  # create output
  .data %>%
    pm_country_detect(dictionary = dictionary) %>%
    dplyr::filter(pm.hasCountry == FALSE) %>%
    dplyr::select(-pm.hasCountry) -> .data

  # return output
  return(.data)

}

#' Parse Country
#'
#' @description Parse a country from a string. These data
#'     should be at the end of the string (i.e. the last word or words).
#'
#' @usage pm_country_parse(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for countries. If none is specified, the full default
#'     country dictionary will be used.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringr word
#'
#' @export
pm_country_parse <- function(.data, dictionary){

  # create bindings for global variables
  pm.address = pm.country = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    dictionary <- pm_dictionary(type = "country")
  }

  # load dictionary if NULL
  if (is.null(dictionary) == TRUE){
    dictionary <- pm_dictionary(type = "country")
  }

  # minimize dictionary
  dict <- paste(dictionary$con.input, collapse = "|")

  # parse countries
  ## parse
  .data <- dplyr::mutate(.data, pm.country =
                           stringr::str_extract(pm.address,
                                                pattern = stringr::str_c("\\b(", dict, ")\\b$")))

  ## clean address data
  .data %>%
    dplyr::mutate(pm.address = ifelse(is.na(pm.country) == FALSE,
                                      stringr::word(pm.address, start = 1, end = -1-stringr::str_count(pm.country, pattern = "\\w+")),
                                      pm.address)) %>%
    pm_country_std(var = pm.country, dictionary = dictionary) -> .data

  # re-order data
  vars <- pm_reorder(.data)
  .data <- dplyr::select(.data, vars)

  # return output
  return(.data)

}

#' Standardize Parsed Countries
#'
#' @description Convert countries to USPS preferred two-letter abbreviation.
#'
#' @usage pm_country_std(.data, var, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable that may contain countries
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for countries. If none is specified, the full default
#'     country dictionary will be used.
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
pm_country_std <- function(.data, var, dictionary){

  # global variables
  . = con.input = con.output = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    dictionary <- pm_dictionary(type = "country")
  }

  # modify dictionary
  dictionary %>%
    dplyr::rename(!!varQ := con.input) -> dictionary

  # standardize country names
  .data %>%
    dplyr::left_join(., dictionary, by = varQN) %>%
    dplyr::mutate(!!varQ := ifelse(is.na(con.output) == FALSE, con.output, !!varQ)) %>%
    dplyr::select(-con.output) -> out

  # return output
  return(out)

}

#' Trim Country
#'
#' @description Remove a country from an address without parsing. These data
#'     should be at the end of the string (i.e. the last word or words).
#'
#' @usage pm_country_trim(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for countries. If none is specified, the full default
#'     country dictionary will be used.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringr word
#'
#' @export
pm_country_trim <- function(.data, dictionary){

  # create bindings for global variables
  pm.address = pm.country = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    dictionary <- pm_dictionary(type = "country")
  }

  # load dictionary if NULL
  if (is.null(dictionary) == TRUE){
    dictionary <- pm_dictionary(type = "country")
  }

  # minimize dictionary
  dict <- paste(dictionary$con.input, collapse = "|")

  # parse countries
  ## parse
  .data <- dplyr::mutate(.data, pm.country =
                           stringr::str_extract(pm.address,
                                                pattern = stringr::str_c("\\b(", dict, ")\\b$")))

  ## clean address data
  .data %>%
    dplyr::mutate(pm.address = ifelse(is.na(pm.country) == FALSE,
                                      stringr::word(pm.address, start = 1, end = -1-stringr::str_count(pm.country, pattern = "\\w+")),
                                      pm.address)) -> .data

  # re-order data
  .data <- dplyr::select(.data, -pm.country)

  # return output
  return(.data)

}
