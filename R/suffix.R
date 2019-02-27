#' Does Street Suffix Dictionary Return Any Matches
#'
#' @description Determine whether the street suffix dictionary returns any matches.
#'
#' @usage pm_any_street_suf(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for street suffixes
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at
#'     least one street suffix name or abbrevation in the given dictionary and \code{FALSE}
#'     if they do not.
#'
#' @export
pm_any_street_suf <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_has_street_suf(.data, locale = locale)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_has_street_suf(.data, dictionary = dictionary, locale = locale)
  }

  # create output
  out <- any(.data$pm.hasStreetSuf)

  # return output
  return(out)

}

#' Does State Dictionary Return a Match for All Observations
#'
#' @description Determine whether the street suffix dictionary returns matches for all observations.
#'
#' @usage pm_all_street_suf(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for street suffixes
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains a street suffix
#'     name or abbreviation for every observation in the data set and \code{FALSE} otherwise.
#'
#' @export
pm_all_street_suf <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_has_street_suf(.data, locale = locale)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_has_street_suf(.data, dictionary = dictionary, locale = locale)
  }

  # create output
  out <- all(.data$pm.hasStreetSuf)

  # return output
  return(out)

}

#' Detect Presence of State Name or Abbreviation
#'
#' @description Determine the presence of street suffix names or abbreviations
#'     at the end of a string.
#'
#' @usage pm_has_street_suf(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for street suffixes
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.hasStreetSuf} that is
#'     \code{TRUE} if a street suffix name or abbreviation from the given dictionary is
#'     found at the end of the address and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#'
#' @export
pm_has_street_suf <- function(.data, dictionary, locale = "us"){

  # create bindings for global variables
  pm.address = pm.hasState = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # minimize dictionary
  if (locale == "us"){
    dict <- paste(dictionary$suf.input, collapse = "|")
  }

  # check observations
  if (locale == "us"){
    .data <- dplyr::mutate(.data, pm.hasStreetSuf = stringr::str_detect(pm.address,
                                                                    pattern = stringr::str_c("\\b(", dict, ")\\b$")))
  }

  # return output
  return(.data)

}


#' Return Only Unmatched Observations From pm_has_street_suf
#'
#' @description Automatically subset the results of \link{pm_has_state} to
#'    return only observations that were not found in the dictionary.
#'
#' @usage pm_no_street_suf(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for street suffixes.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_no_street_suf <- function(.data, dictionary, locale = "us"){

  # global bindings
  pm.hasStreetSuf = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # create output
  .data %>%
    pm_has_street_suf(dictionary = dictionary, locale = locale) %>%
    dplyr::filter(pm.hasStreetSuf == FALSE) %>%
    dplyr::select(-pm.hasStreetSuf) -> out

  # return output
  return(out)

}

#' Parse State Names and Abbreviation
#'
#' @description Parse a state name or abbreviation from a string. These data
#'     should be at the end of the string (i.e. the last word).
#'
#' @usage pm_parse_street_suf(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for street suffixes.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
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
pm_parse_street_suf <- function(.data, dictionary, locale = "us"){

  # create bindings for global variables
  pm.address = pm.streetSuf = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # parse states
  if (locale == "us"){
    .data <- pm_parse_suf_us(.data, dictionary = dictionary)
    vars <- pm_reorder(.data)
    .data <- dplyr::select(.data, vars)
  }

  # return output
  return(.data)

}

# parse American states
pm_parse_suf_us <- function(.data, dictionary){

  # minimize dictionary
  dict <- paste(dictionary$suf.input, collapse = "|")

  # create bindings for global variables
  pm.address = pm.streetSuf = NULL

  # parse
  .data <- dplyr::mutate(.data, pm.streetSuf =
                           stringr::str_extract(pm.address,
                                                pattern = stringr::str_c("\\b(", dict, ")\\b$")))

  # clean address data
  .data %>%
    dplyr::mutate(pm.address = ifelse(is.na(pm.streetSuf) == FALSE, stringr::word(pm.address, start = 1, end = -2), pm.address)) %>% # -> .data # %>%
    pm_std_street_suf(var = pm.streetSuf, dictionary = dictionary) -> .data

}

#' Standardize Parsed Street Suffixes
#'
#' @description Convert street suffixes to USPS preferred abbreviation.
#'
#' @usage pm_std_street_suf(.data, var, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable that may contain street suffixes
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for street suffixes.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
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
pm_std_street_suf <- function(.data, var, dictionary, locale = "us"){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # standardize state names
  if (locale == "us"){
    out <- pm_std_suf_us(.data, var = !!varQ, dictionary = dictionary)
  }

  # return output
  return(out)

}

# standardize us street suffixes
pm_std_suf_us <- function(.data, var, dictionary){

  # create bindings for global variables
  . = suf.input = suf.output = suf.type = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  dictionary %>%
    dplyr::rename(!!varQ := suf.input) -> dictionary

  # standardize
  .data %>%
    dplyr::left_join(., dictionary, by = varQN) %>%
    dplyr::mutate(!!varQ := ifelse(is.na(suf.output) == FALSE, suf.output, !!varQ)) %>%
    dplyr::select(-suf.output, -suf.type) -> out

  # return output
  return(out)

}
