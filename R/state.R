#' Does State Dictionary Return Any Matches
#'
#' @description Determine whether the dictionary returns any matches.
#'
#' @usage pm_state_any(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at
#'     least one state name or abbreviation the given dictionary and \code{FALSE}
#'     if they do not.
#'
#' @export
pm_state_any <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_state_detect(.data, locale = locale)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_state_detect(.data, dictionary = dictionary, locale = locale)
  }

  # create output
  out <- any(.data$pm.hasState, na.rm = TRUE)

  # return output
  return(out)

}

#' Does State Dictionary Return a Match for All Observations
#'
#' @description Determine whether the dictionary returns any matches.
#'
#' @usage pm_state_all(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains a state
#'     name or abbreviation for every observation in the data set and \code{FALSE} otherwise.
#'
#' @export
pm_state_all <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_state_detect(.data, locale = locale)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_state_detect(.data, dictionary = dictionary, locale = locale)
  }

  # create output
  out <- all(.data$pm.hasState, na.rm = TRUE)

  # return output
  return(out)

}

#' Detect Presence of State Name or Abbreviation
#'
#' @description Determine the presence of state names or abbreviations
#'     at the end of a string.
#'
#' @usage pm_state_detect(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for states. If none is provided, the \code{states}
#'     object will be used as the default directory.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.hasState} that is
#'     \code{TRUE} if a state name or abbreviation from the given dictionary is found
#'     at the end of the address and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#'
#' @export
pm_state_detect <- function(.data, dictionary, locale = "us"){

  # create bindings for global variables
  pm.address = pm.hasState = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- pm_dictionary(type = "state")
    }
  }

  # minimize dictionary
  if (locale == "us"){
    dict <- paste(dictionary$state.input, collapse = "|")
  }

  # check observations
  if (locale == "us"){
    .data <- dplyr::mutate(.data, pm.hasState = stringr::str_detect(pm.address,
                                                                    pattern = stringr::str_c("\\b(", dict, ")\\b$")))
  }

  # return output
  return(.data)

}


#' Return Only Unmatched Observations From pm_state_detect
#'
#' @description Automatically subset the results of \link{pm_state_detect} to
#'    return only observations that were not found in the dictionary.
#'
#' @usage pm_state_none(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble containing only observations that were not found in
#'     the dictionary. The variable created by \link{pm_state_detect},
#'     \code{pm.hasState}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_state_none <- function(.data, dictionary, locale = "us"){

  # global bindings
  pm.hasState = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- pm_dictionary(type = "state")
    }
  }

  # create output
  .data %>%
    pm_state_detect(dictionary = dictionary, locale = locale) %>%
    dplyr::filter(pm.hasState == FALSE) %>%
    dplyr::select(-pm.hasState) -> out

  # return output
  return(out)

}

#' Parse State Names and Abbreviation
#'
#' @description Parse a state name or abbreviation from a string. These data
#'     should be at the end of the string (i.e. the last word or two). If a
#'     postal code follows the, use \link{pm_postal_parse} first to remove those
#'     data from \code{pm.address}.
#'
#' @usage pm_state_parse(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for states. If none is provided, the \code{states}
#'     object will be used as the default directory.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new character variable \code{pm.state} that contains
#'     the two-letter abbreviation for the given U.S. state. This follows USPS
#'     addressing standards, which require that the state abbreviation be used.
#'     If a state name or abbreviation is not detected in the string, a value
#'     of \code{NA} will be returned. If it does not yet exist, a copy of the
#'     address variable will be created in \code{pm.address} and returned with
#'     state name or abbreviation removed.
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
pm_state_parse <- function(.data, dictionary, locale = "us"){

  # create bindings for global variables
  pm.address = pm.state = pm.uid = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- pm_dictionary(type = "state")
    }
  }

  # load dictionary if NULL
  if (is.null(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- pm_dictionary(type = "state")
    }
  }

  # parse states
  if (locale == "us"){
    .data <- pm_parse_state_us(.data, dictionary = dictionary)
    vars <- pm_reorder(.data)
    .data <- dplyr::select(.data, vars)
  }

  # return output
  return(.data)

}

# parse American states
pm_parse_state_us <- function(.data, dictionary){

  # minimize dictionary
  dict <- paste(dictionary$state.input, collapse = "|")

  # create bindings for global variables
  pm.address = pm.state = pm.uid = pm.hasState = NULL

  # parse
  .data <- dplyr::mutate(.data, pm.state =
                           stringr::str_extract(pm.address,
                                                pattern = stringr::str_c("\\b(", dict, ")\\b$")))

  # clean address data
  .data %>%
    dplyr::mutate(pm.address = ifelse(is.na(pm.state) == FALSE,
                                      stringr::word(pm.address, start = 1,
                                                    end = -1-stringr::str_count(pm.state, pattern = "\\w+")), pm.address)) %>%
    pm_state_std(var = pm.state, dictionary = dictionary) -> .data

}

#' Standardize Parsed State Names
#'
#' @description Convert state names to the USPS approved two-letter abbreviation.
#'
#' @usage pm_state_std(.data, var, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable that may contain city names
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for states. If none is provided, the \code{states}
#'     object will be used as the default directory.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with an updated variable that contains the two-letter abbreviation
#'     for the given U.S. state. This follows USPS addressing standards, which require
#'     that the state abbreviation be used.
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
pm_state_std <- function(.data, var, dictionary, locale = "us"){

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

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- pm_dictionary(type = "state")
    }
  }

  # standardize state names
  if (locale == "us"){
    out <- pm_std_states_us(.data, var = !!varQ, dictionary = dictionary)
  }

  # return output
  return(out)

}

# standardize us states
pm_std_states_us <- function(.data, var, dictionary){

  # create bindings for global variables
  . = state.input = state.output = NULL

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
    dplyr::rename(!!varQ := state.input) -> dictionary

  # standardize
  .data %>%
    dplyr::left_join(., dictionary, by = varQN) %>%
    dplyr::mutate(!!varQ := ifelse(is.na(state.output) == FALSE, state.output, !!varQ)) %>%
    dplyr::select(-state.output) -> out

  # return output
  return(out)

}
