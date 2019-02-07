#' Detect Presence of State Name or Abbreviation
#'
#' @description Determine the presence of state names or abbreviations
#'     at the end of a string.
#'
#' @usage pm_has_state(.data, dictionary, scalar = TRUE, locale = "us")
#'
#' @param .data A postmastr object (\code{pm_subset})
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for states. If none is provided, the \code{states}
#'     object will be used as the default directory.
#' @param scalar If \code{TRUE}, a single logical scalar is returned; otherwise if
#'     \code{FALSE}, a logical vector is returned.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.isState} that is
#'     \code{TRUE} if a state name or abbreviation is found in the address
#'     and \code{FALSE} otherwise.
#'
#' @return If \code{scalar = TRUE}, a single logical scalar is returned that is
#'     \code{TRUE} if the data contain statenames or abbreviations and \code{FALSE}
#'     if they do not. If \code{scalar = FALSE} a tibble with a new logical variable
#'     \code{pm.isState} that is \code{TRUE} if a state name or abbreviation is found
#'     in the last word of the address and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom purrr map
#'
#' @export
pm_has_state <- function(.data, dictionary, scalar = TRUE, locale = "us"){

  # save parameters to list
  paramList <- as.list(match.call())

  # check for object and key variables
  if (pm_has_uid(working_data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(working_data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # create directory
  if (locale == "us"){
    if (missing(dictionary) == FALSE){
      fullDic <- c(datasets::state.abb, datasets::state.name, directory)
    } else if (missing(dictionary) == TRUE){
      fullDic <- c(datasets::state.abb, datasets::state.name)
    }
  }

  # iterate over observations
  if (locale == "us"){
    .data %>%
      dplyr::mutate(pm.hasState = purrr::map(pm.address, ~ pm_has_pattern(.x, dictionary = fullDic))) %>%
      dplyr::mutate(pm.hasState = as.logical(pm.hasState)) -> out
  }

  # return scalar
  if (scalar == TRUE){
    out <- any(out$pm.hasState)
  }

  # return output
  return(out)

}

#' Parse State Names and Abbreviation
#'
#' @description Parse a state name or abbreviation from a string. These data
#'     should be at the end of the string (i.e. the last word or two). If a
#'     zip-code follows the, use \link{pm_parseZip} first to remove those
#'     data from \code{pm.address}.
#'
#' @usage pm_parse_state(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object (\code{pm_subset})
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
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringr word
#' @importFrom tidyr unnest
#'
#' @export
pm_parse_state <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(working_data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(working_data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # create directory
  if (missing(dictionary) == FALSE){
    default.dictionary <- c(datasets::state.abb, datasets::state.name, dictionary)
  } else if (missing(dictionary) == TRUE){
    default.dictionary <- c(datasets::state.abb, datasets::state.name)
  }

  # identify state
  # issues passing dictionary
  isState <- pm_has_state(.data, scalar = FALSE, locale = locale)

  # iterate over observations
  if (locale == "us"){
    out <- pm_parse_state_us(isState, dictionary = default.dictionary)
  }

  # re-order output
  if (locale == "us"){
    out <- dplyr::select(out, pm.uid, pm.address, pm.state, dplyr::everything())
  }

  # return output
  return(out)

}

# parse American states
pm_parse_state_us <- function(.data, dictionary){

  # subset
  yesState <- dplyr::filter(.data, pm.hasState == TRUE)
  noState <- dplyr::filter(.data, pm.hasState == FALSE)

  # iterate over observations
  yesState %>%
    dplyr::mutate(pm.state = purrr::map(pm.address, ~ pm_extract_pattern(.x, dictionary = dictionary))) -> yesState

  # clean address data
  # issues passing dictionary to pm_std_states
  yesState %>%
    tidyr::unnest(pm.state) %>%
    dplyr::filter(is.na(pm.state) == FALSE) %>%
    dplyr::mutate(pm.state = as.character(pm.state)) %>%
    dplyr::mutate(pm.address =
                    stringr::word(pm.address, start = 1,
                                  end = -1-stringr::str_count(pm.state, pattern = "\\w+"))) %>%
    pm_std_states(var = pm.state) -> yesState

  # combine with data missing states
  dplyr::bind_rows(yesState, noState) %>%
    dplyr::arrange(pm.uid) %>%
    dplyr::select(-pm.hasState) -> out

}

#' Standardize Parsed State Names
#'
#' @description Convert state names to the USPS approved two-letter abbreviation.
#'
#' @usage pm_std_states_us(.data, var, dictionary)
#'
#' @param .data A postmastr object (\code{pm_subset})
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
pm_std_states <- function(.data, var, dictionary, locale = "us"){

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
    out <- pm_std_states(.data, var = varQN, dictionary = dictionary)
  }

  # return output
  return(out)

}

# standardize us states
pm_std_states <- function(.data, var, dictionary){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # load state data
    postmastr::states %>%
      dplyr::rename(!!varQ := stateName) -> stateData

  # standardize
  .data %>%
    dplyr::left_join(., stateData, by = varQN) %>%
    dplyr::mutate(!!varQ := ifelse(is.na(stateAbb) == FALSE, stateAbb, !!varQ)) %>%
    dplyr::select(-stateAbb) -> out

  # return output
  return(out)

}
