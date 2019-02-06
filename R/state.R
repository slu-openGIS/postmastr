#' Detect Presence of State Name or Abbreviation
#'
#' @description Determine the presence of state names or abbreviations
#'     in a string.
#'
#' @param .data A tbl or data frame
#' @param var A character variable that may contain city names
#' @param directory Optional; a tbl created with \code{pm_directory} to be used
#'     as a master list for states. If none is provided, the \code{states}
#'     object will be used as the default directory.
#'
#' @return A tibble with a new logical variable \code{pm.isState} that is
#'     \code{TRUE} if a state name or abbreviation is found in the address
#'     and \code{FALSE} otherwise.
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
pm_isState <- function(.data, var, directory){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # create directory
  if (missing(directory) == FALSE){
    fulLDir <- c(datasets::state.abb, datasets::state.name, directory)
  } else if (missing(directory) == TRUE){
    fulLDir <- c(datasets::state.abb, datasets::state.name)
  }

  # iterate over observations
  .data %>%
    dplyr::mutate(pm.isState = purrr::map(!!varQ, ~pm_idState(.x, directory = fulLDir))) -> out

  # return output
  return(out)

}

# iterate over directory items to identify state names and abbreviations
pm_idState <- function(x, directory){

  # create pattern vector
  patternVector <- directory

  patternVector %>%
    base::split(patternVector) %>%
    purrr::map_lgl( ~ stringr::str_detect(x, pattern = .x)) %>%
    any() -> out

  return(out)

}

#' Parse State Names and Abbreviation
#'
#' @description Parse a state name or abbreviation from a string. These data
#'     should be at the end of the string (i.e. the last word or two). If a
#'     zip-code follows the, use \link{pm_parseZip} first to remove those
#'     data from \code{pm.address}.
#'
#' @param .data A tbl or data frame
#' @param var A character variable that may contain city names
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for states. If none is provided, the \code{states}
#'     object will be used as the default directory.
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
pm_parseState <- function(.data, var, dictionary){

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
  pm_isState(.data, "pm.address") %>%
    tibble::rowid_to_column(var = "pm.id") -> isState

  # subset
  yesState <- dplyr::filter(isState, pm.isState == TRUE)
  noState <- dplyr::filter(isState, pm.isState == FALSE)

  # create directory
  if (missing(dictionary) == FALSE){
    fulLDic <- c(datasets::state.abb, datasets::state.name, directory)
  } else if (missing(dictionary) == TRUE){
    fulLDic <- c(datasets::state.abb, datasets::state.name)
  }

  # iterate over observations
  yesState %>%
    dplyr::mutate(pm.state = purrr::map(!!varQ, ~ pm_extractState(.x, dictionary = fulLDic))) -> yesState

  # clean address data
  yesState %>%
    tidyr::unnest(pm.state) %>%
    dplyr::filter(is.na(pm.state) == FALSE) %>%
    dplyr::mutate(pm.state = as.character(pm.state)) %>%
    dplyr::mutate(pm.address =
                    ifelse(stringr::str_count(pm.state, patter = '\\w+') == 1,
                           stringr::word(pm.address, start = 1, end = -2),
                           pm.address)) %>%
    dplyr::mutate(pm.address =
                    ifelse(stringr::str_count(pm.state, patter = '\\w+') == 2,
                           stringr::word(pm.address, start = 1, end = -3),
                           pm.address)) %>%
    dplyr::mutate(pm.address = stringr::str_replace(pm.address, ",", "")) %>%
    pm_stdState(var = pm.state, dictionary = fullDic) -> yesState

  # combine with data missing states
  dplyr::bind_rows(yesState, noState) %>%
    dplyr::arrange(pm.id) %>%
    dplyr::select(-pm.id, -pm.isState) -> out

  # return output
  return(out)

}

# iterate over dictionary items per observations
pm_extractState <- function(x, dictionary){

  # create pattern vector
  patternVector <- dictionary

  patternVector %>%
    base::split(patternVector) %>%
    purrr::map( ~ stringr::str_extract(x, pattern = stringr::str_c("\\b", .x, "\\b"))) -> out

  return(out)

}

#' Standardize Parsed State Names
#'
#' @description Convert state names to the USPS approved two-letter abbreviation.
#'
#' @param .data A tbl or data frame
#' @param var A character variable that may contain city names
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for states. If none is provided, the \code{states}
#'     object will be used as the default directory.
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
pm_stdState <- function(.data, var, dictionary){

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

