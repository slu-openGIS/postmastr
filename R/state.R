#' Detect Presence of State Name or Abbreviation
#'
#' @description Determine the presence of state names or abbreviations
#'     in a string.
#'
#' @param .data A tbl or data frame
#' @param var A character variable that may contain city names
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
#' @export
pm_parseState <- function(.data, var, directory){

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
  if (missing(directory) == FALSE){
    fulLDir <- c(datasets::state.abb, datasets::state.name, directory)
  } else if (missing(directory) == TRUE){
    fulLDir <- c(datasets::state.abb, datasets::state.name)
  }

  # iterate over observations
  yesState %>%
    dplyr::mutate(pm.state = purrr::map(!!varQ, ~ pm_extractState(.x, directory = fulLDir))) -> yesState

  # load state data
  postmastr::states %>%
    dplyr::rename(pm.state = stateName) -> stateData

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
    dplyr::left_join(., stateData, by = "pm.state") %>%
    dplyr::mutate(pm.state = ifelse(is.na(stateAbb) == FALSE, stateAbb, pm.state)) %>%
    dplyr::select(-stateAbb) -> yesState

  # combine with data missing states
  dplyr::bind_rows(yesState, noState) %>%
    dplyr::arrange(pm.id) %>%
    dplyr::select(-pm.id, -pm.isState) -> out

  # return output
  return(out)

}

#
pm_extractState <- function(x, directory){

  # create pattern vector
  patternVector <- directory

  patternVector %>%
    base::split(patternVector) %>%
    purrr::map( ~ stringr::str_extract(x, pattern = stringr::str_c("\\b", .x, "\\b"))) -> out

  return(out)

}
