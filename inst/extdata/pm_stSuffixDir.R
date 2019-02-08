#' Parse Suffix Direction
#'
#' \code{pm_stSuffixDir} identifies streets with suffix directions (i.e. 123 Washington St NW) and
#' parses those directions into their own variable.
#'
#' @param .data A tbl
#' @param address Name of the street address variable containing suffix directions
#' @param std A logical scalar. Should directional values be standardized?
#' @param output Optional name for output variable
#'
#' @export
pm_stSuffixDir <- function(.data, stName, std = TRUE, output) {

  # save parameters to list
  paramList <- as.list(match.call())

  # reformat stName variable
  if (!is.character(paramList$stName)) {
    var <- rlang::enquo(stName)
  } else if (is.character(paramList$stName)) {
    var <- rlang::quo(!! rlang::sym(stName))
  }

  varQ <- rlang::quo_name(rlang::enquo(var))

  # reformat suffixDir variable
  if (!is.null(paramList$output)) {
    if (!is.character(paramList$output)) {
      newVar <- rlang::enquo(output)
    } else if (is.character(paramList$output)) {
      newVar <- rlang::quo(!! rlang::sym(output))
    }

    newVarQ <- rlang::quo_name(rlang::enquo(newVar))
  }

  # directionals
  dirCardinal <- c("North", "South", "East", "West")
  dirCardinalA <- c("N", "S", "E", "W")
  dirInterCardinal <- c("Northwest", "Northeast", "Southwest", "Southwest")
  dirInterCardinalA <- c("Nw", "Ne", "Sw", "Se")
  dirAll <- c("N", "S", "E", "W", "North", "South", "East", "West", "Nw", "Ne", "Sw", "Se",
              "Northwest", "Northeast", "Southwest", "Southwest")

  # suffix direction
  .data %>%
    dplyr::rename(stName := !!varQ) %>%
    dplyr::mutate(count = stringr::str_count(stName, pattern = "\\S+")) %>%
    dplyr::mutate(stSufDir = ifelse(stringr::word(stName, -1) %in% dirCardinalA,
                                    stringr::word(stName, -1), NA)) %>%
    dplyr::mutate(stSufDir = ifelse(stringr::word(stName, -1) %in% dirCardinal,
                                    stringr::word(stName, -1), stSufDir)) %>%
    dplyr::mutate(stSufDir = ifelse(stringr::word(stName, -1) %in% dirInterCardinal,
                                    stringr::word(stName, -1), stSufDir)) %>%
    dplyr::mutate(stSufDir = ifelse(stringr::word(stName, -1) %in% dirInterCardinalA,
                                    stringr::word(stName, -1), stSufDir)) %>%
    dplyr::mutate(stName = ifelse(!is.na(stSufDir), stringr::word(stName, start = 1, end = count-1), stName)) %>%
    dplyr::select(-count) %>%
    dplyr::rename(!!varQ := stName) -> .data

  # standardize directions
  if (std == TRUE) {
    .data %>%
      dplyr::mutate(stSufDir = ifelse(stSufDir == "Northwest" | stSufDir == "Nw", "NW", stSufDir)) %>%
      dplyr::mutate(stSufDir = ifelse(stSufDir == "Northeast" | stSufDir == "Ne", "NE", stSufDir)) %>%
      dplyr::mutate(stSufDir = ifelse(stSufDir == "Southwest" | stSufDir == "Sw", "SW", stSufDir)) %>%
      dplyr::mutate(stSufDir = ifelse(stSufDir == "Southeast" | stSufDir == "Se", "SE", stSufDir)) %>%
      dplyr::mutate(stSufDir = ifelse(stSufDir == "North", "N", stSufDir)) %>%
      dplyr::mutate(stSufDir = ifelse(stSufDir == "East", "E", stSufDir)) %>%
      dplyr::mutate(stSufDir = ifelse(stSufDir == "South", "S", stSufDir)) %>%
      dplyr::mutate(stSufDir = ifelse(stSufDir == "West", "W", stSufDir)) -> .data
  }

  # rename variable if requested
  if (!is.null(paramList$output)) {
    .data <- rename(.data, !!newVarQ := stSufDir)
  }

  # return
  .data <- dplyr::as_tibble(.data)
  return(.data)
}
