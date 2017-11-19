#' Parse Prefix Direction
#'
#' \code{pm_stSuffixDir} identifies streets with suffix directions (i.e. 123 N Washington St) and
#' parses those directions into their own variable.
#'
#' @param .data A tbl
#' @param address Name of the street address variable containing prefix directions
#' @param std A logical scalar. Should directional values be standardized?
#' @param output Optional name for output variable
#'
#' @export
pm_stPrefixDir <- function(.data, stName, std = TRUE, output) {

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

  # prefix direction
  .data %>%
    dplyr::rename(stName := !!varQ) %>%
    dplyr::mutate(count = stringr::str_count(stName, pattern = "\\S+")) %>%
    dplyr::mutate(stPreDir = ifelse(stringr::word(stName, 1) %in% dirCardinalA,
                                    stringr::word(stName, 1), NA)) %>%
    dplyr::mutate(stPreDir = ifelse(stringr::word(stName, 1) %in% dirCardinal,
                                    stringr::word(stName, 1), stPreDir)) %>%
    dplyr::mutate(stPreDir = ifelse(stringr::word(stName, 1) %in% dirInterCardinal,
                                    stringr::word(stName, 1), stPreDir)) %>%
    dplyr::mutate(stPreDir = ifelse(stringr::word(stName, 1) %in% dirInterCardinalA,
                                    stringr::word(stName, 1), stPreDir)) %>%
    dplyr::mutate(stName = ifelse(!is.na(stPreDir), stringr::word(stName, start = 2, end = count), stName)) %>%
    dplyr::select(-count) %>%
    dplyr::rename(!!varQ := stName) -> .data

  # standardize directions
  if (std == TRUE) {
    .data %>%
      dplyr::mutate(stPreDir = ifelse(stPreDir == "Northwest" | stPreDir == "Nw", "NW", stPreDir)) %>%
      dplyr::mutate(stPreDir = ifelse(stPreDir == "Northeast" | stPreDir == "Ne", "NE", stPreDir)) %>%
      dplyr::mutate(stPreDir = ifelse(stPreDir == "Southwest" | stPreDir == "Sw", "SW", stPreDir)) %>%
      dplyr::mutate(stPreDir = ifelse(stPreDir == "Southeast" | stPreDir == "Se", "SE", stPreDir)) %>%
      dplyr::mutate(stPreDir = ifelse(stPreDir == "North", "N", stPreDir)) %>%
      dplyr::mutate(stPreDir = ifelse(stPreDir == "East", "E", stPreDir)) %>%
      dplyr::mutate(stPreDir = ifelse(stPreDir == "South", "S", stPreDir)) %>%
      dplyr::mutate(stPreDir = ifelse(stPreDir == "West", "W", stPreDir)) -> .data
  }

  # rename variable if requested
  if (!is.null(paramList$output)) {
    .data <- rename(.data, !!newVarQ := stPreDir)
  }

  # return
  .data <- dplyr::as_tibble(.data)
  return(.data)
}
