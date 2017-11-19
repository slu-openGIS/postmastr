#' Extract Street Name
#'
#' @export
pm_stName <- function(.data, address, houseNum = TRUE, output) {

  # save parameters to list
  paramList <- as.list(match.call())

  # reformat address variable
  if (!is.character(paramList$address)) {
    var <- rlang::enquo(address)
  } else if (is.character(paramList$address)) {
    var <- rlang::quo(!! rlang::sym(address))
  }

  varQ <- rlang::quo_name(rlang::enquo(var))

  # reformat stVar variable
  if (!is.null(paramList$output)) {
    if (!is.character(paramList$output)) {
      newVar <- rlang::enquo(output)
    } else if (is.character(paramList$output)) {
      newVar <- rlang::quo(!! rlang::sym(output))
    }

    newVarQ <- rlang::quo_name(rlang::enquo(newVar))
  }

  # parse address variable
  if (houseNum == TRUE){
    .data %>%
      dplyr::rename(stFull := !!varQ) %>%
      dplyr::mutate(count = stringr::str_count(stFull, pattern = "\\S+")) %>%
      dplyr::mutate(stName = ifelse(count >= 3, stringr::word(stFull, start = 2, end = count), NA)) %>%
      dplyr::mutate(stName = stringr::str_to_title(stName)) %>%
      dplyr::select(-count) %>%
      dplyr::rename(!!varQ := stFull) -> .data
  } else if (houseNum == FALSE) {
    .data %>%
      dplyr::rename(stFull := !!varQ) %>%
      dplyr::mutate(count = stringr::str_count(stFull, pattern = "\\S+")) %>%
      dplyr::mutate(stName = ifelse(count >= 2, stringr::word(stFull, start = 1, end = count), NA)) %>%
      dplyr::select(-count) %>%
      dplyr::rename(!!varQ := stFull) -> .data
  }

  # remove punctuation
  .data <- mutate(.data, stName = stringr::str_replace(stName, "[.]", ""))

  # rename variable if requested
  if (!is.null(paramList$output)) {
    .data <- rename(.data, !!newVarQ := stName)
  }

  # return
  .data <- dplyr::as_tibble(.data)
  return(.data)
}
