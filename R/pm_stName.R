#' Extract Street Name
#'
#' @export
pm_stName <- function(.data, address, houseNum = TRUE, stVar) {

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
  if (!is.null(paramList$stVar)) {
    if (!is.character(paramList$stVar)) {
      newVar <- rlang::enquo(stVar)
    } else if (is.character(paramList$stVar)) {
      newVar <- rlang::quo(!! rlang::sym(stVar))
    }

    newVarQ <- rlang::quo_name(rlang::enquo(newVar))
  }

  # parse address variable
  if (houseNum == TRUE){
    .data %>%
      dplyr::rename(stFull := !!varQ) %>%
      dplyr::mutate(count = stringr::str_count(stFull, pattern = "\\S+")) %>%
      dplyr::mutate(stName = ifelse(count == 3, stringr::word(stFull, 2), NA)) %>%
      dplyr::mutate(stName = ifelse(count > 3, stringr::word(stFull, start = 2, end = count-1), stName)) %>%
      dplyr::mutate(stName = stringr::str_to_title(stName)) %>%
      dplyr::select(-count) %>%
      dplyr::rename(!!varQ := stFull) -> .data
  } else if (houseNum == FALSE) {
    .data %>%
      dplyr::rename(stFull := !!varQ) %>%
      dplyr::mutate(count = stringr::str_count(stFull, pattern = "\\S+")) %>%
      dplyr::mutate(stName = ifelse(count == 2, stringr::word(stFull, 1), NA)) %>%
      dplyr::mutate(stName = ifelse(count > 2, stringr::word(stFull, start = 1, end = count-1), stName)) %>%
      dplyr::mutate(stName = stringr::str_to_title(stName)) %>%
      dplyr::select(-count) %>%
      dplyr::rename(!!varQ := stFull) -> .data
  }

  # rename variable if requested
  if (!is.null(paramList$stVar)) {
    .data <- rename(.data, !!newVarQ := stName)
  }

  # return
  .data <- dplyr::as_tibble(.data)
  return(.data)
}
