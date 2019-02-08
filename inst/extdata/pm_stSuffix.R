#' Parse Street Suffixes
#'
#' @param .data A tbl
#' @param address Name of the street address variable containing suffix types
#' @param std A logical scalar. Should suffix types be standardized?
#' @param output Optional name for output variable
#'
#' @export
pm_stSuffix <- function(.data, stName, std = TRUE, output){

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

  # load suffix data
  correct <- get("stdSuffixTbl")

  # parse stName
  .data %>%
    dplyr::rename(stName := !!varQ) %>%
    dplyr::mutate(count = stringr::str_count(stName, pattern = "\\S+")) %>%
    dplyr::mutate(stType = ifelse(stringr::word(stName, -1) %in% correct$suf_com,
                                  stringr::word(stName, -1), NA)) %>%
    dplyr::mutate(stType = ifelse(stringr::word(stName, -1) %in% correct$suf_std,
                                  stringr::word(stName, -1), stType)) %>%
    dplyr::mutate(stName = ifelse(!is.na(stType), stringr::word(stName, start = 1, end = count-1), stName)) %>%
    dplyr::select(-count) %>%
    dplyr::rename(!!varQ := stName) -> .data

  # standardize stType
  if (std == TRUE) {
    .data <- pm_stdSuffix(.data, suffix = stType, overwrite = TRUE)
  }

  # rename variable if requested
  if (!is.null(paramList$output)) {
    .data <- rename(.data, !!newVarQ := stType)
  }

  # return tibble
  .data <- dplyr::as_tibble(.data)
  return(.data)
}
