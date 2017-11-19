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
pm_stAlpha <- function(.data, stName, std = TRUE, input, output) {

  # save parameters to list
  paramList <- as.list(match.call())

  # reformat stName variable
  if (!is.character(paramList$stName)) {
    var <- rlang::enquo(stName)
  } else if (is.character(paramList$stName)) {
    var <- rlang::quo(!! rlang::sym(stName))
  }

  varQ <- rlang::quo_name(rlang::enquo(var))

  # reformat input variable
  if (!is.null(paramList$input)) {
    if (!is.character(paramList$input)) {
      existVar <- rlang::enquo(input)
    } else if (is.character(paramList$input)) {
      existVar <- rlang::quo(!! rlang::sym(input))
    }

    existVarQ <- rlang::quo_name(rlang::enquo(existVar))
  }

  # reformat output variable
  if (!is.null(paramList$output)) {
    if (!is.character(paramList$output)) {
      newVar <- rlang::enquo(output)
    } else if (is.character(paramList$output)) {
      newVar <- rlang::quo(!! rlang::sym(output))
    }

    newVarQ <- rlang::quo_name(rlang::enquo(newVar))
  }

  # list of accepted alphanumeric values
  alphaFrac <- c("Front", "F", "Rear", "R", "1/2", "Half")

  # create houseSuf variable if does not already exist
  if ( any(names(.data) == "houseSuf") == FALSE & is.null(paramList$input)) {
    .data <- dplyr::mutate(.data, houseSuf = NA)
  }

  # modify input variable if necessary
  if (!is.null(paramList$input)) {
    .data <- dplyr::rename(.data, houseSuf := existVarQ)
  }

  # parse alphanumeric and fractional values from stName
  .data %>%
    dplyr::rename(stName := !!varQ) %>%
    dplyr::mutate(count = stringr::str_count(stName, pattern = "\\S+")) %>%
    dplyr::mutate(houseSuf = ifelse(stringr::word(stName, 1) %in% alphaFrac,
                                    stringr::word(stName, 1), houseSuf)) %>%
    dplyr::mutate(stName = ifelse(stringr::word(stName, 1) %in% alphaFrac,
                                  stringr::word(stName, start = 2, end = count), stName)) %>%
    dplyr::select(-count) -> .data

  # standardize values
  if (std == TRUE) {
    .data %>%
      dplyr::mutate(houseSuf = ifelse(houseSuf == "Front", "F", houseSuf)) %>%
      dplyr::mutate(houseSuf = ifelse(houseSuf == "Rear", "R", houseSuf)) %>%
      dplyr::mutate(houseSuf = ifelse(houseSuf == "Half", "1/2", houseSuf)) %>%
      dplyr::rename(!!varQ := stName) -> .data
  }

  # rename variable if requested
  if (!is.null(paramList$output)) {
    .data <- rename(.data, !!newVarQ := houseSuf)
  }

  # return
  .data <- dplyr::as_tibble(.data)
  return(.data)
}
