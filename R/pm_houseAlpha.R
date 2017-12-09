#' Parse Alphanumeric Values in House Number
#'
#' @description \code{pm_houseAlpha} parses the house number for included alphanumeric ranges.
#'     This function is in development - it currently only identifies "F" (front) and "R" (rear)
#'     addresses.
#'
#' @usage pm_houseAlpha(.data, houseNum, alphaVar)
#'
#' @param .data A tbl
#' @param houseNum A vector containing house numbers to be parsed
#' @param alphaVar Optional name for output variable
#'
#' @source See \href{https://pe.usps.com/text/pub28/28ape_003.htm}{USPS Publication 28 -
#'     Postal Addressing Standards, Section E12}
#'
#' @return A tibble with the alphanumeric value, which is named \code{houseAlpha} by default
#'     but can be altered with the \code{alphaVar} argument.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom stringr str_sub
#' @importFrom stringr word
#' @importFrom rlang :=
#'
#' @export
pm_houseAlpha <- function(.data, houseNum, alphaVar) {

  # prevents R CMD check note for undefined gloabl variable:
  houseSuf <- NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # reformat address variable
  if (!is.character(paramList$houseNum)) {
    var <- rlang::enquo(houseNum)
  } else if (is.character(paramList$houseNum)) {
    var <- rlang::quo(!! rlang::sym(houseNum))
  }

  varQ <- rlang::quo_name(rlang::enquo(var))

  # reformat houseNum variable
  if (!is.null(paramList$alphaVar)) {
    if (!is.character(paramList$alphaVar)) {
      newVar <- rlang::enquo(alphaVar)
    } else if (is.character(paramList$alphaVar)) {
      newVar <- rlang::quo(!! rlang::sym(alphaVar))
    }

    newVarQ <- rlang::quo_name(rlang::enquo(newVar))
  }

  # list of accepted alphanumeric values
  alphas <- c("R", "F")

  # create houseSuf variable if does not already exist
  if ( any(names(.data) == "houseSuf") == FALSE ) {
    .data <- dplyr::mutate(.data, houseSuf = NA)
  }

  # parse house number for alphanumeric values
  .data %>%
    dplyr::rename(houseNum := !!varQ) %>%
    dplyr::mutate(houseSuf = ifelse(stringr::str_sub(houseNum, -1) %in% alphas,
                                    stringr::str_sub(houseNum, -1), houseSuf)) %>%
    dplyr::mutate(houseNum = ifelse(stringr::str_sub(houseNum, -1) %in% alphas,
                                    stringr::str_sub(houseNum, start = 1, end = -2), houseNum)) %>%
    dplyr::rename(!!varQ := houseNum) -> .data

  # rename variable if requested
  if (!is.null(paramList$alphaVar)) {
    .data <- rename(.data, !!newVarQ := houseSuf)
  }

  # return
  .data <- dplyr::as_tibble(.data)
  return(.data)
}
