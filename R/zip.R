#' Detect Presence of Zip Codes
#'
#' @description Determine the presence of U.S. zip-codes in a string. This will
#'     identify both five digit zip-codes (e.g. \code{63108}) as well as zip+4
#'     codes (e.g. \code{63108-3412}). The zip-code must be the final word in
#'     the string to return \code{TRUE}.
#'
#' @param .data A tbl or data frame
#' @param var A character variable that may contain zip codes
#'
#' @return A tibble with a new logical variable \code{pm.isZip} that is
#'     \code{TRUE} if a zip-code is found in the last word of the address
#'     and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#' @importFrom stringr str_detect
#' @importFrom stringr word
#'
#' @export
pm_isZip <- function(.data, var){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # detect pattern
  .data %>%
    dplyr::mutate(pm.last = stringr::word(!!varQ, -1)) %>%
    dplyr::mutate(pm.isZip = stringr::str_detect(pm.last, "([0-9]{5})")) %>%
    dplyr::select(-pm.last) -> out

  # return output
  return(out)

}

#' Parse Zip-Codes
#'
#' @description Create a new column containing zip-code data.
#'
#' @param .data A tbl or data frame
#' @param var A character variable that may contain zip codes
#'
#' @return A tibble with a new column \code{pm.zip} that contains the zip-code.
#'     If a zip-code is not detected in the string, a value of \code{NA} will be
#'     returned. If it does not yet exist, a copy of the address variable will
#'     be created in \code{pm.address} and returned with zip-codes removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#' @importFrom stringr word
#'
#' @export
pm_parseZip <- function(.data, var){

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

  # identify zip-code
  out <- pm_isZip(.data, "pm.address")

  # parse
  out %>%
    dplyr::mutate(pm.zip =
                    ifelse(pm.isZip == TRUE,
                           stringr::word(pm.address, start = -1),
                           NA)) %>%
    dplyr::mutate(pm.address =
                    ifelse(pm.isZip == TRUE,
                           stringr::word(pm.address, start = 1, end = -2),
                           pm.address)) %>%
    dplyr::select(-pm.isZip) -> out

  # return output
  return(out)

}
