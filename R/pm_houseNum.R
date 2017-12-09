#' Parse House Number
#'
#' @description \code{pm_houseNum} parses the house number from a given address string
#'     structured in the following way - "123 Main St". This function can also process
#'     address ranges that are structured in the following manner - "123-125 Main St".
#'
#' @usage pm_houseNum(.data, address, output)
#'
#' @param .data A tbl
#' @param address Name of the street address variable containing house numbers
#' @param output Optional root name for output variable(s)
#'
#' @source See \href{https://pe.usps.com/text/pub28/28apd_002.htm}{USPS Publication 28 -
#'     Postal Addressing Standards, Section D1}
#'
#' @return A tibble with the house number and, if needed in the case of hyphenated address ranges,
#'    variables representing the upper and lower bounds of the address range. These variables all
#'    will share the same root, which is \code{houseNum} by default but can be altered with the
#'    \code{output} argument. For non-hyphenated addresses, the variable returned by default is
#'    therefor \code{houseNum}. For hyphenated addresses, the variables returned by default are
#'    \code{houseNum}, \code{houseNumL}, and \code{houseNumU}. If a root is suppled, "L" and "U"
#'    will be appended to the ends of the root in the case of hyphenated addresses.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom stringr str_extract
#' @importFrom string str_replace_all
#' @importFrom stringr word
#' @importFrom rlang :=
#'
#' @examples
#' exampleData <- data.frame(
#'     id = c(1, 2, 3),
#'     streetStr = c("225-227 1st St", "486 First St", "4256-4258 MLK Boulevard"),
#'     stringsAsFactors = FALSE
#' )
#'
#' pm_houseNum(exampleData, streetStr)
#'
#' @export
pm_houseNum <- function(.data, address, output) {

  # prevents R CMD check note for undefined gloabl variable:
  count <- NULL
  houseNum <- NULL
  houseNumL <- NULL
  houseNumS <- NULL
  houseNumU <- NULL
  stFull <- NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # reformat address variable
  if (!is.character(paramList$address)) {
    var <- rlang::enquo(address)
  } else if (is.character(paramList$address)) {
    var <- rlang::quo(!! rlang::sym(address))
  }

  varQ <- rlang::quo_name(rlang::enquo(var))

  # reformat houseNum variable
  if (!is.null(paramList$output)) {
    if (!is.character(paramList$output)) {
      newVar <- rlang::enquo(output)
    } else if (is.character(paramList$output)) {
      newVar <- rlang::quo(!! rlang::sym(output))
    }

    newVarQ <- rlang::quo_name(rlang::enquo(newVar))

    newVarL <- paste(rlang::quo_name(newVar), "L", sep = "")
    newVarL <- rlang::quo_name(rlang::enquo(newVarL))

    newVarU <- paste(rlang::quo_name(newVar), "U", sep = "")
    newVarU <- rlang::quo_name(rlang::enquo(newVarU))
  }

  # process house numbers
  .data %>%
    dplyr::rename(stFull := !!varQ) %>%
    dplyr::mutate(houseNum = stringr::word(stFull, 1)) %>%
    dplyr::mutate(houseNumS = stringr::str_replace_all(houseNum, stringr::fixed("-"), " ")) %>%
    dplyr::mutate(count = vapply(strsplit(houseNumS, "\\W+"), length, integer(1))) %>%
    dplyr::mutate(houseNumL = stringr::word(houseNumS, 1)) %>%
    dplyr::mutate(houseNumU = ifelse(count == 1, houseNumL, stringr::word(houseNumS, 2))) %>%
    dplyr::mutate(houseNumU = paste(substr(houseNumL, 1, nchar(houseNumL) - nchar(houseNumU)),
                                    houseNumU, sep = '')) %>%
    dplyr::mutate(houseNumL = stringr::str_extract(houseNumL, '\\d+')) %>%
    dplyr::mutate(houseNumL = as.numeric(houseNumL)) %>%
    dplyr::mutate(houseNumU = stringr::str_extract(houseNumU, '\\d+')) %>%
    dplyr::mutate(houseNumU = as.numeric(houseNumU)) %>%
    dplyr::mutate(houseNumU = ifelse(houseNumL == houseNumU, NA, houseNumU)) %>%
    dplyr::select(-count, -houseNumS) %>%
    dplyr::rename(!!varQ := stFull) -> .data

  if (!is.null(paramList$output) & (all(is.na(.data$houseNumU)) == TRUE)) {
    .data %>%
      rename(!!newVarQ := houseNum) %>%
      select(-c(houseNumL, houseNumU)) -> .data
  } else if (!is.null(paramList$output) & (all(is.na(.data$houseNumU)) == FALSE)) {
    .data %>%
      rename(!!newVarQ := houseNum) %>%
      rename(!!newVarL := houseNumL) %>%
      rename(!!newVarU := houseNumU) -> .data
  } else if (is.null(paramList$numVar) & (all(is.na(.data$houseNumU)) == TRUE)) {
    .data <- dplyr::select(.data, -c(houseNumL, houseNumU))
  }

  # return
  dplyr::as_tibble(.data)
}
