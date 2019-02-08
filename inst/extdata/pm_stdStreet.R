#' Standardized Street Names
#'
#' \code{pm_stdStreet} standardizes the spelling and formatting of street names. Names like "Second" are
#' consistently converted to "2nd".
#'
#' @usage pm_stdStreet(.data, stName, output)
#'
#' @param .data A tbl
#' @param stName Name of the street name variable
#' @param output Optional name of output variable
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace
#' @importFrom stringr str_sub
#' @importFrom stringr word
#' @importFrom rlang :=
#' @importFrom toOrdinal toOrdinal
#'
#' @export
pm_stdStreet <- function(.data, stName, output) {

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

  # create identification variable
  input <- .data
  input <- mutate(input, st_id = as.numeric(rownames(input)))

  # numeric words
  numWords <- c("One", "First", "Two", "Second", "Three", "Third", "Four", "Fourth",
                "Five", "Fifth", "Six", "Sixth", "Seven", "Seventh", "Eight", "Nine",
                "Ninth", "Ten", "Tenth", "Eleven", "Eleventh", "Twelve", "Twelfth",
                "Thirteen", "Thirteenth", "Fourteen", "Fourteenth", "Fifteen", "Fifteenth",
                "Sixteen", "Sixteenth", "Seventeen", "Seventeenth", "Eighteen", "Eighteenth",
                "Nineteen", "Nineteenth", "Twenty", "Twentieth", "Thirty", "Thirtieth",
                "Forty", "Fortieth", "Fifty", "Fiftieth", "Sixty", "Sixtieth",
                "Seventy", "Seventieth", "Eighty", "Eightieth", "Ninety", "Ninetieth")

  # subset data
  input <- dplyr::rename(input, stName := !!varQ)
  string <- dplyr::filter(input, stringr::word(stName, 1) %nin% numWords)
  input %>%
    dplyr::filter(stringr::word(stName, 1) %in% numWords) -> numeric

  # standardize street names
  numeric$stName <- sapply(numeric$stName, pm_word2num, USE.NAMES = FALSE)
  numeric$stName <- sapply(numeric$stName, toOrdinal::toOrdinal, USE.NAMES = FALSE)
  numeric$stName <- as.character(numeric$stName)

  # combine data
  output <- dplyr::bind_rows(string, numeric)
  output <- dplyr::arrange(output, st_id)
  output <- dplyr::select(output, -st_id)

  # fix St, Nd, Th in street names to all lower case
  output %>%
    mutate(stName = ifelse(stringr::str_detect(stringr::str_sub(stName, start = -2, end = -1), "St") &
                             stringr::str_detect(stringr::str_sub(stName, start = -3, end = -3), "[0-9]"),
                           stringr::str_replace(stName, "St", "st"), stName)) %>%
    mutate(stName = ifelse(stringr::str_detect(stringr::str_sub(stName, start = -2, end = -1), "Nd") &
                             stringr::str_detect(stringr::str_sub(stName, start = -3, end = -3), "[0-9]"),
                           stringr::str_replace(stName, "Nd", "nd"), stName)) %>%
    mutate(stName = ifelse(stringr::str_detect(stringr::str_sub(stName, start = -2, end = -1), "Rd") &
                             stringr::str_detect(stringr::str_sub(stName, start = -3, end = -3), "[0-9]"),
                           stringr::str_replace(stName, "Rd", "rd"), stName)) %>%
    mutate(stName = ifelse(stringr::str_detect(stringr::str_sub(stName, start = -2, end = -1), "Th") &
                             stringr::str_detect(stringr::str_sub(stName, start = -3, end = -3), "[0-9]"),
                           stringr::str_replace(stName, "Th", "th"), stName)) -> output

  # rename variable if requested
  if (!is.null(paramList$output)) {
    .data <- rename(.data, !!newVarQ := stName)
  }

  # return tibble
  output <- dplyr::as_tibble(output)
  return(output)
}
