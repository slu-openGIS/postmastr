#' Standardized Street Names
#'
#' @export
pm_stdStreet <- function(.data, stName, overwrite = TRUE, output) {

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
  numeric$stName <- sapply(numeric$stName, toOrdinal, USE.NAMES = FALSE)
  numeric$stName <- as.character(numeric$stName)

  # combine data
  output <- dplyr::bind_rows(string, numeric)
  output <- dplyr::arrange(output, st_id)
  output <- dplyr::select(output, -st_id)

  # fix St, Nd, Th in street names to all lower case
  output %>%
    mutate(stName = ifelse(str_detect(str_sub(x$stName, start = -2, end = -1), "St") &
                             str_detect(str_sub(x$stName, start = -3, end = -3), "[0-9]"),
                           str_replace(stName, "St", "st"), stName)) %>%
    mutate(stName = ifelse(str_detect(str_sub(x$stName, start = -2, end = -1), "Nd") &
                             str_detect(str_sub(x$stName, start = -3, end = -3), "[0-9]"),
                           str_replace(stName, "Nd", "nd"), stName)) %>%
    mutate(stName = ifelse(str_detect(str_sub(x$stName, start = -2, end = -1), "Rd") &
                             str_detect(str_sub(x$stName, start = -3, end = -3), "[0-9]"),
                           str_replace(stName, "Rd", "rd"), stName)) %>%
    mutate(stName = ifelse(str_detect(str_sub(x$stName, start = -2, end = -1), "Th") &
                             str_detect(str_sub(x$stName, start = -3, end = -3), "[0-9]"),
                           str_replace(stName, "Th", "th"), stName)) -> output

  # return tibble
  output <- dplyr::as_tibble(output)
  return(output)
}
