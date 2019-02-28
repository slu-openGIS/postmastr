#' Parse Street Address
#'
#' @description A wrapper around the parse functions that can be used to shorten all
#'     of \code{postmastr}'s code down to a single function call once dictionaries
#'     have been created and tested against the data.
#'
#' @usage pm_parse(.data, style, locale = "us", newVar, keep_parsed = FALSE, keep_ids = FALSE,
#'     dirDict, streetDict, suffixDict, unitDict, cityDict, stateDict)
#'
#' @param .data A source data set to be parsed
#' @param style One of either \code{"full"} or \code{"short"}. A short address contains,
#'     at the most, a house number, street directionals, a street name, a street suffix,
#'     and a unit type and number. A full address contains all of the selements of a short
#'     address as well as a, at the most, a city, state, and postal code.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#' @param keep_parsed Logical scalar; if \code{TRUE}, all parsed elements will be
#'     added to the source data after replacement. Otherwise, if \code{FALSE},
#'     only the rebuilt address will be added to the source data (default).
#' @param keep_ids Logical scalar; if \code{TRUE}, the identification numbers
#'     will be kept in the source data after replacement. Otherwise, if \code{FALSE},
#'     they will be removed (default).
#' @param newVar Name of new variable to store rebuilt address in.
#' @param dirDict Optional; name of directional dictionary object
#' @param streetDict Optional; name of street dictionary object
#' @param suffixDict Optional; name of street suffix dictionary object
#' @param unitDict Optional; name of unit dictionary object
#' @param cityDict Optional; name of city dictionary object
#' @param stateDict Optional; name of state dictionary object
#'
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
#' @export
pm_parse <- function(.data, style, locale = "us", newVar, keep_parsed = FALSE, keep_ids = FALSE,
                     dirDict, streetDict, suffixDict, unitDict, cityDict, stateDict){

  # global bindings
  address = pm.house = pm.streetSuf = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # unquote
  if (!is.character(paramList$newVar)) {
    varQ <- rlang::enquo(newVar)
  } else if (is.character(paramList$newVar)) {
    varQ <- rlang::quo(!! rlang::sym(newVar))
  }

  # create id variables
  .data %>%
    pm_identify(var = address) -> source

  # parse based on style
  if (style == "full"){

    source %>%
      pm_prep(var = "address") %>%
      pm_parse_postal(locale = locale) %>%
      pm_parse_state(dictionary = stateDict, locale = locale) %>%
      pm_city_parse(dictionary = cityDict, locale = locale) %>%
      pm_house_parse() %>%
      pm_houseFrac_parse() %>%
      pm_streetDir_parse(dictionary = dirDict, locale = locale) %>%
      pm_streetSuf_parse(dictionary = suffixDict, locale = locale) %>%
      pm_street_parse() %>%
      pm_rebuild(start = pm.house, end = "end", locale = locale) %>%
      pm_replace(source = source, newVar = !!varQ) -> out

  } else if (style == "short"){

    source %>%
      pm_prep(var = "address") %>%
      pm_house_parse() %>%
      pm_houseFrac_parse() %>%
      pm_streetDir_parse(dictionary = dirDict, locale = locale) %>%
      pm_streetSuf_parse(dictionary = suffixDict, locale = locale) %>%
      pm_street_parse() %>%
      pm_rebuild(start = pm.house, end = pm.streetSuf, locale = locale) %>%
      pm_replace(source = source, newVar = !!varQ) -> out

  }

  # return output
  return(out)

}
