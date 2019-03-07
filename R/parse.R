#' Parse Street Address
#'
#' @description A wrapper around the parse functions that can be used to shorten all
#'     of \code{postmastr}'s code down to a single function call once dictionaries
#'     have been created and tested against the data.
#'
#' @usage pm_parse(.data, input, var, output, newVar, ordinal = TRUE, add_commas = FALSE,
#'     keep_parsed = "no", keep_ids = FALSE, dirDict, streetDict, suffixDict, unitDict,
#'     cityDict, stateDict, locale = "us")
#' @param .data A source data set to be parsed
#' @param input Describes the format of the source address. One of either \code{"full"} or \code{"short"}.
#'     A short address contains, at the most, a house number, street directionals, a street name,
#'     a street suffix, and a unit type and number. A full address contains all of the selements of a
#'     short address as well as a, at the most, a city, state, and postal code.
#' @param var A character variable containing address data to be parsed
#' @param output Describes the format of the output address. One of either \code{"full"} or \code{"short"}.
#'     A short address contains, at the most, a house number, street directionals, a street name,
#'     a street suffix, and a unit type and number. A full address contains all of the selements of a
#'     short address as well as a, at the most, a city, state, and postal code.
#' @param newVar Name of new variable to store rebuilt address in.
#' @param ordinal A logical scalar; if \code{TRUE}, street names that contain numeric words values
#'     (i.e. "Second") will be converted and standardized to ordinal values (i.e. "2nd"). The
#'     default is \code{TRUE} because it returns much more compact clean addresses (i.e.
#'     "168th St" as opposed to "One Hundred Sixty Eigth St").
#' @param add_commas A logical scalar; if \code{TRUE}, a comma is added both before and after the city
#'    name in rebuild addresses. If \code{FALSE} (default), no punctuation is added.
#' @param keep_parsed Logical scalar; if \code{TRUE}, all parsed elements will be
#'     added to the source data after replacement. Otherwise, if \code{FALSE},
#'     only the rebuilt address will be added to the source data (default).
#' @param keep_ids Logical scalar; if \code{TRUE}, the identification numbers
#'     will be kept in the source data after replacement. Otherwise, if \code{FALSE},
#'     they will be removed (default).
#' @param dirDict Optional; name of directional dictionary object
#' @param streetDict Optional; name of street dictionary object
#' @param suffixDict Optional; name of street suffix dictionary object
#' @param unitDict Optional; name of unit dictionary object
#' @param cityDict Optional; name of city dictionary object
#' @param stateDict Optional; name of state dictionary object
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
#' @export
pm_parse <- function(.data, input, var, output, newVar, ordinal = TRUE, add_commas = FALSE,
                     keep_parsed = "no", keep_ids = FALSE, dirDict, streetDict, suffixDict, unitDict,
                     cityDict, stateDict, locale = "us"){

  # global bindings
  address = pm.house = pm.streetSuf = NULL

  # optional output
  if (input == "short" & missing(output) == TRUE){
    output <- "short"
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # unquote variable
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # unquote new variable
  if (missing(newVar) == FALSE){
    if (!is.character(paramList$newVar)) {
      newVarQ <- rlang::enquo(newVar)
    } else if (is.character(paramList$newVar)) {
      newVarQ <- rlang::quo(!! rlang::sym(newVar))
    }
  } else if (missing(newVar) == TRUE){
    newVarQ <- rlang::quo(!! rlang::sym("pm.address"))
  }

  # create id variables
  .data %>%
    pm_identify(var = !!varQ) -> source

  # parse based on style
  if (input == "full" & output == "full"){

    source %>%
      pm_prep(var = "address") %>%
      pm_postal_parse(locale = locale) %>%
      pm_state_parse(dictionary = stateDict, locale = locale) %>%
      pm_city_parse(dictionary = cityDict, locale = locale) %>%
      pm_house_parse() %>%
      pm_houseFrac_parse() %>%
      pm_streetDir_parse(dictionary = dirDict, locale = locale) %>%
      pm_streetSuf_parse(dictionary = suffixDict, locale = locale) %>%
      pm_street_parse(ordinal = ordinal) %>%
      pm_rebuild(start = pm.house, end = "end", add_commas = add_commas, locale = locale) %>%
      pm_replace(source = source, newVar = !!newVarQ, keep_parsed = keep_parsed, keep_ids = keep_ids) -> out

  } else if (input == "full" & output == "short") {

    source %>%
      pm_prep(var = "address") %>%
      pm_postal_parse(locale = locale) %>%
      pm_state_parse(dictionary = stateDict, locale = locale) %>%
      pm_city_parse(dictionary = cityDict, locale = locale) %>%
      pm_house_parse() %>%
      pm_houseFrac_parse() %>%
      pm_streetDir_parse(dictionary = dirDict, locale = locale) %>%
      pm_streetSuf_parse(dictionary = suffixDict, locale = locale) %>%
      pm_street_parse(ordinal = ordinal) %>%
      pm_rebuild(start = pm.house, end = pm.streetSuf, locale = locale) %>%
      pm_replace(source = source, newVar = !!newVarQ, keep_parsed = keep_parsed, keep_ids = keep_ids) -> out

  } else if (input == "short" & output == "short"){

    source %>%
      pm_prep(var = "address") %>%
      pm_house_parse() %>%
      pm_houseFrac_parse() %>%
      pm_streetDir_parse(dictionary = dirDict, locale = locale) %>%
      pm_streetSuf_parse(dictionary = suffixDict, locale = locale) %>%
      pm_street_parse(ordinal = ordinal) %>%
      pm_rebuild(start = pm.house, end = pm.streetSuf, locale = locale) %>%
      pm_replace(source = source, newVar = !!newVarQ, keep_parsed = keep_parsed, keep_ids = keep_ids) -> out

  }

  # return output
  return(out)

}
