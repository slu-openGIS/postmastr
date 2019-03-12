#' Parse Street Addresses
#'
#' @description A wrapper around the parse functions that can be used to shorten all
#'     of \code{postmastr}'s core code down to a single function call once dictionaries
#'     have been created and tested against the data.
#'
#' @usage pm_parse(.data, input, address, output, new_address, ordinal = TRUE,
#'     unnest = FALSE, include_commas = FALSE, include_unit = TRUE,
#'     keep_parsed, side = "right", left_vars, keep_ids = FALSE,
#'     houseSuf_dict, dir_dict, street_dict, suffix_dict, unit_dict, city_dict,
#'     state_dict, locale = "us")
#'
#' @param .data A source data set to be parsed
#' @param input Describes the format of the source address. One of either \code{"full"} or \code{"short"}.
#'     A short address contains, at the most, a house number, street directionals, a street name,
#'     a street suffix, and a unit type and number. A full address contains all of the selements of a
#'     short address as well as a, at the most, a city, state, and postal code.
#' @param address A character variable containing address data to be parsed
#' @param output Describes the format of the output address. One of either \code{"full"} or \code{"short"}.
#'     A short address contains, at the most, a house number, street directionals, a street name,
#'     a street suffix, and a unit type and number. A full address contains all of the selements of a
#'     short address as well as a, at the most, a city, state, and postal code.
#' @param new_address Name of new variable to store rebuilt address in.
#' @param ordinal A logical scalar; if \code{TRUE}, street names that contain numeric words values
#'     (i.e. "Second") will be converted and standardized to ordinal values (i.e. "2nd"). The
#'     default is \code{TRUE} because it returns much more compact clean addresses (i.e.
#'     "168th St" as opposed to "One Hundred Sixty Eigth St").
#' @param unnest A logical scalar; if \code{TRUE}, house ranges will be unnested (i.e. a house range that
#'    has been expanded to cover four addresses with \code{\link{pm_houseRage_parse}} will be converted
#'    from a single observation to four observations, one for each house number). If \code{FALSE} (default),
#'    the single observation will remain.
#' @param include_commas A logical scalar; if \code{TRUE}, a comma is added both before and after the city
#'     name in rebuild addresses. If \code{FALSE} (default), no punctuation is added.
#' @param include_unit A logical scalar; if \code{TRUE} (default), the unit name and number (if given)
#'     will be included in the output string. Otherwise if \code{FALSE}, the unit name and number
#'     will not be included.
#' @param keep_parsed Character string; if \code{"yes"}, all parsed elements will be
#'     added to the source data after replacement. If \code{"limited"}, only the \code{pm.city},
#'     \code{pm.state}, and postal code variables will be retained. Otherwise, if \code{"no"},
#'     only the rebuilt address will be added to the source data (default).
#' @param side One of either \code{"left"} or \code{"right"} - should parsed data be placed to the left
#'     or right of the original data? Placing data to the left may be useful in particularly wide
#'     data sets.
#' @param left_vars A character scalar or vector of variables to place on the left-hand side of
#'     the output when \code{side} is equal to \code{"middle"}.
#' @param keep_ids Logical scalar; if \code{TRUE}, the identification numbers
#'     will be kept in the source data after replacement. Otherwise, if \code{FALSE},
#'     they will be removed (default).
#' @param houseSuf_dict Optional; name of house suffix dictionary object.  Standardizationl
#'     and parsing are skipped if none is specified.
#' @param dir_dict Optional; name of directional dictionary object. If none is specified,
#'     the full default directional dictionary will be used.
#' @param street_dict Optional; name of street dictionary object. Standardizationl is skipped
#'     if none is specified.
#' @param suffix_dict Optional; name of street suffix dictionary object. If none is specified,
#'     the full default street suffix dictionary will be used.
#' @param unit_dict Optional; name of unit dictionary object - NOT CURRENTLY ENABLED
#' @param city_dict Required for \code{"full"} addresses; name of city dictionary object.
#' @param state_dict Optional; name of state dictionary object. If none is specified,
#'     the full default state dictionary will be used.
#' @param locale A string indicating the country these data represent; the only
#'     current option is "us" but this is included to facilitate future expansion.
#'
#' @return An updated version of the source data with, at a minimum, a new variable containing
#'     standardized street addresses for each observation. Options allow for columns containing
#'     parsed elements to be returned as well.
#'
#' @examples
#' # construct dictionaries
#' dirs <- pm_dictionary(type = "directional", filter = c("N", "S", "E", "W"), locale = "us")
#' sufs <- pm_dictionary(type = "suffix", locale = "us")
#' mo <- pm_dictionary(type = "state", filter = "MO", case = c("title", "upper"), locale = "us")
#' cities <- pm_append(type = "city",
#'     input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood", "St. Louis",
#'               "SAINT LOUIS", "Webster Groves"),
#'     output = c(NA, NA, "Clayton", NA, NA, "St. Louis", NA))
#'
#' # add example data
#' df <- sushi1
#'
#' # temporary code to subset unit
#' df <- dplyr::filter(df, name != "Drunken Fish - Ballpark Village")
#'
#' # parse, full output
#' pm_parse(df, input = "full", address = address, output = "full",
#'     dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo)
#'
#' # parse, short output
#' pm_parse(df, input = "full", address = address, output = "short",
#'     new_address = clean_address, dir_dict = dirs, suffix_dict = sufs,
#'     city_dict = cities, state_dict = mo)
#'
#' @importFrom dplyr %>%
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
#' @export
pm_parse <- function(.data, input, address, output, new_address, ordinal = TRUE,
                     unnest = FALSE, include_commas = FALSE, include_unit = TRUE,
                     keep_parsed, side = "right", left_vars, keep_ids = FALSE,
                     houseSuf_dict, dir_dict, street_dict, suffix_dict, unit_dict, city_dict,
                     state_dict, locale = "us"){

  # global bindings
  address = pm.house = pm.streetSuf = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # optional output
  if (input == "short" & missing(output) == TRUE){
    output <- "short"
  }

  # optional dictionary parameters
  if (missing(houseSuf_dict) == TRUE){
    houseSuf_dict <- NULL
  }

  if (missing(dir_dict) == TRUE){
    dir_dict <- NULL
  }

  if (missing(street_dict) == TRUE){
    street_dict <- NULL
  }

  if (missing(suffix_dict) == TRUE){
    suffix_dict <- NULL
  }

  if (missing(unit_dict) == TRUE){
    unit_dict <- NULL
  }

  if (missing(city_dict) == TRUE){
    city_dict <- NULL
  }

  if (missing(state_dict) == TRUE){
    state_dict <- NULL
  }

  # unquote variable
  add <- paramList$address

  if (!is.character(paramList$address)) {
    varQ <- rlang::enquo(add)
  } else if (is.character(paramList$add)) {
    varQ <- rlang::quo(!! rlang::sym(add))
  }

  # unquote new variable
  if (missing(new_address) == FALSE){
    new_add <- paramList$new_address

    if (!is.character(paramList$new_address)) {
      newVarQ <- rlang::enquo(new_add)
    } else if (is.character(paramList$new_address)) {
      newVarQ <- rlang::quo(!! rlang::sym(new_add))
    }

  } else if (missing(new_address) == TRUE){
    newVarQ <- rlang::quo(!! rlang::sym("pm.address"))
  }

  # include units?
  # if (include_unit == TRUE){

  #  endVarQ <- rlang::quo(!! rlang::sym("pm.unitNum"))

  # } else if (include_unit == FALSE){

  #  endVarQ <- rlang::quo(!! rlang::sym("pm.streetSuf"))

  # }

  # temporary!
  endVarQ <- rlang::quo(!! rlang::sym("pm.streetSuf"))

  # create id variables
  .data %>%
    pm_identify(var = !!varQ) -> source

  # parse based on style
  if (input == "full" & output == "full"){

    source %>%
      pm_prep(var = "address") %>%
      pm_postal_parse(locale = locale) %>%
      pm_state_parse(dictionary = state_dict, locale = locale) %>%
      pm_city_parse(dictionary = city_dict, locale = locale) %>%
      pm_house_parse() %>%
      pm_houseRange_parse() %>%
      pm_houseFrac_parse() %>%
      pm_houseSuf_parse(dictionary = houseSuf_dict) %>%
      pm_streetDir_parse(dictionary = dir_dict, locale = locale) %>%
      pm_streetSuf_parse(dictionary = suffix_dict, locale = locale) %>%
      pm_street_parse(dictionary = street_dict, ordinal = ordinal) %>%
      pm_replace(source = source, unnest = unnest) %>%
      pm_rebuild(start = pm.house, end = "end", new_address = !!newVarQ,
                 keep_parsed = keep_parsed, side = side, left_vars = left_vars, keep_ids = keep_ids,
                 locale = locale) -> out

  } else if (input == "full" & output == "short") {

    source %>%
      pm_prep(var = "address") %>%
      pm_postal_parse(locale = locale) %>%
      pm_state_parse(dictionary = state_dict, locale = locale) %>%
      pm_city_parse(dictionary = city_dict, locale = locale) %>%
      pm_house_parse() %>%
      pm_houseRange_parse() %>%
      pm_houseFrac_parse() %>%
      pm_houseSuf_parse(dictionary = houseSuf_dict) %>%
      pm_streetDir_parse(dictionary = dir_dict, locale = locale) %>%
      pm_streetSuf_parse(dictionary = suffix_dict, locale = locale) %>%
      pm_street_parse(dictionary = street_dict, ordinal = ordinal) %>%
      pm_replace(source = source, unnest = unnest) %>%
      pm_rebuild(start = pm.house, end = !!endVarQ, new_address = !!newVarQ,
                 keep_parsed = keep_parsed, side = side, left_vars = left_vars, keep_ids = keep_ids,
                 locale = locale) -> out

  } else if (input == "short" & output == "short"){

    source %>%
      pm_prep(var = "address") %>%
      pm_house_parse() %>%
      pm_houseRange_parse() %>%
      pm_houseFrac_parse() %>%
      pm_houseSuf_parse(dictionary = houseSuf_dict) %>%
      pm_streetDir_parse(dictionary = dir_dict, locale = locale) %>%
      pm_streetSuf_parse(dictionary = suffix_dict, locale = locale) %>%
      pm_street_parse(dictionary = street_dict, ordinal = ordinal) %>%
      pm_replace(source = source, unnest = unnest) %>%
      pm_rebuild(start = pm.house, end = !!endVarQ, new_address = !!newVarQ,
                 keep_parsed = keep_parsed, side = side, left_vars = left_vars, keep_ids = keep_ids,
                 locale = locale) -> out

  }

  # return output
  return(out)

}
