#' Parse Street Address
#'
#' @export
pm_parse <- function(.data, style, locale = "us", newVar, dirDictionary, suffixDictionary, cityDictionary, stateDictionary){

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
      pm_parse_state(dictionary = stateDictionary, locale = locale) %>%
      pm_parse_city(dictionary = cityDictionary, locale = locale) %>%
      pm_parse_house() %>%
      pm_parse_houseFrac() %>%
      pm_parse_street_dir(dictionary = dirDictionary, locale = locale) %>%
      pm_parse_street_suf(dictionary = suffixDictionary, locale = locale) %>%
      pm_parse_street() %>%
      pm_rebuild(start = pm.house, end = "end", locale = locale) %>%
      pm_replace(source = source, newVar = !!varQ) -> out

  } else if (style == "short"){

    source %>%
      pm_prep(var = "address") %>%
      pm_parse_house() %>%
      pm_parse_houseFrac() %>%
      pm_parse_street_dir(dictionary = dirDictionary, locale = locale) %>%
      pm_parse_street_suf(dictionary = suffixDictionary, locale = locale) %>%
      pm_parse_street() %>%
      pm_rebuild(start = pm.house, end = pm.streetSuf, locale = locale) %>%
      pm_replace(source = source, newVar = !!varQ) -> out

  }

  return(out)

}
