#' Do Any Addresses Have Prefix or Suffix Directionals
#'
#' @description Determine whether the directional returns any matches.
#'
#' @usage pm_streetDir_any(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; A tbl created with \code{pm_dictionary} to be used
#'     as a master list of directionals. If none is provided, the \code{dic_us_dir}
#'     object will be used as the default dictionary when \code{locale = "us"}.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     one directional and \code{FALSE} if they do not.
#'
#' @export
pm_streetDir_any <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_streetDir_detect(.data, locale = locale)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_streetDir_detect(.data, dictionary = dictionary, locale = locale)
  }

  # create output
  out <- any(.data$pm.hasDir)

  # return output
  return(out)

}

#' Do All Addresses Have Prefix or Suffix Directionals
#'
#' @description Determine whether the directional test returns matches for every
#'     observation.
#'
#' @usage pm_streetDir_all(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; A tbl created with \code{pm_dictionary} to be used
#'     as a master list of directionals. If none is provided, the \code{dic_us_dir}
#'     object will be used as the default dictionary when \code{locale = "us"}.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain
#'     directionals and \code{FALSE} otherwise.
#'
#' @export
pm_streetDir_all <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_streetDir_detect(.data, locale = locale)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_streetDir_detect(.data, dictionary = dictionary, locale = locale)
  }

  # create output
  out <- all(.data$pm.hasDir)

  # return output
  return(out)

}

#' Detect Presence of Prefix or Suffix Directionals
#'
#' @description Determine the presence of house numbersin a string.
#'
#' @usage pm_streetDir_detect(.data, dictionary, locale = "us")
#'
#' @details If a street name is also a directional, like \code{North Ave}, it will be
#'     identified as such. See \link{pm_streetDir_parse} and \link{pm_streetSuf_parse}
#'     for additional details on how these addresses are handled.
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; A tbl created with \code{pm_dictionary} to be used
#'     as a master list of directionals. If none is provided, the \code{dic_us_dir}
#'     object will be used as the default dictionary when \code{locale = "us"}.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.hasDir} that is
#'     \code{TRUE} if a directional is found in the first or last word of
#'     the address and \code{FALSE} otherwise.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom stringr word
#'
#' @export
pm_streetDir_detect <- function(.data, dictionary, locale = "us"){

  # global bindings
  pm.address = ...preDir = ...sufDir = dic_us_dir = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # dictionary if none specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- postmastr::dic_us_dir
    }
  }

  # minimize dictionary
  if (locale == "us"){
    dict <- paste(dictionary$dir.input, collapse = "|")
  }

  # detect pattern
  .data %>%
    dplyr::mutate(...preDir = stringr::str_detect(pm.address, pattern = stringr::str_c("^\\b(", dict, ")\\b"))) %>%
    dplyr::mutate(...sufDir = stringr::str_detect(pm.address, pattern = stringr::str_c("\\b(", dict, ")\\b$"))) %>%
    dplyr::mutate(pm.hasDir = ifelse(...preDir == TRUE | ...sufDir == TRUE, TRUE, FALSE)) %>%
    dplyr::select(-...preDir, -...sufDir) -> .data

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_has_dir
#'
#' @description Automatically subset the results of \link{pm_streetDir_detect} to
#'    return only observations that were not found in the dictionary.
#'
#' @usage pm_streetDir_none(.data, dictionary, locale = "us")
#'
#' @details If a street name is also a directional, like \code{North Ave}, it will be
#'     identified as such. See \link{pm_streetDir_parse} and \link{pm_streetSuf_parse}
#'     for additional details on how these addresses are handled.
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; A tbl created with \code{pm_dictionary} to be used
#'     as a master list of directionals. If none is provided, the \code{dic_us_dir}
#'     object will be used as the default dictionary when \code{locale = "us"}.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble containing only observations that were not found in
#'     the dictionary. The variable created by \link{pm_streetDir_detect},
#'     \code{pm.hasDir}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_streetDir_none <- function(.data, dictionary, locale = "us"){

  # global bindings
  pm.hasDir = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # dictionary if none specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- postmastr::dic_us_dir
    }
  }

  # create output
  .data %>%
    pm_streetDir_detect(dictionary = dictionary, locale = locale) %>%
    dplyr::filter(pm.hasDir == FALSE) %>%
    dplyr::select(-pm.hasDir) -> out

  # return output
  return(out)

}

#' Parse Prefix and Suffix Directionals
#'
#' @description Parse a prefix or suffix directional from a string. These data
#'     should be at the beginning or end of the string (i.e. the first/last word or two).
#'
#' @details If a street name is also a directional, like \code{North Ave}, it will be
#'     identified and parsed as such. The \link{pm_streetSuf_parse} function includes
#'     a logic check for streets that have a prefix direction but not street name
#'     after the street suffix is parsed. If those conditions are met, the street name
#'     will be changed from \code{NA} to the directional's preferred spelling according
#'     to the USPS.
#'
#' @usage pm_streetDir_parse(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary Optional; A tbl created with \code{pm_dictionary} to be used
#'     as a master list of directionals. If none is provided, the \code{dic_us_dir}
#'     object will be used as the default dictionary when \code{locale = "us"}.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new character variable \code{pm.preDir} that contains
#'     the abbreviation for the given directional for any prefix directional
#'     and a second new character variable \code{pm.sufDir} that contains the
#'     abbreviation for the given directional for any suffix directional.
#'     The use of abbrevations follows USPS addressing standards. If a prefix
#'     or suffix direction is not detected in the string, a value of \code{NA}
#'     will be returned. If no prefix directions are found in the data at all,
#'     that column will not be returned; the same is true for suffix directions.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringr word
#'
#' @export
pm_streetDir_parse <- function(.data, dictionary, locale = "us"){

  # create bindings for global variables
  pm.address = pm.uid = dic_us_dir = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # dictionary if none specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- postmastr::dic_us_dir
    }
  }

  # dictionary if NULL
  if (is.null(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- postmastr::dic_us_dir
    }
  }

  # parse states
  if (locale == "us"){
    .data <- pm_parse_dir_us(.data, dictionary = dictionary)
  }

  # re-order output
  if (locale == "us"){
    vars <- pm_reorder(.data)
    .data <- dplyr::select(.data, vars)
  }

  # return output
  return(.data)

}


# parse us directionals
pm_parse_dir_us <- function(.data, dictionary){

  # global binding
  pm.address = pm.preDir = pm.sufDir = NULL

  # minimize dictionary
  dict <- paste(dictionary$dir.input, collapse = "|")

  # parse
  .data %>%
    mutate(pm.preDir = ifelse(stringr::str_detect(pm.address, pattern = stringr::str_c("^\\b(", dict, ")\\b")) == TRUE,
                              stringr::str_extract(pm.address, pattern = stringr::str_c("^\\b(", dict, ")\\b")), NA)) %>%
    mutate(pm.sufDir = ifelse(stringr::str_detect(pm.address, pattern = stringr::str_c("\\b(", dict, ")\\b$")) == TRUE,
                              stringr::str_extract(pm.address, pattern = stringr::str_c("\\b(", dict, ")\\b$")), NA)) -> .data

  # clean address data
  .data %>%
    dplyr::mutate(pm.address = ifelse(is.na(pm.preDir) == FALSE,
                                      stringr::word(pm.address, start = 2, end = -1), pm.address)) %>%
    dplyr::mutate(pm.address = ifelse(is.na(pm.sufDir) == FALSE,
                                      stringr::word(pm.address, start = 1, end = -2), pm.address)) -> .data

  # standardize prefix direction (or drop)
  if (all(is.na(.data$pm.preDir)) == TRUE){
    .data <- dplyr::select(.data, -pm.preDir)
  } else if (all(is.na(.data$pm.preDir)) == FALSE){
    .data <- pm_streetDir_std(.data, var = pm.preDir, dictionary = dictionary)
  }

  # standardize suffix direction (or drop)
  if (all(is.na(.data$pm.sufDir)) == TRUE){
    .data <- dplyr::select(.data, -pm.sufDir)
  } else if (all(is.na(.data$pm.sufDir)) == FALSE){
    .data <- pm_streetDir_std(.data, var = pm.sufDir, dictionary = dictionary)
  }

  # return output
  return(.data)

}

#' Standardize Parsed State Names
#'
#' @description Convert directionals to the USPS preferred abbreviations
#'
#' @usage pm_streetDir_std(.data, var, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable that may contain directionals
#' @param dictionary Optional; A tbl created with \code{pm_dictionary} to be used
#'     as a master list of directionals. If none is provided, the \code{dic_us_dir}
#'     object will be used as the default dictionary when \code{locale = "us"}.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with an updated variable that contains the one or two-letter abbreviation
#'     for the given directional. This follows USPS addressing standards.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
#' @export
pm_streetDir_std <- function(.data, var, dictionary, locale = "us"){

  # global bindings
  dic_us_dir = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # dictionary if none specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- postmastr::dic_us_dir
    }
  }

  # standardize state names
  if (locale == "us"){
    out <- pm_std_dir_us(.data, var = !!varQ, dictionary = dictionary)
  }

  # return output
  return(out)

}

# standardize us states
pm_std_dir_us <- function(.data, var, dictionary){

  # create bindings for global variables
  . = dir.input = dir.output = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  dictionary %>%
    dplyr::rename(!!varQN := dir.input) -> dictionary

  # standardize
  .data %>%
    dplyr::left_join(., dictionary, by = varQN) %>%
    dplyr::mutate(!!varQ := ifelse(is.na(dir.output) == FALSE, dir.output, !!varQ)) %>%
    dplyr::select(-dir.output) -> out

  # return output
  return(out)

}
