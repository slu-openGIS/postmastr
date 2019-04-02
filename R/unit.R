#' Detect Presence of Unit
#'
#' @description Determine the presence of unit types within a string.
#'
#' @usage pm_has_unit(.data, dictionary, scalar = TRUE, locale = "us")
#'
#' @param .data A postmastr object (created with \code{pm_prep})
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for unit names. If none is provided, the \code{pm_dic_units}
#'     object will be used as the default dictionary
#' @param scalar If \code{TRUE}, a single logical scalar is returned; otherwise if
#'     \code{FALSE}, a logical vector is returned.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.hasUnit} that is
#'     \code{TRUE} if a state name or abbreviation is found in the address
#'     and \code{FALSE} otherwise.
#'
#' @return If \code{scalar = TRUE}, a single logical scalar is returned that is
#'     \code{TRUE} if the data contain statenames or abbreviations and \code{FALSE}
#'     if they do not. If \code{scalar = FALSE} a tibble with a new logical variable
#'     \code{pm.hasUnit} that is \code{TRUE} if a unit types is found in the address
#'     and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#'
#' @export
pm_has_unit <- function(.data, dictionary, scalar = TRUE, locale = "us"){

  # create bindings for global variables
  pm.address = pm.hasUnit = working_data = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # check for object and key variables
  if (pm_has_uid(working_data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(working_data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # create directory
  if (locale == "us"){
    if (missing(dictionary) == FALSE){
      fullDic <- c("APT", "BSMT", "BLDG", "DEPT", "FL", "FRNT", "HNGR", "KEY",
                   "LBBY", "LOT", "LOWR", "OFC", "PH", "PIER", "REAR", "RM",
                   "SIDE", "SLIP", "SPC", "STOP", "STE", "TRLR", "UNIT", "UPPR")
    } else if (missing(dictionary) == TRUE){
      fullDic <- dictionary
    }

    dict <- paste(fullDic, collapse = "|")
  }

  # iterate over observations
  if (locale == "us"){
    .data <- dplyr::mutate(.data, pm.hasUnit = stringr::str_detect(pm.address,
                                                                    pattern = stringr::str_c("\\b(", dict, ")\\b")))
  }

  # return output
  return(.data)

}


#' Does Unit Name Dictionary Return Any Matches
#'
#' @description Determine whether the dictionary returns any matches.
#'
#' @usage pm_Unit_any(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for Units.
#'
#' @return If \code{scalar = TRUE}, a single logical scalar is returned that is
#'     \code{TRUE} if the data contains at least one Unit name from the given
#'     dictionary and \code{FALSE} if they do not.
#'
#' @export
pm_unit_any <- function(.data, dictionary){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test dictionary
  # do I want pm_unit_detect or pm_has_unit
  .data <- pm_unit_detect(.data, dictionary = dictionary)

  # create output
  out <- any(.data$pm.hasUnit)

  # return output
  return(out)

}


#' Does Unit Name Dictionary Return a Match for All Observations
#'
#' @description Determine whether the dictionary returns any matches.
#'
#' @usage pm_unit_all(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for cities.
#'
#' @return If \code{scalar = TRUE}, a single logical scalar is returned that is
#'     \code{TRUE} if the data contains at least one unit name from the given
#'     dictionary and \code{FALSE} if they do not.
#'
#' @export
pm_unit_all <- function(.data, dictionary){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test dictionary
  # do I want pm_unit_detect or pm_has_unit
  .data <- pm_unit_detect(.data, dictionary = dictionary)

  # create output
  out <- all(.data$pm.hasCity)

  # return output
  return(out)

}

# standardize us unit
pm_std_unit_us <- function(.data, var, dictionary){

  # create bindings for global variables
  . = unit.input = unit.output = unit.type = NULL

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
    dplyr::rename(!!varQ := unit.input) -> dictionary

  # standardize
  .data %>%
    dplyr::left_join(., dictionary, by = varQN) %>%
    dplyr::mutate(!!varQ := ifelse(is.na(unit.output) == FALSE, unit.output, !!varQ)) %>%
    dplyr::select(-unit.output, -unit.type) -> out

  # return output
  return(out)

}

#' Parse Unit
#'  this description needs to be updated  originally taken from street
#' @description Converts the remaining text of \code{pm.address} to title case and stores
#'     it in a new variable named \code{pm.unit}.
#'
#' @usage pm_unit_parse(.data, dictionary, ordinal = TRUE, drop = TRUE, locale = "us")
#'
#' @details This is typically the last function to be executed before rebuilding and replacing.
#'
#' @param .data A \code{postmastr} object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_append} to be used to standardize
#'     specific street names.
#'
#' @param drop A logical scalar; if \code{TRUE}, the \code{pm.address} variable will
#'     be dropped from the \code{postmastr} object.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new character variable \code{pm.unit} that contains
#'
#' @export
pm_street_parse <- function(.data, dictionary = NULL, ordinal = TRUE, drop = TRUE, locale = "us"){

  # global bindings
  pm.address = pm.street = NULL

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

  # parse
  .data <- dplyr::mutate(.data, pm.street = pm.address)

  # reorder output
  vars <- pm_reorder(.data)
  .data <- dplyr::select(.data, vars)

  # set dictionary to null if not specified
  if (missing(dictionary) == TRUE){
    dictionary <- NULL
  }

  # standardize street names
  .data <- pm_street_std(.data, var = "pm.street", dictionary = dictionary, ordinal = ordinal, locale = locale)

  # optionally drop pm.address
  if (drop == TRUE){

    .data <- dplyr::select(.data, -pm.address)

  }

  # return output
  return(.data)

}

