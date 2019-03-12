#' Do Any Addresses Have House Numbers
#'
#' @description Determine whether the house number test returns any matches.
#'
#' @usage pm_house_any(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     one house number and \code{FALSE} if they do not.
#'
#' @export
pm_house_any <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test and create output
  .data <- pm_house_detect(.data)
  out <- any(.data$pm.hasHouse)

  # return output
  return(out)

}

#' Do All Addresses Have House Numbers
#'
#' @description Determine whether the house number test returns matches for every
#'     observation.
#'
#' @usage pm_house_all(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain
#'     house numbers and \code{FALSE} otherwise.
#'
#' @export
pm_house_all <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test and create output
  .data <- pm_house_detect(.data)
  out <- all(.data$pm.hasHouse)

  # return output
  return(out)

}

#' Detect Presence of House Numbers
#'
#' @description Determine the presence of house numbersin a string.
#'
#' @usage pm_house_detect(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble with a new logical variable \code{pm.hasHouse} that is
#'     \code{TRUE} if a house number is found in the first word of the address
#'     and \code{FALSE} otherwise.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom stringr word
#'
#' @export
pm_house_detect <- function(.data){

  # global bindings
  pm.address = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # detect pattern
  .data <- dplyr::mutate(.data, pm.hasHouse = stringr::str_detect(stringr::word(pm.address, 1), pattern = "[0-9]"))

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_house_detect
#'
#' @description Automatically subset the results of \link{pm_house_detect} to
#'    return only observations that were not found to include a house number
#'
#' @usage pm_house_none(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble containing only observations that were not found matched
#'     using the house number test. The variable created by \link{pm_house_detect},
#'     \code{pm.hasHouse}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_house_none <- function(.data){

  # global bindings
  pm.hasHouse = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # create output
  .data %>%
    pm_house_detect() %>%
    dplyr::filter(pm.hasHouse == FALSE) %>%
    dplyr::select(-pm.hasHouse) -> out

  # return output
  return(out)

}

#' Parse House Numbers
#'
#' @description Parse house number data out from \code{pm.address}.
#'
#' @usage pm_house_parse(.data, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new column \code{pm.house} that contains the house number.
#'     If a house number is not detected in the string, a value of \code{NA} will be
#'     returned.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom purrr map
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#' @importFrom stringr str_length
#' @importFrom stringr str_split
#' @importFrom stringr str_sub
#' @importFrom stringr str_replace
#' @importFrom stringr word
#'
#' @export
pm_house_parse <- function(.data, locale = "us"){

  # global bindings
  pm.uid = pm.address = pm.house = pm.houseRange = pm.houseLow = pm.houseHigh = pm.hasHouse = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # detect individual addresses
  if ("pm.hasHouse" %in% names(.data) == FALSE){
    houseDetect <- FALSE
    .data <- pm_house_detect(.data)
  } else if ("pm.hasHouse" %in% names(.data) == TRUE){
    houseDetect <- TRUE
  }

  # parse
  .data %>%
    dplyr::mutate(pm.house = ifelse(pm.hasHouse == TRUE, stringr::word(pm.address, 1), NA)) %>%
    dplyr::mutate(pm.address = ifelse(pm.hasHouse == TRUE,
                                      stringr::word(pm.address, start = 2, end = -1),
                                      pm.address)) %>%
    dplyr::select(pm.uid, pm.address, pm.house, dplyr::everything()) -> .data

  # remove pm.hasHouse if not present initially
  if (houseDetect == FALSE){
    .data <- dplyr::select(.data, -pm.hasHouse)
  }

  # reorder variables
  if (locale == "us"){
    vars <- pm_reorder(.data)
    .data <- dplyr::select(.data, vars)
  }

  # return output
  return(.data)

}
