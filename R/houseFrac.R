#' Do Any Addresses Have Fractional House Numbers
#'
#' @description Determine whether the fractional house number test returns any matches.
#'
#' @details A fractional house number is used in some parts of the United States.
#'    Fractional house numbers typically look like \code{123 1/2 Main St}.
#'    The U.S.P.S allows any fraction, though \code{1/2} appears commonly.
#'
#' @usage pm_houseFrac_any(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     one fractional house number and \code{FALSE} if they do not.
#'
#' @export
pm_houseFrac_any <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test and create output
  .data <- pm_houseFrac_detect(.data)
  out <- any(.data$pm.hasHouseFrac)

  # return output
  return(out)

}

#' Do All Addresses Have House Numbers
#'
#' @description Determine whether the fractional house number test returns matches for every
#'     observation.
#'
#' @details A fractional house number is used in some parts of the United States.
#'    Fractional house numbers typically look like \code{123 1/2 Main St}.
#'    The U.S.P.S allows any fraction, though \code{1/2} appears commonly.
#'
#' @usage pm_houseFrac_all(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain
#'     fractional house numbers and \code{FALSE} otherwise.
#'
#' @export
pm_houseFrac_all <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test and create output
  .data <- pm_houseFrac_detect(.data)
  out <- all(.data$pm.hasHouseFrac)

  # return output
  return(out)

}

#' Detect Presence of Fractional House Numbers
#'
#' @description Determine the presence of fractional house numbers in a string.
#'
#' @details A fractional house number is used in some parts of the United States.
#'    Fractional house numbers typically look like \code{123 1/2 Main St}.
#'    The U.S.P.S allows any fraction, though \code{1/2} appears commonly.
#'
#' @usage pm_houseFrac_detect(.data)
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
pm_houseFrac_detect <- function(.data){

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
  .data <- dplyr::mutate(.data, pm.hasHouseFrac = stringr::str_detect(stringr::word(pm.address, 1), pattern = "[1-9]/"))

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_houseFrac_detect
#'
#' @description Automatically subset the results of \link{pm_houseFrac_detect} to
#'    return only observations that were not found to include a fractional house number.
#'
#' @usage pm_houseFrac_none(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble containing only observations that were not found matched
#'     using the fractional house number test. The variable created by
#'     \link{pm_houseFrac_detect}, \code{pm.hasHouseFrac}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_houseFrac_none <- function(.data){

  # global bindings
  pm.hasHouseFrac = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # create output
  .data %>%
    pm_houseFrac_detect() %>%
    dplyr::filter(pm.hasHouseFrac == FALSE) %>%
    dplyr::select(-pm.hasHouseFrac) -> out

  # return output
  return(out)

}

#' Parse Fractional House Numbers
#'
#' @description Create a new column containing fractional house number data.
#'
#' @usage pm_houseFrac_parse(.data, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new column \code{pm.houseFrac} that contains the fractional house number.
#'     If a house number is not detected in the string, a value of \code{NA} will be
#'     returned. If a fractional is detected in an address that has a house range associated with
#'     it, a new element will be added to the vector stored in \code{pm.houseRange} for the
#'     fractional.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr word
#'
#' @export
pm_houseFrac_parse <- function(.data, locale = "us"){

  # global binding
  . = pm.address = pm.uid = pm.house = pm.houseLow = pm.houseHigh = pm.houseFrac =
    pm.hasHouseFrac = pm.houseRange = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # detect individual fractional addresses
  if ("pm.hasHouseFrac" %in% names(.data) == FALSE){
    fracDetect <- FALSE
    .data <- pm_houseFrac_detect(.data)
  } else if ("pm.hasHouseFrac" %in% names(.data) == TRUE){
    fracDetect <- TRUE
  }

  # parse
  .data %>%
    dplyr::mutate(pm.houseFrac = ifelse(pm.hasHouseFrac == TRUE, stringr::word(pm.address, 1), NA)) %>%
    dplyr::mutate(pm.address = ifelse(pm.hasHouseFrac == TRUE,
                                      stringr::word(pm.address, start = 2, end = -1),
                                      pm.address)) -> .data

  # remove pm.hasHouseFrac if not present initially
  if (fracDetect == FALSE){
    .data <- dplyr::select(.data, -pm.hasHouseFrac)
  }

  # add fractionals to house ranges
  if ("pm.houseRange" %in% names(.data) == TRUE){

    # identify non-fractional address ranges and fractional single addresses
    noRangeFrac <- dplyr::filter(.data, (is.na(pm.houseRange) == TRUE & is.na(pm.houseFrac) == TRUE) |
                                   (is.na(pm.houseRange) == FALSE & is.na(pm.houseFrac) == TRUE))

    # subset fractional address ranges, add to list-col vector, replace
    .data %>%
      dplyr::filter(is.na(pm.houseRange) == FALSE & is.na(pm.houseFrac) == FALSE) %>%
      dplyr::mutate(pm.houseRange = purrr::map(.x = pm.houseRange, .f = pm_add_fraction)) %>%
      dplyr::bind_rows(noRangeFrac, .) %>%
      dplyr::arrange(pm.uid) -> .data

  }

  # re-order variables
  if (locale == "us"){
    vars <- pm_reorder(.data)
    .data <- dplyr::select(.data, vars)
  }
  # return output
  return(.data)

}


pm_add_fraction <- function(x){

  frac <- stringr::str_c(x[length(x)], " ", "1/2")

  # add frac to end of vector
  vector <- c(x, frac)

}
