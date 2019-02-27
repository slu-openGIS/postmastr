#' Do Any Addresses Have House Numbers
#'
#' @description Determine whether the house number test returns any matches.
#'
#' @usage pm_any_house(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     one house number and \code{FALSE} if they do not.
#'
#' @export
pm_any_house <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test and create output
  .data <- pm_has_house(.data)
  out <- any(.data$pm.hasHouse)

  # return output
  return(out)

}

#' Do All Addresses Have House Numbers
#'
#' @description Determine whether the house number test returns matches for every
#'     observation.
#'
#' @usage pm_all_house(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain
#'     house numbers and \code{FALSE} otherwise.
#'
#' @export
pm_all_house <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test and create output
  .data <- pm_has_house(.data)
  out <- all(.data$pm.hasHouse)

  # return output
  return(out)

}

#' Detect Presence of House Numbers
#'
#' @description Determine the presence of house numbersin a string.
#'
#' @usage pm_has_house(.data)
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
pm_has_house <- function(.data){

  # global bindings
  pm.address = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error.")
  }

  # detect pattern
  .data <- dplyr::mutate(.data, pm.hasHouse = stringr::str_detect(stringr::word(pm.address, 1), pattern = "[0-9]"))

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_has_house
#'
#' @description Automatically subset the results of \link{pm_has_house} to
#'    return only observations that were not found to include a house number
#'
#' @usage pm_no_house(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble containing only observations that were not found matched
#'     using the house number test. The variable created by \link{pm_has_house},
#'     \code{pm.hasHouse}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_no_house <- function(.data){

  # global bindings
  pm.hasHouse = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # create output
  .data %>%
    pm_has_house() %>%
    dplyr::filter(pm.hasHouse == FALSE) %>%
    dplyr::select(-pm.hasHouse) -> out

  # return output
  return(out)

}

#' Parse House Numbers
#'
#' @description Create a new column containing house number data.
#'
#' @usage pm_parse_house(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble with a new column \code{pm.house} that contains the house number.
#'     If a house number is not detected in the string, a value of \code{NA} will be
#'     returned.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr word
#'
#' @export
pm_parse_house <- function(.data, locale = "us"){

  # global bindings
  pm.uid = pm.address = pm.house = pm.houseRange = pm.houseLow = pm.houseHigh = pm.hasHouseRange = pm.hasHouse = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error.")
  }

  if ("pm.hasHouse" %in% names(.data) == FALSE){
    .data <- pm_has_house(.data)
  }

  # parse
  .data %>%
    dplyr::mutate(pm.house = ifelse(pm.hasHouse == TRUE, stringr::word(pm.address, 1), NA)) %>%
    dplyr::mutate(pm.address = ifelse(pm.hasHouse == TRUE,
                                      stringr::word(pm.address, start = 2, end = -1),
                                      pm.address)) %>%
    dplyr::select(-pm.hasHouse) %>%
    dplyr::select(pm.uid, pm.address, pm.house, dplyr::everything()) -> out

  # address ranges
  if (pm_any_houseRange(out) == TRUE){

    # set indicator
    range <- TRUE

    # parse
    out %>%
      pm_has_houseRange() %>%
      pm_parse_houseRange() -> out

  } else if (pm_any_houseRange(out) == FALSE){
    range <- FALSE
  }

  # reorder variables
  if (locale == "us"){
    vars <- pm_reorder(.data)
    .data <- dplyr::select(.data, vars)
  }

  # return output
  return(out)

}

# logic test for house ranges
pm_any_houseRange <- function(.data){

  # test and create output
  .data <- pm_has_houseRange(.data)
  out <- any(.data$pm.hasHouseRange)

  # return output
  return(out)

}

# detect address ranges
pm_has_houseRange <- function(.data){

  # global binding
  pm.house = NULL

  # detect pattern
  .data <- dplyr::mutate(.data, pm.hasHouseRange = stringr::str_detect(pm.house, pattern = "-"))

  # return output
  return(.data)

}

# parse house range
pm_parse_houseRange <- function(.data){

  # global bindings
  pm.address = pm.uid = pm.hasHouseRange = pm.house = pm.houseRange = pm.houseLow = pm.houseHigh = pm.houseShort = pm.house2 = NULL

  # parse into two columns
  .data %>%
    dplyr::mutate(pm.houseRange = ifelse(pm.hasHouseRange == TRUE, pm.house, NA)) %>%
    dplyr::mutate(pm.houseRange = stringr::str_replace(pm.houseRange, pattern = "-", replacement = " ")) %>%
    dplyr::mutate(pm.houseLow = stringr::word(pm.houseRange, 1)) %>%
    dplyr::mutate(pm.houseHigh = stringr::word(pm.houseRange, 2)) -> out

  # look for shortened house numbers
  out %>%
    dplyr::mutate(pm.houseShort = ifelse(stringr::str_length(pm.houseLow) > stringr::str_length(pm.houseHigh), TRUE, FALSE)) %>%
    dplyr::mutate(pm.houseHigh = ifelse(pm.houseShort == TRUE,
                                        stringr::str_c(stringr::str_sub(pm.houseLow,
                                                                        start = 1,
                                                                        end = stringr::str_length(pm.houseLow)-
                                                                          stringr::str_length(pm.houseHigh)),
                                                                        pm.houseHigh),
                                        pm.houseHigh)) %>%
    dplyr::mutate(pm.house2 = ifelse(pm.houseShort == TRUE, stringr::str_c(pm.houseLow, "-", pm.houseHigh), pm.house)) %>%
    dplyr::mutate(pm.house = ifelse(is.na(pm.house2) == FALSE, pm.house2, pm.house)) %>%
    dplyr::select(-pm.house2, -pm.houseShort) -> out

  out %>%
    dplyr::select(-pm.hasHouseRange, -pm.houseRange) %>%
    dplyr::select(pm.uid, pm.address, pm.house, pm.houseLow, pm.houseHigh, dplyr::everything()) -> out

  # return output
  return(out)

}

#' Do Any Addresses Have Fractional House Numbers
#'
#' @description Determine whether the fractional house number test returns any matches.
#'
#' @details A fractional house number is used in some parts of the United States.
#'    Fractional house numbers typically look like \code{123 1/2 Main St}.
#'    The U.S.P.S allows any fraction, though \code{1/2} appears commonly.
#'
#' @usage pm_any_houseFrac(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     one fractional house number and \code{FALSE} if they do not.
#'
#' @export
pm_any_houseFrac <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test and create output
  .data <- pm_has_houseFrac(.data)
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
#' @usage pm_all_houseFrac(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain
#'     fractional house numbers and \code{FALSE} otherwise.
#'
#' @export
pm_all_houseFrac <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test and create output
  .data <- pm_has_houseFrac(.data)
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
#' @usage pm_has_houseFrac(.data)
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
pm_has_houseFrac <- function(.data){

  # global bindings
  pm.address = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error.")
  }

  # detect pattern
  .data <- dplyr::mutate(.data, pm.hasHouseFrac = stringr::str_detect(stringr::word(pm.address, 1), pattern = "[1-9]/"))

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_has_houseFrac
#'
#' @description Automatically subset the results of \link{pm_has_houseFrac} to
#'    return only observations that were not found to include a fractional house number.
#'
#' @usage pm_no_houseFrac(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble containing only observations that were not found matched
#'     using the fractional house number test. The variable created by
#'     \link{pm_has_houseFrac}, \code{pm.hasHouseFrac}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_no_houseFrac <- function(.data){

  # global bindings
  pm.hasHouseFrac = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # create output
  .data %>%
    pm_has_houseFrac() %>%
    dplyr::filter(pm.hasHouseFrac == FALSE) %>%
    dplyr::select(-pm.hasHouseFrac) -> out

  # return output
  return(out)

}

#' Parse Fractional House Numbers
#'
#' @description Create a new column containing fractional house number data.
#'
#' @usage pm_parse_houseFrac(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble with a new column \code{pm.houseFrac} that contains the fractional house number.
#'     If a house number is not detected in the string, a value of \code{NA} will be
#'     returned.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr word
#'
#' @export
pm_parse_houseFrac <- function(.data, locale = "us"){

  # global binding
  pm.address = pm.uid = pm.house = pm.houseLow = pm.houseHigh = pm.houseFrac = pm.hasHouseFrac = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error.")
  }

  if ("pm.hasHouseFrac" %in% names(.data) == FALSE){
    .data <- pm_has_houseFrac(.data)
  }

  # parse
  .data %>%
    dplyr::mutate(pm.houseFrac = ifelse(pm.hasHouseFrac == TRUE, stringr::word(pm.address, 1), NA)) %>%
    dplyr::mutate(pm.address = ifelse(pm.hasHouseFrac == TRUE,
                                      stringr::word(pm.address, start = 2, end = -1),
                                      pm.address)) %>%
    dplyr::select(-pm.hasHouseFrac) -> out

  # re-order variables
  if (locale == "us"){
    vars <- pm_reorder(.data)
    .data <- dplyr::select(.data, vars)
  }

  # return output
  return(out)

}

