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
#' @param expand_range A logical scalar; if \code{TRUE} (default), house numbers that
#'    contain a numerical range (i.e. \code{11-15 Main St}) will be expanded to specify
#'    all integer values within the range. Ranges that contain an alphanumeric value
#'    cannot be expanded and will be skipped.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new column \code{pm.house} that contains the house number.
#'     If a house number is not detected in the string, a value of \code{NA} will be
#'     returned. If the house number has a range (i.e. \code{11-15 Main St}), a
#'     list-column will also be returned. The list-column will contain the low and
#'     high values for ranges, and can optionally be expanded to include all integer
#'     values within a range if \code{expand_range} is equal to \code{TRUE}.
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
pm_house_parse <- function(.data, expand_range = TRUE, locale = "us"){

  # global bindings
  pm.uid = pm.address = pm.house = pm.houseRange = pm.houseLow = pm.houseHigh = pm.hasHouseRange = pm.hasHouse = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  if ("pm.hasHouse" %in% names(.data) == FALSE){
    .data <- pm_house_detect(.data)
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

    # parse
    out %>%
      pm_has_houseRange() %>%
      pm_parse_houseRange(expand_range = expand_range) -> out

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
pm_parse_houseRange <- function(.data, expand_range = TRUE){

  # global bindings
  . = pm.address = pm.uid = pm.hasHouseRange = pm.house = pm.houseRange = pm.houseLow =
    pm.houseHigh = pm.houseShort = pm.house2 = NULL

  # parse into two columns
  .data %>%
    dplyr::mutate(pm.houseRange = ifelse(pm.hasHouseRange == TRUE, pm.house, NA)) %>%
    dplyr::mutate(pm.houseRange = stringr::str_replace(pm.houseRange, pattern = "-", replacement = " ")) %>%
    dplyr::mutate(pm.houseLow = stringr::word(pm.houseRange, 1)) %>%
    dplyr::mutate(pm.houseHigh = stringr::word(pm.houseRange, 2)) -> .data

  # look for shortened house numbers
  .data %>%
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
    dplyr::select(-pm.house2, -pm.houseShort, -pm.hasHouseRange, -pm.houseRange) -> .data

  # construct list-col
  # if there is no range, a list of <chr [1]> with a value of NA is created, this is needed
  # so that tidyr::unnest() works down the road
  .data %>%
    dplyr::mutate(
      pm.houseRange = stringr::str_split(string = stringr::str_c(
        as.character(pm.houseLow), "-", as.character(pm.houseHigh)), pattern = "-")
    ) -> .data

  # expand numeric ranges
  if (expand_range == TRUE){

    # subset data without a range
    .data %>%
      dplyr::filter(is.na(pm.houseLow) == TRUE) %>%
      dplyr::select(-pm.houseLow, -pm.houseHigh) -> noRange

    # subset data with a range, identify ranges with alphanumeric values
    .data %>%
      dplyr::filter(is.na(pm.houseLow) == FALSE) %>%
      dplyr::select(-pm.houseLow, -pm.houseHigh) %>%
      pm_houseAlpha_detect() -> yesRange

    # subset ranges without alphanumeric values, expand
    yesRange %>%
      dplyr::filter(pm.hasAlpha.a == FALSE) %>%
      dplyr::select(-pm.hasAlpha.a) %>%
      dplyr::mutate(pm.houseRange = purrr::map(.x = pm.houseRange, .f = pm_parse_range)) -> yesRange_num

    # put data pack together
    yesRange %>%
      dplyr::filter(pm.hasAlpha.a == TRUE) %>%
      dplyr::select(-pm.hasAlpha.a) %>%
      dplyr::bind_rows(yesRange_num, ., noRange) %>%
      dplyr::arrange(pm.uid) -> .data

  } else if (expand_range == FALSE){

    .data <- dplyr::select(.data, -pm.houseLow, -pm.houseHigh)

  }

  # return output
  return(.data)

}

# Parse and Expand House Range
pm_parse_range <- function(x){

  # convert item to numeric
  vector <- as.numeric(x)

  # expand vector to include every other integer between low and high values
  out <- seq.int(from = vector[1], to = vector[2], by = 2)

  # convert to string
  out <- as.character(out)

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
#'     returned.
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
  pm.address = pm.uid = pm.house = pm.houseLow = pm.houseHigh = pm.houseFrac = pm.hasHouseFrac = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  if ("pm.hasHouseFrac" %in% names(.data) == FALSE){
    .data <- pm_houseFrac_detect(.data)
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

