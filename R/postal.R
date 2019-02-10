#' Do Any Addresses Have Postal Codes
#'
#' @description Determine whether the postal code test returns any matches.
#'
#' @usage pm_any_postal(.data, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     one postal code and \code{FALSE} if they do not.
#'
#' @export
pm_any_postal <- function(.data, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # test and create output
  if (locale == "us"){
    .data <- pm_has_postal(.data, locale = locale)

    # create output
    out <- any(.data$pm.hasZip)
  }

  # return output
  return(out)

}

#' Do All Addresses Have Postal Codes
#'
#' @description Determine whether the postal code test returns matches for every
#'     observation.
#'
#' @usage pm_all_postal(.data, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain postal
#'     codes and \code{FALSE} otherwise.
#'
#' @export
pm_all_postal <- function(.data, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # test and create output
  if (locale == "us"){
    .data <- pm_has_postal(.data, locale = locale)

    # create output
    out <- all(.data$pm.hasZip)
  }

  # return output
  return(out)

}

#' Detect Presence of Postal Codes
#'
#' @description Determine the presence of U.S. zip-codes in a string. This will
#'     identify both five digit zip-codes (e.g. \code{63108}) as well as zip+4
#'     codes (e.g. \code{63108-3412}). The zip-code must be the final word in
#'     the string to return \code{TRUE}.
#'
#' @usage pm_has_postal(.data, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.hasZip} that is
#'     \code{TRUE} if a zip-code is found in the last word of the address
#'     and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @importFrom stringr word
#'
#' @export
pm_has_postal <- function(.data, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # detect pattern
  if (locale == "us"){
    out <- pm_has_zip_us(.data)
  }

  # return output
  return(out)

}

# identify american zip codes
pm_has_zip_us <- function(.data){

  # create bindings for global variables
  pm.address = pm.last = NULL

  # detect pattern
  .data %>%
    dplyr::mutate(pm.last = stringr::word(pm.address, -1)) %>%
    dplyr::mutate(pm.hasZip = stringr::str_detect(pm.last, "([0-9]{5})")) %>%
    dplyr::select(-pm.last) -> out

  # return output
  return(out)

}

#' Return Only Unmatched Observations From pm_has_postal
#'
#' @description Automatically subset the results of \link{pm_has_postal} to
#'    return only observations that were not found to include a postal (zip) code.
#'
#' @usage pm_no_postal(.data,  locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble containing only observations that were not found in
#'     the dictionary. The variable created by \link{pm_has_postal},
#'     \code{pm.hasZip}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_no_postal <- function(.data, locale = "us"){

  # global bindings
  pm.hasZip = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # create output
  .data %>%
    pm_has_postal(locale = locale) %>%
    dplyr::filter(pm.hasZip == FALSE) %>%
    dplyr::select(-pm.hasZip) -> out

  # return output
  return(out)

}

#' Parse Postal Codes
#'
#' @description Create a new column containing postal code data.
#'
#' @usage pm_parse_postal(.data, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new column \code{pm.zip} that contains the zip-code.
#'     If a postal code is not detected in the string, a value of \code{NA} will be
#'     returned. If the "zip+4" formatting is detected in the string, a second column
#'     named \code{pm.zip4} will be returned with the carrier route parsed out of the
#'     five-digit postal code.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr word
#'
#' @export
pm_parse_postal <- function(.data, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # identify zip-code
  out <- pm_has_postal(.data, locale = locale)

  # parse
  if (locale == "us"){
    out <- pm_parse_zip_us(out)
  }

  # return output
  return(out)

}

# parse American zip codes
pm_parse_zip_us <- function(.data){

  # global bindings
  pm.hasZip4 = NULL

  # create bindings for global variables
  pm.address = pm.hasZip = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # parse
  .data %>%
    dplyr::mutate(pm.zip =
                    ifelse(pm.hasZip == TRUE,
                           stringr::word(pm.address, start = -1),
                           NA)) %>%
    dplyr::mutate(pm.address =
                    ifelse(pm.hasZip == TRUE,
                           stringr::word(pm.address, start = 1, end = -2),
                           pm.address)) %>%
    dplyr::select(-pm.hasZip) -> .data

  # look for presence of zip+4
  .data <- pm_has_zip4_us(.data)

  # parse zip+4 if necessary
  if (any(.data$pm.hasZip4) == TRUE){

    .data %>%
      pm_parse_zip4_us() %>%
      dplyr::select(-pm.hasZip4) -> .data

  } else if (any(.data$pm.hasZip4) == FALSE){

    .data <- dplyr::select(.data, -pm.hasZip4)

  }

}

# check for zip+4
pm_has_zip4_us <- function(.data){

  # global bindings
  pm.hasZip4 = pm.zip = NULL

  .data %>%
    dplyr::mutate(pm.hasZip4 = ifelse(stringr::str_detect(pm.zip, pattern = "-") == TRUE, TRUE, FALSE)) -> out

}

# parse zip+4
pm_parse_zip4_us <- function(.data){

  # global bindings
  pm.hasZip4 = pm.zip = NULL

  .data %>%
    dplyr::mutate(pm.zip4 = ifelse(pm.hasZip4 == TRUE, stringr::word(pm.zip, 2, sep = "-"), NA)) %>%
    dplyr::mutate(pm.zip = ifelse(pm.hasZip4 == TRUE, stringr::word(pm.zip, 1, sep = "-"), pm.zip)) -> out

}
