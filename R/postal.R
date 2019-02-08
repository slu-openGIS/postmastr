#' Detect Presence of Postal Codes
#'
#' @description Determine the presence of U.S. zip-codes in a string. This will
#'     identify both five digit zip-codes (e.g. \code{63108}) as well as zip+4
#'     codes (e.g. \code{63108-3412}). The zip-code must be the final word in
#'     the string to return \code{TRUE}.
#'
#' @usage pm_has_postal(.data, scalar = TRUE, locale = "us")
#'
#' @param .data A postmastr object (\code{pm_subset})
#' @param scalar If \code{TRUE}, a single logical scalar is returned; otherwise if
#'     \code{FALSE}, a logical vector is returned.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return If \code{scalar = TRUE}, a single logical scalar is returned that is
#'     \code{TRUE} if the data contain postal codes and \code{FALSE} if they do not.
#'     If \code{scalar = FALSE} a tibble with a new logical variable \code{pm.hasZip}
#'     that is \code{TRUE} if a zip-code is found in the last word of the address
#'     and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_detect
#' @importFrom stringr word
#'
#' @export
pm_has_postal <- function(.data, scalar = TRUE, locale = "us"){

  # create bindings for global variables
  working_data = NULL

  # check for object and key variables
  if (pm_has_uid(working_data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(working_data) == FALSE){
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

  # return scalar
  if (scalar == TRUE){
    out <- any(out$pm.hasZip)
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

#' Parse Postal Codes
#'
#' @description Create a new column containing postal code data.
#'
#' @usage pm_parse_zip(.data, locale = "us")
#'
#' @param .data A postmastr object (\code{pm_subset})
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new column \code{pm.zip} that contains the zip-code.
#'     If a postal code is not detected in the string, a value of \code{NA} will be
#'     returned.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr word
#'
#' @export
pm_parse_postal <- function(.data, locale = "us"){

  # create bindings for global variables
  working_data = NULL

  # check for object and key variables
  if (pm_has_uid(working_data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(working_data) == FALSE){
    stop("Error.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # identify zip-code
  out <- pm_has_postal(.data, scalar = FALSE, locale = locale)

  # parse
  if (locale == "us"){
    out <- pm_parse_zip_us(out)
  }

  # return output
  return(out)

}

# parse American zip codes
pm_parse_zip_us <- function(.data){

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
    dplyr::select(-pm.hasZip) -> out

}
