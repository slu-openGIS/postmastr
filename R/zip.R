#' Detect Presence of Zip Codes
#'
#' @description Determine the presence of U.S. zip-codes in a string. This will
#'     identify both five digit zip-codes (e.g. \code{63108}) as well as zip+4
#'     codes (e.g. \code{63108-3412}). The zip-code must be the final word in
#'     the string to return \code{TRUE}.
#'
#' @param .data A postmastr object (\code{pm_subset})
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.isZip} that is
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
pm_has_zip <- function(.data, locale = "us"){

  # check for object and key variables
  if (pm_is_subset(working_data) == FALSE){
    stop("Error.")
  }

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


  # return output
  return(out)

}

# identify american zip codes
pm_has_zip_us <- function(.data){

  # detect pattern
  .data %>%
    dplyr::mutate(pm.last = stringr::word(pm.address, -1)) %>%
    dplyr::mutate(pm.isZip = stringr::str_detect(pm.last, "([0-9]{5})")) %>%
    dplyr::select(-pm.last) -> out

  # return output
  return(out)

}

#' Parse Zip-Codes
#'
#' @description Create a new column containing zip-code data.
#'
#' @usage pm_parse_zip(.data, locale = "us")
#'
#' @param .data A postmastr object (\code{pm_subset})
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new column \code{pm.zip} that contains the zip-code.
#'     If a zip-code is not detected in the string, a value of \code{NA} will be
#'     returned. If it does not yet exist, a copy of the address variable will
#'     be created in \code{pm.address} and returned with zip-codes removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr word
#'
#' @export
pm_parse_zip <- function(.data, locale = "us"){

  # check for object and key variables
  if (pm_is_subset(working_data) == FALSE){
    stop("Error.")
  }

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
  out <- pm_has_zip(.data, locale = locale)

  # parse
  if (locale == "us"){
    out <- pm_parse_zip_us(out)
  }

  # return output
  return(out)

}

# parse American zip codes
pm_parseZip_us <- function(.data){

  # save parameters to list
  paramList <- as.list(match.call())

  # parse
  .data %>%
    dplyr::mutate(pm.zip =
                    ifelse(pm.isZip == TRUE,
                           stringr::word(pm.address, start = -1),
                           NA)) %>%
    dplyr::mutate(pm.address =
                    ifelse(pm.isZip == TRUE,
                           stringr::word(pm.address, start = 1, end = -2),
                           pm.address)) %>%
    dplyr::select(-pm.isZip) -> out

}
