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
#' @importFrom purrr map
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
  }

  # iterate over observations
  if (locale == "us"){
    .data %>%
      dplyr::mutate(pm.hasUnit = purrr::map(pm.address, ~ pm_has_pattern(.x, dictionary = fullDic, end = FALSE))) %>%
      dplyr::mutate(pm.hasUnit = as.logical(pm.hasUnit)) -> out
  }

  # return scalar
  if (scalar == TRUE){
    out <- any(out$pm.hasUnit)
  }

  # return output
  return(out)

}

