#' Modify Improperly Parsed Addresses
#'
#' @description Difficult to parse addresses, particularly those that have
#'    alphanumeric house ranges as well as units associated with them,
#'    may be mis-parsed by \code{postmastr}. This function can be used
#'    to manually fix mis-parsed address data.
#'
#' @usage pm_mutate(.data, uid, address, house, houseRange, houseFrac, houseSuf,
#'     preDir, street, streetSuf, sufDir, unitType, unitNum,
#'     city, state, zip, zip4, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param uid A \code{pm.uid} value to edit.
#' @param address Optional; the new value for the \code{pm.address} variable.
#' @param house Optional; the new value for the \code{pm.house} variable.
#' @param houseRange Optional; the new value (or vector) for the \code{pm.houseRange}
#'     variable.
#' @param houseFrac Optional; the new value for the \code{pm.houseFrac} variable.
#' @param houseSuf Optional; the new value for the \code{pm.houseSuf} variable.
#' @param preDir Optional; the new value for the \code{pm.preDir} variable.
#' @param street Optional; the new value for the \code{pm.street} variable.
#' @param streetSuf Optional; the new value for the \code{pm.streetSuf} variable.
#' @param sufDir Optional; the new value for the \code{pm.sufDir} variable.
#' @param unitType Optional; the new value for the \code{pm.unitType} variable.
#' @param unitNum Optional; the new value (or vector) for the \code{pm.unitNum}
#'     variable.
#' @param city Optional; the new value for the \code{pm.city} variable.
#' @param state Optional; the new value for the \code{pm.state} variable.
#' @param postal Optional; the new value for the \code{pm.postal} variable.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @export
pm_mutate <- function(.data, uid, address, house, houseRange, houseFrac, houseSuf,
                      preDir, street, streetSuf, sufDir, unitType, unitNum,
                      city, state, postal, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  if (uid %in% .data$pm.uid == FALSE){
    stop("The given 'uid' value is not found in your postmastr object.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # remove elements from function call
  paramList[-1] %>%
    purrr::list_modify(
      ".data" = NULL,
      "uid" = NULL,
      locale = NULL
    ) -> paramList

  # return output
  return(paramList)

}
