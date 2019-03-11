#' Modify Improperly Parsed Addresses
#'
#' @description Difficult to parse addresses, particularly those that have
#'    alphanumeric house ranges as well as units associated with them,
#'    may be mis-parsed by \code{postmastr}. Once all parsing has been
#'    completed, but before the data are rebuilt, this function can be used
#'    to manually fix mis-parsed address data.
#'
#' @export
pm_mutate <- function(.data, pm.uid, pm.house, pm.houseRange, pm.houseFrac, pm.houseSuf,
                      pm.preDir, pm.street, pm.streetSuf, pm.sufDir, pm.unitType, pm.unitNum,
                      pm.city, pm.state, pm.zip, pm.zip4){

}
