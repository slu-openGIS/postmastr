#' Parse Street Names
#'
#' @export
pm_parse_street <- function(.data, dictionary, drop = TRUE){

  .data <- dplyr::mutate(.data, pm.street = stringr::str_to_title(pm.address))

  # reorder output
  .data <- dplyr::select(.data, -pm.streetSuf, dplyr::everything())

  # optionally drop pm.address
  if (drop == TRUE){

    .data <- dplyr::select(.data, -pm.address)

  }

  # return output
  return(.data)

}
