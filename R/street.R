#' Parse Street Names
#'
#' @export
pm_parse_street <- function(.data, dictionary){

  .data <- dplyr::mutate(.data, pm.street = stringr::str_to_title(pm.address))

  # reorder output
  .data <- dplyr::select(.data, -pm.streetSuf, dplyr::everything())

  # return output
  return(.data)

}
