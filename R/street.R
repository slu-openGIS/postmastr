#' Parse Street Names
#'
#' @description Converts the remaining text of \code{pm.address} to title case and stores
#'     it in a new variable named \code{pm.street}.
#'
#' @details This is typically the last function to be executed before rebuilding and replacing.
#'
#' @param .data A \code{postmastr} object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_dictionary} to be used
#'     as a master list for streets.
#' @param drop A logical scalar; if \code{TRUE}, the \code{pm.address} variable will
#'     be dropped from the \code{postmastr} object.
#'
#' @return A tibble with a new character variable \code{pm.street} that contains
#'     the two-letter abbreviation for the given U.S. state. Variables are automatically
#'     re-ordered, so the new vector will not be in the last position of the tibble.
#'
#' @export
pm_parse_street <- function(.data, dictionary, drop = TRUE){

  # global bindings
  pm.address = NULL

  # parse and convert to title case
  .data <- dplyr::mutate(.data, pm.street = stringr::str_to_title(pm.address))

  # reorder output
  vars <- pm_reorder(.data)
  .data <- dplyr::select(.data, vars)

  # optionally drop pm.address
  if (drop == TRUE){

    .data <- dplyr::select(.data, -pm.address)

  }

  # return output
  return(.data)

}
