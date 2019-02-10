#' Do Any Addresses Have House Numbers
#'
#' @description Determine whether the house number test returns any matches.
#'
#' @usage pm_any_house(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     one house number and \code{FALSE} if they do not.
#'
#' @export
pm_any_house <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test and create output
  .data <- pm_has_house(.data)
  out <- any(.data$pm.hasHouse)

  # return output
  return(out)

}

#' Do All Addresses Have House Numbers
#'
#' @description Determine whether the house number test returns matches for every
#'     observation.
#'
#' @usage pm_all_house(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain
#'     house numbers and \code{FALSE} otherwise.
#'
#' @export
pm_all_house <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test and create output
  .data <- pm_has_house(.data)
  out <- all(.data$pm.hasHouse)

  # return output
  return(out)

}

#' Detect Presence of House Numbers
#'
#' @description Determine the presence of house numbersin a string.
#'
#' @usage pm_has_house(.data)
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
pm_has_house <- function(.data){

  # global bindings
  pm.address = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error.")
  }

  # detect pattern
  .data <- dplyr::mutate(.data, pm.hasHouse = stringr::str_detect(stringr::word(pm.address, 1), pattern = "[0-9]"))

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_has_house
#'
#' @description Automatically subset the results of \link{pm_has_house} to
#'    return only observations that were not found to include a house number
#'
#' @usage pm_no_house(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble containing only observations that were not found matched
#'     using the house number test. The variable created by \link{pm_has_house},
#'     \code{pm.hasHouse}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_no_house <- function(.data){

  # global bindings
  pm.hasHouse = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # create output
  .data %>%
    pm_has_house() %>%
    dplyr::filter(pm.hasHouse == FALSE) %>%
    dplyr::select(-pm.hasHouse) -> out

  # return output
  return(out)

}

#' Parse House Numbers
#'
#' @description Create a new column containing house number data.
#'
#' @usage pm_parse_house(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble with a new column \code{pm.house} that contains the house number.
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
pm_parse_house <- function(.data){

  # global bindings
  pm.uid = pm.address = pm.house = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error.")
  }

  # parse
  .data %>%
    dplyr::mutate(pm.house = stringr::word(pm.address, 1)) %>%
    dplyr::mutate(pm.address = stringr::word(pm.address, start = 2, end = -1)) %>%
    dplyr::select(pm.uid, pm.address, pm.house, dplyr::everything()) -> out

  # return output
  return(out)

}

