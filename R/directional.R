#' Do Any Addresses Have Prefix or Suffix Directionals
#'
#' @description Determine whether the directional returns any matches.
#'
#' @usage pm_any_dir(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list of directionals.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     one directional and \code{FALSE} if they do not.
#'
#' @export
pm_any_dir <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_has_dir(.data, locale = locale)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_has_dir(.data, dictionary = dictionary, locale = locale)
  }

  # create output
  out <- any(.data$pm.hasDir)

  # return output
  return(out)

}

#' Do All Addresses Have Prefix or Suffix Directionals
#'
#' @description Determine whether the directional test returns matches for every
#'     observation.
#'
#' @usage pm_all_dir(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list of directionals.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain
#'     directionals and \code{FALSE} otherwise.
#'
#' @export
pm_all_dir <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # test dictionary
  if (missing(dictionary) == TRUE){
    .data <- pm_has_dir(.data, locale = locale)
  } else if (missing(dictionary) == FALSE){
    .data <- pm_has_dir(.data, dictionary = dictionary, locale = locale)
  }

  # create output
  out <- all(.data$pm.hasDir)

  # return output
  return(out)

}

#' Detect Presence of Prefix or Suffix Directionals
#'
#' @description Determine the presence of house numbersin a string.
#'
#' @usage pm_has_dir(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list of directionals.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.hasDir} that is
#'     \code{TRUE} if a directional is found in the first or last word of
#'     the address and \code{FALSE} otherwise.
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_detect
#' @importFrom stringr word
#'
#' @export
pm_has_dir <- function(.data, dictionary, locale = "us"){

  # global bindings
  pm.address = ...preDir = ...sufDir = NULL

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

  # minimize dictionary
  if (locale == "us"){
    dict <- paste(dictionary$dir.input, collapse = "|")
  }

  # detect pattern
  .data %>%
    dplyr::mutate(...preDir = stringr::str_detect(pm.address, pattern = stringr::str_c("^\\b(", dict, ")\\b"))) %>%
    dplyr::mutate(...sufDir = stringr::str_detect(pm.address, pattern = stringr::str_c("\\b(", dict, ")\\b$"))) %>%
    dplyr::mutate(pm.hasDir = ifelse(...preDir == TRUE | ...sufDir == TRUE, TRUE, FALSE)) %>%
    dplyr::select(-...preDir, -...sufDir) -> .data

  # return output
  return(.data)

}
