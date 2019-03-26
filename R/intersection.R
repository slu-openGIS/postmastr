#' Detect Presence of Intersection in Address
#'
#' @description Determine the presence of intersection in a string.
#'
#' @usage pm_intersect_detect(.data, var, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable containing address data to be parsed
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list intersection values.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.hasIntersect} that is
#'     \code{TRUE} if an intersection from the given dictionary is found in the
#'     address and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#'
#' @export
pm_intersect_detect <- function(.data, var, dictionary, locale = "us"){

  # create bindings for global variables
  pm.address = pm.hasIntersect = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Pre-process yur data with pm_identify before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- pm_dictionary(type = "intersection")
    }
  }

  # load dictionary if NULL
  if (is.null(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- pm_dictionary(type = "intersection")
    }
  }


  # minimize dictionary
  if (locale == "us"){
    dict <- paste(dictionary$intersect.input, collapse = "|")
  }

  # check observations
  if (locale == "us"){
    .data <- dplyr::mutate(.data, pm.hasIntersect = stringr::str_detect(!!varQ,
                                                                 pattern = stringr::str_c("\\b(", dict, ")\\b")))
  }

  # return output
  return(.data)

}
