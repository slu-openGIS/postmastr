#' Does Intersection Dictionary Return Any Matches
#'
#' @description Determine whether the intersection dictionary returns any matches.
#'
#' @usage pm_intersect_any(.data, dictionary)
#'
#' @param .data A raw data set that has been pre-processed with \code{\link{pm_identify}}
#' @param var A character variable containing address data to be parsed
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for intersection values.
#'
#' @return A single logical scalar is returned that is
#'     \code{TRUE} if the data contains at least intersection from the given
#'     dictionary and \code{FALSE} if they do not.
#'
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
pm_intersect_any <- function(.data, dictionary){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Pre-process yur data with pm_identify before proceeding.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # test dictionary
  .data <- pm_intersect_detect(.data, var = !!varQ, dictionary = dictionary)

  # create output
  out <- any(.data$pm.hasIntersect)

  # return output
  return(out)

}

#' Does Intersection Dictionary Return a Match for All Observations
#'
#' @description Determine whether the intersection dictionary returns any matches.
#'
#' @usage pm_intersect_all(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable containing address data to be parsed
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list intersection values.
#'
#' @return A single logical scalar is returned that is
#'     \code{TRUE} if the data contains at least one intersection from the given
#'     dictionary and \code{FALSE} if they do not.
#'
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
pm_intersect_all <- function(.data, var, dictionary){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Pre-process yur data with pm_identify before proceeding.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # test dictionary
  .data <- pm_intersect_detect(.data, var = !!varQ, dictionary = dictionary)

  # create output
  out <- all(.data$pm.hasIntersect)

  # return output
  return(out)

}

#' Detect Presence of Intersection in Address
#'
#' @description Determine the presence of intersection in a string.
#'
#' @usage pm_intersect_detect(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable containing address data to be parsed
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list intersection values.
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
pm_intersect_detect <- function(.data, var, dictionary){

  # create bindings for global variables
  pm.address = pm.hasIntersect = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Pre-process yur data with pm_identify before proceeding.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # minimize dictionary
  dict <- paste(dictionary$intersect.input, collapse = "|")

  # check observations
  .data <- dplyr::mutate(.data, pm.hasIntersect = stringr::str_detect(!!varQ,
                                                                 pattern = stringr::str_c("\\b(", dict, ")\\b")))

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_intersect_detect
#'
#' @description Automatically subset the results of \link{pm_intersect_detect} to
#'    return only observations that were not found in the dictionary.
#'
#' @usage pm_intersect_none(.data, dictionary)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable containing address data to be parsed
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for intersections.
#'
#' @return A tibble containing only observations that were not found in
#'     the dictionary. The variable created by \link{pm_intersect_detect},
#'     \code{pm.hasIntersect}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#'
#' @export
pm_intersect_none <- function(.data, dictionary){

  # global bindings
  pm.hasIntersect = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Pre-process yur data with pm_identify before proceeding.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # create output
  .data %>%
    pm_intersect_detect(var = !!varQ, dictionary = dictionary) %>%
    dplyr::filter(pm.hasIntersect == FALSE) %>%
    dplyr::select(-pm.hasIntersect) -> out

  # return output
  return(out)

}
