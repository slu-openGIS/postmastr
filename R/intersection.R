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
  pm.address = pm.hasIntersect = intersect.input = NULL

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

  # modify forward slash
  if ("/" %in% dictionary$intersect.input == TRUE){
    dict1 <- "/"

    dictionary <- dplyr::filter(dictionary, intersect.input != "/")
  }

  # minimize dictionary
  if (locale == "us"){
    dict2 <- paste(dictionary$intersect.input, collapse = "|")
  }

  # check observations
  if (locale == "us"){
    .data %>%
      dplyr::mutate(pm.hasIntersect = stringr::str_detect(!!varQ, pattern = dict1)) %>%
      dplyr::mutate(pm.hasIntersect = ifelse(stringr::str_detect(!!varQ,
                                     pattern = stringr::str_c("\\b(", dict2, ")\\b")) == TRUE,
                                     TRUE, pm.hasIntersect)) -> .data
  }

  # return output
  return(.data)

}

#' Convert Intersections to Long Form
#'
#' @description Split intersections into x and y streets.
#'
#' @usage pm_intersect_longer(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_dictionary} to be used
#'     as a master list for intersection operators.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @export
pm_intersect_longer <- function(.data, dictionary, locale = "us"){

  # global bindings
  pm.address = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # load dictionary if not specified
  if (missing(dictionary) == TRUE){
    if (locale == "us"){
      dictionary <- pm_dictionary(type = "intersection")
    }
  }

  # minimize dictionary
  if (locale == "us"){
    dict <- paste(dictionary$intersect.input, collapse = "|")
  }

  # split
  if (locale == "us"){

    .data %>%
      dplyr::mutate(pm.address = stringr::str_split(string = pm.address, pattern = stringr::str_c("\\b(", dict, ")\\b"))) %>%
      tidyr::unnest() %>%
      mutate(pm.address = stringr::str_trim(pm.address)) -> .data

  }

  # return output
  return(.data)

}

#' Covert Intersections to Wide Form
#'
#' @description Convert a parsed intersection object into wide form.
#'
#' @usage pm_intersect_wider(.data, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep} that has also been
#'    modified with \link{pm_intersect_longer} and parsed with the necessary functions.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @export
pm_intersect_wider <- function(.data, locale = "us"){

  # global bindings
  pm.uid = data = rowid = y = pm.city1 = pm.state1 = pm.zip1 = pm.zip41 = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # convert from long to wide
  .data %>%
    dplyr::group_by(pm.uid) %>%

    # create list-col
    tidyr::nest() %>%

    # add rowid
    tibble::rowid_to_column() %>%

    # create copy of list-col
    dplyr::mutate(y = data) %>%

    # select first row
    dplyr::mutate(data = purrr::map2(data, rowid, ~ pm_x_street(x = .x, id = .y))) %>%

    # select second row
    dplyr::mutate(y = purrr::map2(y, rowid, ~ pm_y_street(x = .x, id = .y))) %>%

    # remove list-cols
    tidyr::unnest() %>%
    dplyr::select(-rowid) -> .data

  # rename pm.city if found
  if ("pm.city1" %in% names(.data) == TRUE){
    .data <- dplyr::rename(.data, pm.city = pm.city1)
  }

  # rename pm.state if found
  if ("pm.state1" %in% names(.data) == TRUE){
    .data <- dplyr::rename(.data, pm.state = pm.state1)
  }

  # rename pm.zip if found
  if ("pm.zip1" %in% names(.data) == TRUE){
    .data <- dplyr::rename(.data, pm.zip = pm.zip1)
  }

  # rename pm.zip4 if found
  if ("pm.zip41" %in% names(.data) == TRUE){
    .data <- dplyr::rename(.data, pm.zip4 = pm.zip41)
  }

  # return output
  return(.data)

}

pm_x_street <- function(x, id){

  # global bindings
  pm.city = pm.state = pm.zip = pm.zip4 = NULL

  # subset to first row only
  y <- dplyr::slice(x, 1L)

  # remove pm.city if found
  if ("pm.city" %in% names(y) == TRUE){
    y <- dplyr::select(y, -pm.city)
  }

  # remove pm.state if found
  if ("pm.state" %in% names(y) == TRUE){
    y <- dplyr::select(y, -pm.state)
  }

  # remove pm.zip if found
  if ("pm.zip" %in% names(y) == TRUE){
    y <- dplyr::select(y, -pm.zip)
  }

  # remove pm.zip4 if found
  if ("pm.zip4" %in% names(x) == TRUE){
    y <- dplyr::select(y, -pm.zip4)
  }

  # return output
  return(y)

}

pm_y_street <- function(x, id){

  # global bindings
  pm.preDir = pm.street = pm.streetSuf = pm.sufDir = NULL

  # subset to second row only
  x <- dplyr::slice(x, 2L)

  # rename pm.preDir if found
  if ("pm.preDir" %in% names(x) == TRUE){
    x <- dplyr::rename(x, pm.preDir.y = pm.preDir)
  }

  # rename pm.street if found
  if ("pm.street" %in% names(x) == TRUE){
    x <- dplyr::rename(x, pm.street.y = pm.street)
  }

  # rename pm.streetSuf if found
  if ("pm.streetSuf" %in% names(x) == TRUE){
    x <- dplyr::rename(x, pm.streetSuf.y = pm.streetSuf)
  }

  # rename pm.sufDir if found
  if ("pm.sufDir" %in% names(x) == TRUE){
    x <- dplyr::rename(x, pm.sufDir.y = pm.sufDir)
  }

  # return output
  return(x)

}
