#' Rebuild Street Address
#'
#' @description Create a single address from parsed components. Data are automatically re-ordered
#'     as part of this process, so there is no need to subset columns prior to executing
#'     this function.
#'
#' @usage pm_rebuild(.data, start, end, add_commas = FALSE, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param start Variable name to begin rebuilding process with, typically the house number
#' @param end Variable name to end rebuilding process with, typically the street suffix or postal code
#' @param add_commas A logical scalar; if \code{TRUE}, a comma is added both before and after the city
#'    name in rebuild addresses. If \code{FALSE} (default), no punctuation is added.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A copy of the pomstmastr object with a standardized address variable.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_squish
#' @importFrom tidyr unite
#'
#' @export
pm_rebuild <- function(.data, start, end, add_commas = FALSE, locale = "us"){

  # global bindings
  pm.rebuilt = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # unquote start
  if (!is.character(paramList$start)) {
    startQ <- rlang::enquo(start)
  } else if (is.character(paramList$start)) {
    startQ <- rlang::quo(!! rlang::sym(start))
  }

  # unquote end
  if (!is.character(paramList$end)) {
    endQ <- rlang::enquo(end)
  } else if (is.character(paramList$end)) {
    endQ <- rlang::quo(!! rlang::sym(end))
  }

  # test end
  endQN <- rlang::quo_name(rlang::enquo(end))

  if (endQN == "end"){
    if ("pm.zip4" %in% names(.data) == TRUE){
      endQ <- rlang::quo(!! rlang::sym("pm.zip4"))
    } else if ("pm.zip4" %in% names(.data) == FALSE){
      endQ <- rlang::quo(!! rlang::sym("pm.zip"))
    }
  }

  # add comma vars optionally
  if (add_commas == TRUE){

    .data <- mutate(.data,
                    pm.preComma = ",",
                    pm.postComma = ",")

  }

  # re-order variables
  vars <- pm_reorder_build(.data)
  .data <- dplyr::select(.data, vars)

  # rebuild
  .data %>%
    tidyr::unite(pm.rebuilt, !!startQ:!!endQ, sep = " ", remove = FALSE) %>%
    dplyr::mutate(pm.rebuilt = stringr::str_replace_all(pm.rebuilt, pattern = "\\bNA\\b", replacement = "")) %>%
    dplyr::mutate(pm.rebuilt = stringr::str_replace_all(pm.rebuilt, pattern = " , ", replacement = ", ")) %>%
    dplyr::mutate(pm.rebuilt = stringr::str_squish(pm.rebuilt)) -> .data

  # re-order variables again
  vars <- pm_reorder(.data)
  .data <- dplyr::select(.data, vars)

  # return output
  return(.data)

}

# Re-order Columns Prior to Build
pm_reorder_build <- function(.data, locale = "us"){

  if (locale == "us"){

    # master list of variables for pm objects
    master <- data.frame(
      master.vars = c("pm.uid", "pm.address", "pm.rebuilt", "pm.house",
                      "pm.houseFrac",
                      "pm.preDir", "pm.street", "pm.streetSuf", "pm.sufDir",
                      "pm.unitType", "pm.unitNum", "pm.preComma", "pm.city", "pm.postComma",
                      "pm.state", "pm.zip", "pm.zip4", "pm.houseLow", "pm.houseHigh",
                      "pm.hasHouse", "pm.hasHouseFrac", "pm.hasDir", "pm.hasStreetSuf",
                      "pm.hasUnit", "pm.hasCity", "pm.hasState", "pm.hasZip"),
      stringsAsFactors = FALSE
    )

    # create data frame of current variables
    working <- data.frame(
      master.vars = names(.data),
      working.vars = names(.data),
      stringsAsFactors = FALSE
    )

    # join master and working data
    joined <- dplyr::left_join(master, working, by = "master.vars")

    # create vector of re-ordered variables
    out <- na.omit(joined$working.vars)

  }

  # return output
  return(out)

}

#' Add Address to Source Data
#'
#' @description Adds standardized address and, optionally, parsed elements back into
#'     original source data frame.
#'
#' @usage pm_replace(.data, source, newVar, keep_parsed, keep_ids = FALSE)
#'
#' @param .data A postmastr object created with \link{pm_prep} that has been readied
#'     for replacement by (a) fully parsing the data and (b) rebuilding a sinle
#'     address field.
#' @param source Original source data to merge clean addresses with.
#' @param newVar Name of new variable to store rebuilt address in.
#' @param keep_parsed Character scalar; if \code{"yes"}, all parsed elements will be
#'     added to the source data after replacement. If \code{"limited"}, only the \code{pm.city},
#'     \code{pm.state}, and postal code variables will be retained. Otherwise, if \code{"no"},
#'     only the rebuilt address will be added to the source data (default).
#' @param keep_ids Logical scalar; if \code{TRUE}, the identification numbers
#'     will be kept in the source data after replacement. Otherwise, if \code{FALSE},
#'     they will be removed (default).
#'
#' @return The source data with a standardized address variable and, optionally, parsed
#'     address elements.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom stats na.omit
#'
#' @export
pm_replace <- function(.data, source, newVar, keep_parsed, keep_ids = FALSE){

  # global bindings
  pm.id = pm.uid = pm.rebuilt = pm.city = pm.state = pm.zip = pm.zip4 = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$newVar)) {
    varQ <- rlang::enquo(newVar)
  } else if (is.character(paramList$newVar)) {
    varQ <- rlang::quo(!! rlang::sym(newVar))
  }

  # optionally retain parsed elements
  if (keep_parsed == "no"){

    .data <- dplyr::select(.data, pm.uid, pm.rebuilt)

  } else if (keep_parsed == "limited") {

    if ("pm.zip4" %in% names(.data) == TRUE){
      .data <- dplyr::select(.data, pm.uid, pm.rebuilt, pm.city, pm.state, pm.zip, pm.zip4)
    } else if ("pm.zip4" %in% names(.data) == FALSE){
      .data <- dplyr::select(.data, pm.uid, pm.rebuilt, pm.city, pm.state, pm.zip)
    }

  } else if (keep_parsed == "yes"){

    vars <- pm_reorder_replace(.data)
    .data %>%
      dplyr::select(vars) %>%
      dplyr::select_if(function(x) !(all(is.na(x)))) -> .data

  }

  # optionally rename output variable
  if (missing(newVar) == FALSE){

    .data <- rename(.data, !!varQ := pm.rebuilt)

  }

  # join parsed and source data
  out <- dplyr::left_join(source, .data, by = "pm.uid")

  # optionally retain id variables
  if (keep_ids == FALSE){

    out <- dplyr::select(out, -pm.id, -pm.uid)

  }

  # return output
  return(out)

}

# Re-order Columns Prior to Build
pm_reorder_replace <- function(.data, locale = "us"){

  if (locale == "us"){

    # master list of variables for pm objects
    master <- data.frame(
      master.vars = c("pm.uid", "pm.rebuilt", "pm.house", "pm.houseLow", "pm.houseHigh",
                      "pm.houseFrac",
                      "pm.preDir", "pm.street", "pm.streetSuf", "pm.sufDir",
                      "pm.unitType", "pm.unitNum",  "pm.city",
                      "pm.state", "pm.zip", "pm.zip4"),
      stringsAsFactors = FALSE
    )

    # create data frame of current variables
    working <- data.frame(
      master.vars = names(.data),
      working.vars = names(.data),
      stringsAsFactors = FALSE
    )

    # join master and working data
    joined <- dplyr::left_join(master, working, by = "master.vars")

    # create vector of re-ordered variables
    out <- stats::na.omit(joined$working.vars)

  }

  # return output
  return(out)

}
