#' Add Parsed Street Address Data to Source Data Set
#'
#' @description Adds standardized address and, optionally, unnest house ranges. All logical variables
#'    that were created during the data cleaning process (i.e. \code{pm.hasHouse}, \code{pm.hasDir}, etc.)
#'    are removed at this stage.
#'
#' @usage pm_replace(source, street, intersect, side = "right", unnest = FALSE)
#'
#' @param source Original source data to merge clean addresses with.
#' @param street A postmastr object created with \link{pm_prep} with street addresses that has been readied
#'     for replacement by fully parsing the data.
#' @param intersect A postmastr object created with \link{pm_prep} with intersections that has been readied
#'     for replacement by fully parsing the data.
#' @param side One of either \code{"left"} or \code{"right"} - should parsed data be placed to the left
#'    or right of the original data? Placing data to the left may be useful in particularly wide
#'    data sets.
#' @param unnest A logical scalar; if \code{TRUE}, house ranges will be unnested (i.e. a house range that
#'    has been expanded to cover four addresses with \code{\link{pm_houseRange_parse}} will be converted
#'    from a single observation to four observations, one for each house number). If \code{FALSE} (default),
#'    the single observation will remain.
#'
#' @return The source data with a parsed address elements added to the left side of the source data.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr everything
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr select_if
#' @importFrom dplyr starts_with
#' @importFrom tidyr unnest
#'
#' @export
pm_replace <- function(source, street, intersect, side = "right", unnest = FALSE){

  # global bindings
  pm.id = pm.houseRange = pm.houseFrac = pm.house = pm.hasHouseFracRange = NULL

  # check for objects and key variables
  if (missing(street) == FALSE){

    if (pm_has_uid(street) == FALSE){
      stop("The variable 'pm.uid' is missing from the object supplied for the 'street' argument. Create a postmastr object with pm_identify and pm_prep and fully parse it before proceeding.")
    }

    if ("pm.street" %in% names(street) == FALSE){
      stop("The object supplied for the 'street' argument is missing the 'pm.street' variable and therefore is not fully parsed. postmastr objects must be fully parsed before replacement.")
    }

  }

  if (missing(intersect) == FALSE){

    if (pm_has_uid(intersect) == FALSE){
      stop("The variable 'pm.uid' is missing from the object supplied for the 'intersect' argument. Create a postmastr object with pm_identify and pm_prep and fully parse it before proceeding.")
    }

    if ("pm.street" %in% names(intersect) == FALSE){
      stop("The object supplied for the 'intersect' argument is missing the 'pm.street' variable and therefore is not fully parsed. postmastr objects must be fully parsed before replacement.")
    }

  }

  if (pm_has_uid(source) == FALSE){
    stop("The variable 'pm.uid' is missing from the source data. Source data should be processed with pm_identify before parsing.")
  }

  # determine rebuild operations
  if (missing(street) == TRUE & missing(intersect) == TRUE){
    stop("At least one parsed data set must be supplied for 'street' or 'intersect'.")
  } else if (missing(street) == FALSE & missing(intersect) == TRUE){
    build <- "street"
  } else if (missing(street) == TRUE & missing(intersect) == FALSE){
    build <- "intersect"
  } else if (missing(street) == FALSE & missing(intersect) == FALSE){
    build <- "combined"
  }

  # rebuild
  if (build == "street"){


  }

  # return output
  return(out)

}

#
pm_rebuild_street <- function(.data, source, side, unnest){

  # remove logical variables from postmastr object as well as any missing all values
  .data %>%
    dplyr::select(-dplyr::starts_with("pm.has")) %>%
    dplyr::select_if(function(x) !(all(is.na(x)))) -> .data

  # combine data
  out <- dplyr::left_join(source, .data, by = "pm.uid")

  # optionally re-order
  if (side == "left"){

    # store remaining variable names
    parsedNames <- names(.data)

    # re-order variables
    out <- dplyr::select(out, pm.id, parsedNames, dplyr::everything())
  }

  # optionally unnest and clean-up unnested data
  if (unnest == TRUE){

    out %>%
      tidyr::unnest() %>%
      dplyr::mutate(pm.houseFrac =
                      ifelse(is.na(pm.houseRange) == FALSE & is.na(pm.houseFrac) == FALSE,
                             NA, pm.houseFrac)) %>%
      dplyr::mutate(pm.house = ifelse(is.na(pm.houseRange) == FALSE, pm.houseRange, pm.house)) %>%
      dplyr::mutate(pm.hasHouseFracRange = ifelse(stringr::str_detect(
        string = stringr::word(pm.house, -1),
        pattern = "[1-9]/") == TRUE, TRUE, FALSE)) %>%
      dplyr::mutate(pm.houseFrac = ifelse(pm.hasHouseFracRange == TRUE, stringr::word(pm.house, -1), pm.houseFrac)) %>%
      dplyr::mutate(pm.house = ifelse(pm.hasHouseFracRange == TRUE &
                                        stringr::str_count(string = pm.house, pattern = "\\S+") == 2,
                                      stringr::word(pm.house, 1),
                                      pm.house)) %>%
      dplyr::mutate(pm.house = ifelse(pm.hasHouseFracRange == TRUE &
                                        stringr::str_count(string = pm.house, pattern = "\\S+") > 2,
                                      stringr::word(pm.house, start = 1, end = -2),
                                      pm.house)) %>%
      dplyr::select(-pm.houseRange, -pm.hasHouseFracRange) -> out

  }

}

#
pm_rebuild_intersect <- function(.data, source, side){



}

#' Re-construct Street Addressed from Parsed Elements
#'
#' @description Create a single address from parsed components.
#'
#' @usage pm_rebuild(.data, start, end, new_address, include_commas = FALSE,
#'     keep_parsed, side = "right", left_vars, keep_ids = FALSE, locale = "us")
#'
#' @param .data An object with raw and parsed data created by \code{\link{pm_rebuild}}
#' @param start Variable name to begin rebuilding process with, typically the house number
#' @param end Variable name to end rebuilding process with, typically the street suffix or postal code
#' @param new_address Optiona; name of new variable to store rebuilt address in. If not specified,
#'     the re-build addressed will be stored in \code{pm.address}.
#' @param include_commas A logical scalar; if \code{TRUE}, a comma is added both before and after the city
#'     name in rebuild addresses. If \code{FALSE} (default), no punctuation is added.
#' @param keep_parsed Character string; if \code{"yes"}, all parsed elements will be
#'     added to the source data after replacement. If \code{"limited"}, only the \code{pm.city},
#'     \code{pm.state}, and postal code variables will be retained. Otherwise, if \code{"no"},
#'     only the rebuilt address will be added to the source data (default).
#' @param side One of either \code{"left"} or \code{"right"} - should parsed data be placed to the left
#'     or right of the original data? Placing data to the left may be useful in particularly wide
#'     data sets.
#' @param left_vars A character scalar or vector of variables to place on the left-hand side of
#'     the output when \code{side} is equal to \code{"middle"}.
#' @param keep_ids Logical scalar; if \code{TRUE}, the identification numbers
#'     will be kept in the source data after replacement. Otherwise, if \code{FALSE},
#'     they will be removed (default).
#' @param locale A string indicating the country these data represent; the only
#'     current option is "us" but this is included to facilitate future expansion.
#'
#' @export
pm_rebuild <- function(.data, start, end, new_address, include_commas = FALSE,
                       keep_parsed, side = "right", left_vars, keep_ids = FALSE, locale = "us"){

  # global bindings
  pm.id = pm.uid = pm.houseRange = pm.city = ...temp_address = ...pm.id = ...pm.uid = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote new_var
  if (missing(new_address) == FALSE){
    if (!is.character(paramList$new_address)) {
      varQ <- rlang::enquo(new_address)
    } else if (is.character(paramList$new_address)) {
      varQ <- rlang::quo(!! rlang::sym(new_address))
    }
  } else if (missing(new_address) == TRUE){
    varQ <- rlang::quo(!! rlang::sym("pm.address"))
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

  # convert left_vars to expression
  if (missing(left_vars) == FALSE){
    left_varsE <- rlang::enexpr(left_vars)
  }

  # move pm.houseRange
  if (keep_parsed == "yes" & "pm.houseRange" %in% names(.data) == TRUE){
    .data <- dplyr::select(.data, pm.id, pm.uid, pm.houseRange, dplyr::everything())
  } else if ((keep_parsed == "no" | keep_parsed == "limited") & "pm.houseRange" %in% names(.data) == TRUE){
    .data <- dplyr::select(.data, -pm.houseRange)
  }

  # optionally add commas
  if (include_commas == TRUE & "pm.city" %in% names(.data) == TRUE){
    .data <- dplyr::mutate(.data, pm.city = stringr::str_c(", ", pm.city, ","))
  }

  # rebuild
  .data %>%
    tidyr::unite(...temp_address, !!startQ:!!endQ, sep = " ", remove = FALSE) %>%
    dplyr::mutate(...temp_address = stringr::str_replace_all(...temp_address, pattern = "\\bNA\\b", replacement = "")) %>%
    dplyr::mutate(...temp_address = stringr::str_replace_all(...temp_address, pattern = " , ", replacement = ", ")) %>%
    dplyr::mutate(...temp_address = stringr::str_squish(...temp_address)) %>%
    dplyr::select(pm.id, pm.uid, ...temp_address, dplyr::everything()) -> .data

  # rename ids
  .data <- dplyr::rename(.data, ...pm.id = pm.id, ...pm.uid = pm.uid)

  # remove parsed variables
  if (keep_parsed == "no"){

    # re-order data
    if (side == "right"){

      .data %>%
        dplyr::select(-dplyr::starts_with("pm.")) %>%
        dplyr::select(-...temp_address, dplyr::everything()) -> .data

    } else if (side == "left" | side == "middle"){

      .data %>%
        dplyr::select(-dplyr::starts_with("pm.")) %>%
        dplyr::select(...pm.id, ...pm.uid, ...temp_address, dplyr::everything()) -> .data

      if (side == "middle"){
        .data <- dplyr::select(.data, ...pm.id, ...pm.uid, !!left_varsE, dplyr::everything())
      }

    }

  } else if (keep_parsed == "yes" | keep_parsed == "limited"){

    # create vector of current pm variables in data
    .data %>%
      dplyr::select(dplyr::starts_with("pm."), ...temp_address) %>%
      names() -> pmVarsCurrent

    # create vector of original source data variables
    .data %>%
      dplyr::select(-dplyr::starts_with("pm."), -...pm.id, -...pm.uid) %>%
      names() -> sourceVars

    if (keep_parsed == "yes"){

      # master list of variables for pm objects
      master <- data.frame(
        master.vars = c("...temp_address", "pm.house", "pm.houseRage",
                        "pm.houseFrac", "pm.houseSuf",
                        "pm.preDir", "pm.street", "pm.streetSuf", "pm.sufDir",
                        "pm.unitType", "pm.unitNum",  "pm.city",
                        "pm.state", "pm.zip", "pm.zip4"),
        stringsAsFactors = FALSE
      )

      # create data frame of current variables
      working <- data.frame(
        master.vars = c(pmVarsCurrent),
        working.vars = c(pmVarsCurrent),
        stringsAsFactors = FALSE
      )

      # join master and working data
      joined <- dplyr::left_join(master, working, by = "master.vars")

      # create vector of re-ordered variables
      vars <- stats::na.omit(joined$working.vars)

      # re-order data
      if (side == "right"){
        .data <- dplyr::select(.data, -c(vars), dplyr::everything())
      } else if (side == "left" | side == "middle"){

        .data <- dplyr::select(.data, ...pm.id, ...pm.uid, vars, sourceVars)

        if (side == "middle"){
          .data <- dplyr::select(.data, ...pm.id, ...pm.uid, left_vars, dplyr::everything())
        }

      }

    } else if (keep_parsed == "limited"){

      # master list of variables for pm objects
      if (locale == "us"){
        master <- data.frame(
          master.vars = c("...temp_address", "pm.city", "pm.state", "pm.zip", "pm.zip4"),
          stringsAsFactors = FALSE
        )
      }

      # create data frame of current variables
      working <- data.frame(
        master.vars = c(pmVarsCurrent),
        working.vars = c(pmVarsCurrent),
        stringsAsFactors = FALSE
      )

      # join master and working data
      joined <- dplyr::left_join(master, working, by = "master.vars")

      # create vector of re-ordered variables
      vars <- stats::na.omit(joined$working.vars)

      # re-order data
      if (side == "right"){
        .data <- dplyr::select(.data, -c(vars), dplyr::everything())
      } else if (side == "left" | side == "middle"){

        .data <- dplyr::select(.data, ...pm.id, ...pm.uid, vars, sourceVars)

        if (side == "middle"){
          .data <- dplyr::select(.data, ...pm.id, ...pm.uid, left_vars, dplyr::everything())
        }

      }

    }

  }

  # rename ids if kept; drop if discarded
  if (keep_ids == TRUE){
    .data <- dplyr::rename(.data, pm.id = ...pm.id, pm.uid = ...pm.uid)
  } else if (keep_ids == FALSE){
    .data <- dplyr::select(.data, -...pm.id, -...pm.uid)
  }

  # rename ...temp_address
  .data <- dplyr::rename(.data, !!varQ := ...temp_address)

  # return output
  return(.data)

}

