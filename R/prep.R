#' Identify Observations
#'
#' @description Adds two identification numbers to raw data that will be used
#'     for matching parsed data with the original, raw data once parsing
#'     is complete. One (\code{pm.id}) uniquely identifies each observation,
#'     while the other uniquely identifies each distinct street address
#'     (\code{pm.uid}). A variable named \code{pm.type} is also created that
#'     attempts to identify the type of variable of address contained in a
#'     given variable. If \code{pm_identify} is re-run, only \code{pm.type}
#'     will be updated.
#'
#' @details \code{postmastr} functions are designed to operate
#'     on unique street addresses rather than on an entire data set to increase
#'     speed and performance. The \code{pm.uid} number helps facilitate the
#'     matching process between processed and original data while the observation
#'     identification number \code{pm.id} preserves the original sort order of
#'     the data. These variable names should not be changed - subsequent functions
#'     applied to the prepared data check for their presence before executing
#'     to ensure that they remain, and will error if they are not found.
#'
#' @usage pm_identify(.data, var, intersect_dict, locale = "us")
#'
#' @param .data A tbl or data frame
#' @param var A character variable containing address data to be parsed
#' @param intersect_dict A dictionary object with intersection identifiers
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble with the \code{pm.id}, \code{pm.uid}, and \code{pm.type} variables added
#'    in the first three positions of the data set.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom tibble rowid_to_column
#'
#' @export
pm_identify <- function(.data, var, intersect_dict, locale = "us"){

  # create bindings for global variables
  . = pm.id = pm.uid = pm.type = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # error if var does not exist
  if (varQN %in% names(.data) == FALSE){
    stop("The variable with address data given for 'var' argument does not exist in the given object.")
  }

  # error if variables are already present
  if ("pm.id" %in% names(.data) == TRUE){
    reIdentify <- TRUE
  } else if ("pm.id" %in% names(.data) == FALSE){
    reIdentify <- FALSE
  }

  # optional dictionary parameters
  if (missing(intersect_dict) == TRUE){
    intersect_dict <- NULL
  }

  if (reIdentify == FALSE){

    # add id numbers to each row
    full <- tibble::rowid_to_column(.data, var = "pm.id")

    # add unique id numbers for each address string
    .data %>%
      dplyr::distinct(!!varQ) %>%
      tibble::rowid_to_column(var = "pm.uid") -> subset

    # identify address type
    subset <- pm_identify_type(subset, var = !!varQ, dict = intersect_dict, locale = locale)

    # put data back together
    subset %>%
      dplyr::left_join(full, ., by = varQN) %>%
      dplyr::select(pm.id, pm.uid, pm.type, dplyr::everything()) %>%
      dplyr::as_tibble() -> out

  } else if (reIdentify == TRUE){

    # identify address type
    out <- pm_identify_type(.data, var = !!varQ, dict = intersect_dict, locale = locale)

  }

  # return output
  return(out)

}

# Identify Address Type
pm_identify_type <- function(.data, var, dict, locale){

  # global bindings
  pm.type = pm.hasIntersect = pm.hasHouse = pm.hasZip = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  # identify address type
  .data %>%
    pm_intersect_detect(var = !!varQ, dictionary = dict, locale = locale) %>%
    pm_house_detect_prep(var = !!varQ) %>%
    pm_postal_detect_prep(var = !!varQ, locale = locale) %>%
    dplyr::mutate(pm.type = dplyr::case_when(
      pm.hasHouse == TRUE & (pm.hasIntersect == TRUE | pm.hasIntersect == FALSE) & pm.hasZip == TRUE ~ "full",
      pm.hasHouse == TRUE & (pm.hasIntersect == TRUE | pm.hasIntersect == FALSE) & pm.hasZip == FALSE ~ "short",
      pm.hasHouse == FALSE & pm.hasIntersect == TRUE & (pm.hasZip == TRUE | pm.hasZip == FALSE) ~ "intersection",
      pm.hasHouse == FALSE & pm.hasIntersect == FALSE & pm.hasZip == TRUE  ~ "partial",
      pm.hasHouse == FALSE & pm.hasIntersect == FALSE & pm.hasZip == FALSE  ~ "unknown"
    )) %>%
    dplyr::mutate(pm.type = ifelse(is.na(pm.type) == TRUE, "unknown", pm.type)) %>%
    dplyr::select(-pm.hasIntersect, -pm.hasHouse, -pm.hasZip) -> .data

  # return output
  return(.data)

}

#' Evaluate Address Type
#'
#' @description Prints a tibble with the results of \code{\link{pm_identify}}'s assessment
#'     of address types.
#'
#' @usage pm_evaluate(.data)
#'
#' @param .data A source tibble that has already had identification
#'    numbers added using \link{pm_identify}.
#'
#' @return A tibble with the frequency and percent of values in the \code{pm.type} variable.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr group_by
#' @importFrom dplyr n
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#'
#' @export
pm_evaluate <- function(.data){

  # global bindings
  pm.type = count = pct = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Evaluate your data with pm_identify before proceeding.")
  }

  # store total n
  n <- nrow(.data)

  # create summary tibble
  .data %>%
    dplyr::group_by(pm.type) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::mutate(pct = round(count/n*100, digits = 2)) %>%
    dplyr::arrange(dplyr::desc(pct), pm.type) -> out

  # return output
  return(out)

}

#' Return Only Partial Records
#'
#' @description Subsets addresses that are marked as \code{"partial"} in \code{pm.type} so that
#'    they can be individually evaluated and updated prior to standardization.
#'
#' @usage pm_type_partial(.data)
#'
#' @param .data A source tibble that has already had identification
#'    numbers added using \link{pm_identify}.
#'
#' @return A tibble with only observations that are \code{"unknown"} in \code{pm.type}.
#'
#' @export
pm_type_partial <- function(.data){

  # global bindings
  pm.type = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Evaluate your data with pm_identify before proceeding.")
  }

  # subset
  out <- dplyr::filter(.data, pm.type == "partial")

  # return output
  return(out)

}

#' Return Only Unknown Records
#'
#' @description Subsets addresses that are marked as \code{"unknown"} in \code{pm.type} so that
#'    they can be individually evaluated and updated prior to standardization.
#'
#' @usage pm_type_unknown(.data)
#'
#' @param .data A source tibble that has already had identification
#'    numbers added using \link{pm_identify}.
#'
#' @return A tibble with only observations that are \code{"unknown"} in \code{pm.type}.
#'
#' @export
pm_type_unknown <- function(.data){

  # global bindings
  pm.type = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Evaluate your data with pm_identify before proceeding.")
  }

  # subset
  out <- dplyr::filter(.data, pm.type == "unknown")

  # return output
  return(out)

}

#' Create postmastr Object for Parsing
#'
#' @description Once identification numbers have been been added, this
#'    function creates a subset that will be parsed and then re-applied
#'    to the primary data set.
#'
#' @details Creation of the subset data is dependent on whether the data are
#'    house numbers or intersections. Any addresses identified as \code{"short"},
#'    \code{"full"}, or \code{"partial"} will be returned when \code{type = "street"}.
#'    Addresses that are identified as \code{"unknown"} will also be included in \code{"street"},
#'    but the accuracy of the parser may be limited. Addresses identified as intersections
#'    will be returned when \code{type = "intersection"}.
#'
#' @usage pm_prep(.data, var, type)
#'
#' @param .data A source tibble that has already had identification
#'    numbers added using \link{pm_identify}.
#' @param var A character variable containing address data to be parsed
#' @param type The type of addresses to be parsed, one of either \code{"street"}
#'     or \code{"intersection"}
#'
#' @return A tibble with one observation per unique address in the source data
#'    frame. The tibble will have two variables, \code{pm.uid} and \code{pm.address},
#'    which is used as the basis for exploratory parsing.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom stringr str_replace_all
#'
#' @export
pm_prep <- function(.data, var, type){

  # global bindings
  pm.type = NULL

  # create bindings for global variables
  pm.address = pm.uid = geometry = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # ensure sf objects are convert to tibbles
  if ("sf" %in% class(.data)){
    .data <- as.data.frame(.data)
    .data <- dplyr::select(.data, -geometry)
  }

  # error if variables are not already present
  if ("pm.id" %in% names(.data) == FALSE | "pm.uid" %in% names(.data) == FALSE){
    stop("Identification numbers variables 'pm.id' and/or 'pm.uid' do not exist in your data. Please add them with postmastr::pm_identify() before proceeding.")
  }

  # error if var does not exist
  if (varQN %in% names(.data) == FALSE){
    stop("The variable with address data given for 'var' argument does not exist in the given object.")
  }

  # filter by type
  if (type == "street"){
    .data <- dplyr::filter(.data, pm.type != "intersection")
  } else if (type == "intersection"){
    .data <- dplyr::filter(.data, pm.type == "intersection")
  }

  # filter and format
  .data %>%
    dplyr::distinct(pm.uid, .keep_all = TRUE) %>%
    dplyr::mutate(pm.address := !!varQ) %>%
    dplyr::mutate(pm.address = stringr::str_replace_all(pm.address, pattern = ",", replace = "")) %>%
    dplyr::select(pm.uid, pm.address) %>%
    dplyr::as_tibble() -> .data

  # replace forward slashes
  .data %>%
    dplyr::mutate(pm.address = stringr::str_replace_all(
      string = pm.address,
      pattern = "/",
      replacement = " at ")) %>%
    dplyr::mutate(pm.address = stringr::str_squish(pm.address)) -> .data

  # return output
  return(.data)

}

#' Validate postmastr pm.uid Variable
#'
#' @description This function tests to see whether the unique identification
#'     number \code{pm.uid} remains in an object. It is used as part of the
#'     parsing functions, and is exported so that it can be used interactively
#'     as well.
#'
#' @usage pm_has_uid(obj)
#'
#' @param obj Object to test
#'
#' @return A logical scalar that is \code{TRUE} if the variable remains;
#'     it will return \code{FALSE} otherwise.
#'
#' @export
pm_has_uid <- function(obj){

  # test
  result <- "pm.uid" %in% names(obj)

  # return output
  return(result)

}

#' Validate postmastr Address Variable
#'
#' @description This function tests to see whether the address variable
#'     \code{pm.address} remains in an object. It is used as part of the
#'     parsing functions, and is exported so that it can be used interactively a
#'     s well.
#'
#' @usage pm_has_address(obj)
#'
#' @param obj Object to test
#'
#' @return A logical scalar that is \code{TRUE} if the variable remains;
#'     it will return \code{FALSE} otherwise.
#'
#' @export
pm_has_address <- function(obj){

  # test
  result <- "pm.address" %in% names(obj)

  # return output
  return(result)

}
