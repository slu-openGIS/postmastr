#' Modify Improperly Parsed Addresses
#'
#' @description Difficult to parse addresses, particularly those that have
#'    alphanumeric house ranges as well as units associated with them,
#'    may be mis-parsed by \code{postmastr}. This function can be used
#'    to manually fix mis-parsed address data.
#'
#' @usage pm_mutate(.data, uid, address, house, house_range, house_frac, house_suf,
#'     pre_dir, street, street_suf, suf_dir, unit_type, unit_num,
#'     city, state, postal, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param uid A \code{pm.uid} value to edit.
#' @param address Optional; the new value for the \code{pm.address} variable.
#' @param house Optional; the new value for the \code{pm.house} variable.
#' @param house_range Optional; the new value (or vector) for the \code{pm.houseRange}
#'     variable.
#' @param house_frac Optional; the new value for the \code{pm.houseFrac} variable.
#' @param house_suf Optional; the new value for the \code{pm.houseSuf} variable.
#' @param pre_dir Optional; the new value for the \code{pm.preDir} variable.
#' @param street Optional; the new value for the \code{pm.street} variable.
#' @param street_suf Optional; the new value for the \code{pm.streetSuf} variable.
#' @param suf_dir Optional; the new value for the \code{pm.sufDir} variable.
#' @param unit_type Optional; the new value for the \code{pm.unitType} variable.
#' @param unit_num Optional; the new value (or vector) for the \code{pm.unitNum}
#'     variable.
#' @param city Optional; the new value for the \code{pm.city} variable.
#' @param state Optional; the new value for the \code{pm.state} variable.
#' @param postal Optional; the new value for the \code{pm.postal} variable.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @export
pm_mutate <- function(.data, uid, address, house, house_range, house_frac, house_suf,
                      pre_dir, street, street_suf, suf_dir, unit_type, unit_num,
                      city, state, postal, locale = "us"){

  # global bindings
  pm.address = pm.city = pm.house = pm.houseFrac = pm.houseRage = pm.houseSuf =
    pm.preDir = pm.state = pm.street = pm.streetSuf = pm.sufDir = pm.uid = pm.unitNum =
    pm.unitType = pm.zip = pm.zip4 = NULL

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  if (any(uid %in% .data$pm.uid) == FALSE){
    stop("The given 'uid' value is not found in your postmastr object.")
  }

  # save parameters to list
  paramList <- as.list(match.call())

  # remove elements from function call
  paramList[-1] %>%
    purrr::list_modify(
      ".data" = NULL,
      "uid" = NULL,
      locale = NULL
    ) -> paramList

  # ensure variables are present
  if (locale == "us"){

    # test variable names
    nameResult <- pm_check_vars_us(.data, params = paramList)

    # return error if result is TRUE
    if (nameResult == TRUE){
      stop('One or more arguments given do not match variables in the given postmastr object.')
    }

  }

  # edit variables
  ## pm.address
  if (missing(address) == FALSE){
    .data <- dplyr::mutate(.data, pm.address = ifelse(pm.uid %in% uid, address, pm.address))
  }

  ## pm.house
  if (missing(house) == FALSE){
    .data <- dplyr::mutate(.data, pm.house = ifelse(pm.uid %in% uid, house, pm.house))
  }

  ## pm.houseRage
  if (missing(house_range) == FALSE){
    .data <- dplyr::mutate(.data, pm.houseRage = ifelse(pm.uid %in% uid, house_range, pm.houseRage))
  }

  ## pm.houseFrac
  if (missing(house_frac) == FALSE){
    .data <- dplyr::mutate(.data, pm.houseFrac = ifelse(pm.uid %in% uid, house_frac, pm.houseFrac))
  }

  ## pm.houseSuf
  if (missing(house_suf) == FALSE){
    .data <- dplyr::mutate(.data, pm.houseSuf = ifelse(pm.uid %in% uid, house_suf, pm.houseSuf))
  }

  ## pm.preDir
  if (missing(pre_dir) == FALSE){
    .data <- dplyr::mutate(.data, pm.preDir = ifelse(pm.uid %in% uid, pre_dir, pm.preDir))
  }

  ## pm.street
  if (missing(street) == FALSE){
    .data <- dplyr::mutate(.data, pm.street = ifelse(pm.uid %in% uid, street, pm.street))
  }

  ## pm.streetSuf
  if (missing(street_suf) == FALSE){
    .data <- dplyr::mutate(.data, pm.streetSuf = ifelse(pm.uid %in% uid, street_suf, pm.streetSuf))
  }

  ## pm.sufDir
  if (missing(suf_dir) == FALSE){
    .data <- dplyr::mutate(.data, pm.sufDir = ifelse(pm.uid %in% uid, suf_dir, pm.sufDir))
  }

  ## pm.unitType
  if (missing(unit_type) == FALSE){
    .data <- dplyr::mutate(.data, pm.unitType = ifelse(pm.uid %in% uid, unit_type, pm.unitType))
  }

  ## pm.unitNum
  if (missing(unit_num) == FALSE){
    .data <- dplyr::mutate(.data, pm.unitNum = ifelse(pm.uid %in% uid, unit_num, pm.unitNum))
  }

  ## pm.city
  if (missing(city) == FALSE){
    .data <- dplyr::mutate(.data, pm.city = ifelse(pm.uid %in% uid, city, pm.city))
  }

  ## pm.state
  if (missing(state) == FALSE){
    .data <- dplyr::mutate(.data, pm.state = ifelse(pm.uid %in% uid, state, pm.state))
  }

  ## postal
  if (missing(postal) == FALSE){

    if (locale == "us"){

      if (stringr::str_detect(string = postal, pattern = "-") == FALSE){

        .data <- dplyr::mutate(.data, pm.zip = ifelse(pm.uid %in% uid, postal, pm.zip))

      } else if (stringr::str_detect(string = postal, pattern = "-") == TRUE){

        # convert string into two words
        postalEdit <- stringr::str_replace(string = postal, pattern = "-", replacement = " ")

        # replace main part of zip-code with first word
        .data <- dplyr::mutate(.data, pm.zip = ifelse(pm.uid %in% uid, stringr::word(postalEdit, 1), pm.zip))

        # replace delivery route with second word
        .data <- dplyr::mutate(.data, pm.zip4 = ifelse(pm.uid %in% uid, stringr::word(postalEdit, 2), pm.zip4))

      }

    }

  }

  # return output
  return(.data)

}


# Check Variables
pm_check_vars_us <- function(.data, params){

  # global bindings
  master.vars = NULL

  # master list of variables for pm objects
  master <- dplyr::tibble(
    master.vars = c("pm.address", "pm.house", "pm.houseRage", "pm.houseFrac", "pm.houseSuf",
                    "pm.preDir", "pm.street", "pm.streetSuf", "pm.sufDir", "pm.unitType", "pm.unitNum",
                    "pm.city", "pm.state", "pm.zip"),
    master.args = c("address", "house", "house_range", "house_frac", "house_suf",
                    "pre_dir", "street", "street_suf", "suf_dir", "unit_type", "unit_num",
                    "city", "state", "postal")
  )

  # create tibble of given names
  args <- dplyr::tibble(
    master.args = names(params),
    given.args = names(params)
  )

  # match given args
  dplyr::left_join(args, master, by = "master.args") %>%
    dplyr::select(master.vars) -> args

  # create tibble of data names
  given_vars <- names(.data)

  given <- dplyr::tibble(
    master.vars = given_vars,
    given.vars = given_vars
  )

  # match given and args
  match <- dplyr::left_join(args, given, by = "master.vars")

  # test
  result <- any(is.na(match$given.vars))

}

#' Modify Raw Addresses
#'
#' @description Clean raw address data before parsing to improve the parsing process. This
#'     may be particularly advantageous for addresses identified as \code{"partial"} or
#'     \code{"unknown"} in \code{pm.type}.
#'
#' @param .data A source tibble that has already had identification
#'    numbers added using \link{pm_identify}.
#' @param uid A \code{pm.uid} value to edit.
#' @param var A character variable containing address data to be edited
#' @param new_val The new value for the for the given address variable.
#'
#' @seealso \code{\link{pm_identify}}
#'
#' @export
pm_mutate_raw <- function(.data, uid, var, new_val){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # edit value
  .data <- dplyr::mutate(.data, !!varQ := ifelse(pm.uid == uid, new_val, !!varQ))

  # return output
  return(.data)

}
