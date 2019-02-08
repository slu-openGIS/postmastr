#' Identify Observations
#'
#' @description Adds two identification numbers to raw data that will be used
#'     for matching parsed data with the original, raw data once parsing
#'     is complete. One (\code{pm.id}) uniquely identifies each observation,
#'     while the other uniquely identifies each distinct street address
#'     (\code{pm.uid}).
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
#' @param .data A tbl or data frame
#' @param var A character variable containing address data to be parsed
#'
#' @return A tibble with both the \code{pm.id} and \code{pm.uid} variables added
#'    in the first two positions of the data set.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom tibble rowid_to_column
#'
#' @export
pm_identify <- function(.data, var){

  # create bindings for global variables
  . = pm.id = pm.uid = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # error if variables are already present
  if ("pm.id" %in% names(.data) == TRUE | "pm.uid" %in% names(.data) == TRUE){
    stop("Identification numbers using the reserved variable names 'pm.id' and/or 'pm.uid'
         already exist in your data frame. Please remove them using dplyr::select() before
         proceeding.")
  }

  # error if var does not exist
  if (varQN %in% names(.data) == FALSE){
    stop("The variable with address data given for 'var' argument does not exist in the given object.")
  }

  # add id numbers to each row
  full <- tibble::rowid_to_column(.data, var = "pm.id")

  # add unique id numbers for each address string
  .data %>%
    dplyr::distinct(!!varQ) %>%
    tibble::rowid_to_column(var = "pm.uid") %>%
    dplyr::left_join(full, ., by = varQN) %>%
    dplyr::select(pm.id, pm.uid, dplyr::everything()) %>%
    dplyr::as_tibble() -> out

  # return output
  return(out)

}

#' Create postmastr Object for Parsing
#'
#' @description Once identification numbers have been been added, this
#'    function creates a subset that will be parsed and then re-applied
#'    to the primary data set.
#'
#' @usage pm_prep(.data, var)
#'
#' @param .data A tibble that has already had identification numbers added
#'    using \link{pm_identify}.
#' @param var A character variable containing address data to be parsed
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang quo_name
#' @importFrom rlang sym
#' @importFrom stringr str_replace
#'
pm_prep <- function(.data, var){

  # create bindings for global variables
  pm.address = pm.uid = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # error if variables are not already present
  if ("pm.id" %in% names(.data) == FALSE | "pm.uid" %in% names(.data) == FALSE){
    stop("Identification numbers variables 'pm.id' and/or 'pm.uid' do not exist in your data.
         Please add them with postmastr::pm_identify() before proceeding.")
  }

  # error if var does not exist
  if (varQN %in% names(.data) == FALSE){
    stop("The variable with address data given for 'var' argument does not exist in the given object.")
  }

  # filter and format
  .data %>%
    dplyr::distinct(pm.uid, .keep_all = TRUE) %>%
    dplyr::mutate(pm.address := !!varQ) %>%
    dplyr::mutate(pm.address = stringr::str_replace(pm.address, ",", "")) %>%
    dplyr::select(pm.uid, pm.address) -> out

  # return output
  return(out)

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
#' @usage pm_has_uid(obj)
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
