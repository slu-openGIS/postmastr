#' Do Any Addresses Have House Ranges
#'
#' @description Determine whether the house range test returns any matches.
#'
#' @details A house range is used in some parts of the United States.
#'    House ranges typically look like \code{121-123 Main St}.
#'
#' @usage pm_houseRange_any(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     house range and \code{FALSE} if they do not.
#'
#' @export
pm_houseRange_any <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test and create output
  .data <- pm_houseRange_detect(.data)
  out <- any(.data$pm.hasHouseRange)

  # return output
  return(out)

}

#' Do All Addresses Have House Ranges
#'
#' @description Determine whether all addresses have a house range.
#'
#' @details A house range is used in some parts of the United States.
#'    House ranges typically look like \code{121-123 Main St}.
#'
#' @usage pm_houseRange_all(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain
#'     a house range and \code{FALSE} otherwise.
#'
#' @export
pm_houseRange_all <- function(.data){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # test and create output
  .data <- pm_houseRange_detect(.data)
  out <- all(.data$pm.hasHouseRange)

  # return output
  return(out)

}

#' Detect Presence of House Range
#'
#' @description Determine the presence of a houge range in a string.
#'
#' @details A house range is used in some parts of the United States.
#'    House ranges typically look like \code{121-123 Main St}.
#'
#' @usage pm_houseRange_detect(.data)
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
pm_houseRange_detect <- function(.data){

  # global bindings
  pm.house = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # detect pattern
  .data <- dplyr::mutate(.data, pm.hasHouseRange = stringr::str_detect(pm.house, pattern = "-"))

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_houseRange_detect
#'
#' @description Automatically subset the results of \link{pm_houseRange_detect} to
#'    return only observations that were not found to include a house range.
#'
#' @details A house range is used in some parts of the United States.
#'    House ranges typically look like \code{121-123 Main St}.
#'
#' @usage pm_houseRange_none(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble containing only observations that were not matched
#'     using the house range test. The variable created by
#'     \link{pm_houseRange_detect}, \code{pm.hasHouseRange}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_houseRange_none <- function(.data){

  # global bindings
  pm.hasHouseRange = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # create output
  .data %>%
    pm_houseRange_detect() %>%
    dplyr::filter(pm.hasHouseRange == FALSE) %>%
    dplyr::select(-pm.hasHouseRange) -> out

  # return output
  return(out)

}

#' Return Only Matched Observations From pm_houseRange_detect
#'
#' @description Automatically subset the results of \link{pm_houseRange_detect} to
#'    return only observations that were found to include a house range. This functionality
#'    is only available for house range matching right now because ranges can contain
#'    complex alphanumeric values that need manual cleaning.
#'
#' @details A house range is used in some parts of the United States.
#'    House ranges typically look like \code{121-123 Main St}.
#'
#' @usage pm_houseRange_match(.data)
#'
#' @param .data A postmastr object created with \link{pm_prep}
#'
#' @return A tibble containing only observations that were matched
#'     using the house range test. The variable created by
#'     \link{pm_houseRange_detect}, \code{pm.hasHouseRange}, is removed.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_houseRange_match <- function(.data){

  # global bindings
  pm.hasHouseRange = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # create output
  .data %>%
    pm_houseRange_detect() %>%
    dplyr::filter(pm.hasHouseRange == TRUE) %>%
    dplyr::select(-pm.hasHouseRange) -> out

  # return output
  return(out)

}

#' Parse Fractional House Numbers
#'
#' @description Create a new column containing house range values as a list-column.
#'
#' @usage pm_houseRange_parse(.data, expand_range = TRUE, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param expand_range A logical scalar; if \code{TRUE} (default), house numbers that
#'    contain a numerical range (i.e. \code{11-15 Main St}) will be expanded to specify
#'    all integer values within the range. Ranges that contain an alphanumeric value
#'    cannot be expanded and will be skipped.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new column \code{pm.house} that contains the house range
#'     store in a list-column. The list-column will contain the low and
#'     high values for ranges, and can optionally be expanded to include all integer
#'     values within a range if \code{expand_range} is equal to \code{TRUE}.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr everything
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr word
#'
#' @export
pm_houseRange_parse <- function(.data, expand_range = TRUE, locale = "us"){

  # global bindings
  . = pm.address = pm.uid = pm.hasHouseRange = pm.house = pm.houseRange = pm.houseLow =
    pm.houseHigh = pm.houseShort = pm.house2 = pm.hasAlpha.a = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # only parse house ranges if there are house ranges present in the data
  if (pm_houseRange_any(.data) == TRUE){

    # detect individual addresses with ranges
    if ("pm.hasHouseRange" %in% names(.data) == FALSE){
      rangeDetect <- FALSE
      .data <- pm_houseRange_detect(.data)
    } else if ("pm.hasHouseRange" %in% names(.data) == TRUE){
      rangeDetect <- TRUE
    }

    # parse into two columns
    .data %>%
      dplyr::mutate(pm.houseVal = ifelse(pm.hasHouseRange == TRUE, pm.house, NA)) %>%
      dplyr::mutate(pm.houseVal = stringr::str_replace(pm.houseVal, pattern = "-", replacement = " ")) %>%
      dplyr::mutate(pm.houseLow = stringr::word(pm.houseVal, 1)) %>%
      dplyr::mutate(pm.houseHigh = stringr::word(pm.houseVal, 2)) -> .data

    # look for shortened house numbers
    .data %>%
      dplyr::mutate(pm.houseShort = ifelse(stringr::str_length(pm.houseLow) > stringr::str_length(pm.houseHigh), TRUE, FALSE)) %>%
      dplyr::mutate(pm.houseHigh = ifelse(pm.houseShort == TRUE,
                                          stringr::str_c(stringr::str_sub(pm.houseLow,
                                                                          start = 1,
                                                                          end = stringr::str_length(pm.houseLow)-
                                                                            stringr::str_length(pm.houseHigh)),
                                                         pm.houseHigh),
                                          pm.houseHigh)) %>%
      dplyr::mutate(pm.house2 = ifelse(pm.houseShort == TRUE, stringr::str_c(pm.houseLow, "-", pm.houseHigh), pm.house)) %>%
      dplyr::mutate(pm.house = ifelse(is.na(pm.house2) == FALSE, pm.house2, pm.house)) %>%
      dplyr::select(-pm.house2, -pm.houseShort, -pm.houseVal) -> .data

    # remove pm.houseDetect if not present initially
    if (rangeDetect == FALSE){
      .data <- dplyr::select(.data, -pm.hasHouseRange)
    }

    # construct list-col
    # if there is no range, a list of <chr [1]> with a value of NA is created, this is needed
    # so that tidyr::unnest() works down the road
    .data %>%
      dplyr::mutate(
        pm.houseRange = stringr::str_split(string = stringr::str_c(
          as.character(pm.houseLow), "-", as.character(pm.houseHigh)), pattern = "-")
      ) -> .data

    # expand numeric ranges
    if (expand_range == TRUE){

      # subset data without a range
      .data %>%
        dplyr::filter(is.na(pm.houseLow) == TRUE) %>%
        dplyr::select(-pm.houseLow, -pm.houseHigh) -> noRange

      # subset data with a range, identify ranges with alphanumeric values
      .data %>%
        dplyr::filter(is.na(pm.houseLow) == FALSE) %>%
        dplyr::select(-pm.houseLow, -pm.houseHigh) %>%
        pm_houseAlpha_detect() -> yesRange

      # subset ranges without alphanumeric values, expand
      yesRange %>%
        dplyr::filter(pm.hasAlpha.a == FALSE) %>%
        dplyr::select(-pm.hasAlpha.a) %>%
        dplyr::mutate(pm.houseRange = purrr::map(.x = pm.houseRange, .f = pm_parse_range)) -> yesRange_num

      # put data pack together
      yesRange %>%
        dplyr::filter(pm.hasAlpha.a == TRUE) %>%
        dplyr::select(-pm.hasAlpha.a) %>%
        dplyr::bind_rows(yesRange_num, ., noRange) %>%
        dplyr::arrange(pm.uid) -> .data

    } else if (expand_range == FALSE){

      .data <- dplyr::select(.data, -pm.houseLow, -pm.houseHigh)

    }

    # reorder variables
    if (locale == "us"){
      vars <- pm_reorder(.data)
      .data <- dplyr::select(.data, vars)
    }

  }

  # return output
  return(.data)

}

# Parse and Expand House Range
pm_parse_range <- function(x){

  # convert item to numeric
  vector <- as.numeric(x)

  # expand vector to include every other integer between low and high values
  out <- seq.int(from = vector[1], to = vector[2], by = 2)

  # convert to string
  out <- as.character(out)

  # return output
  return(out)

}

