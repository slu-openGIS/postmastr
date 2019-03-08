#' Do Any Addresses Have Alphanumeric House Numbers
#'
#' @description Determine whether the alphanumeric house number test returns any matches.
#'
#' @usage pm_houseAlpha_any(.data, position = "any", locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param position A string indicating where letters are located; one of either
#'    \code{"any"} (anywhere in the house number), \code{"front"} (initial letters
#'    of the house number), \code{"end"} (last letters of the house number),
#'    or \code{"middle"} (in the middle but not at the ends).
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at least
#'     one alphanumeric house number and \code{FALSE} if they do not.
#'
#' @export
pm_houseAlpha_any <- function(.data, position = "any", locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # test and create output
  if (locale == "us"){
    .data <- pm_houseAlpha_detect(.data, position = position, locale = locale)

    # create output
    if (position == "any"){
      out <- all(.data$pm.hasAlpha.a)
    } else if (position == "front"){
      out <- all(.data$pm.hasAlpha.f)
    } else if (position == "end"){
      out <- all(.data$pm.hasAlpha.e)
    } else if (position == "middle"){
      out <- all(.data$pm.hasAlpha.m)
    }
  }

  # return output
  return(out)

}

#' Do All Addresses Have Alphanumeric House Numbers
#'
#' @description Determine whether the alphanumeric house number test returns matches for every
#'     observation.
#'
#' @usage pm_houseAlpha_all(.data, position = "any", locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param position A string indicating where letters are located; one of either
#'    \code{"any"} (anywhere in the house number), \code{"front"} (initial letters
#'    of the house number), \code{"end"} (last letters of the house number),
#'    or \code{"middle"} (in the middle but not at the ends).
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if all observations contain
#'     alphanumeric house number and \code{FALSE} otherwise.
#'
#' @export
pm_houseAlpha_all <- function(.data, position = "any", locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # test and create output
  if (locale == "us"){
    .data <- pm_houseAlpha_detect(.data, position = position, locale = locale)

    # create output
    if (position == "any"){
      out <- all(.data$pm.hasAlpha.a)
    } else if (position == "front"){
      out <- all(.data$pm.hasAlpha.f)
    } else if (position == "end"){
      out <- all(.data$pm.hasAlpha.e)
    } else if (position == "middle"){
      out <- all(.data$pm.hasAlpha.m)
    }

  }

  # return output
  return(out)

}

#' Detect Presence of Alphanumeric House Numbers
#'
#' @description Detect the presence of U.S. alphanumeric house numbers
#'    in a string address (i.e. 123A Main St).
#'
#' @usage pm_houseAlpha_detect(.data, position = "any", locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param position A string indicating where letters are located; one of either
#'    \code{"any"} (anywhere in the house number), \code{"front"} (initial letters
#'    of the house number), \code{"end"} (last letters of the house number),
#'    or \code{"middle"} (in the middle but not at the ends).
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable beginning with \code{pm.hasAlpha.}
#'     that is \code{TRUE} if letters are detected in the specified position and
#'     \code{FALSE} otherwise. If \code{pm.hasAlpha.} has a \code{a} suffix, it
#'     corresponds to the \code{"any"} position. If \code{pm.hasAlpha.} has a \code{f}
#'     suffix, it corresponds to the \code{"front"} position. If \code{pm.hasAlpha.}
#'     has a \code{e} suffix, it corresponds to the \code{"end"} position. Finally,
#'     If \code{pm.hasAlpha.} has a \code{m} suffix, it corresponds to the \code{"middle"}
#'     position. If a \code{NA} value is returned, it is because the house number is an
#'     address range.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#' @importFrom stringr str_detect
#'
#' @export
pm_houseAlpha_detect <- function(.data, position = "any", locale = "us"){

  # global bindings
  ...test = pm.house = NULL

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

  # create output variable
  if (position == "any"){
    varQ <- rlang::quo(!! rlang::sym("pm.hasAlpha.a"))
  } else if (position == "front"){
    varQ <- rlang::quo(!! rlang::sym("pm.hasAlpha.f"))
  } else if (position == "end"){
    varQ <- rlang::quo(!! rlang::sym("pm.hasAlpha.e"))
  } else if (position == "middle"){
    varQ <- rlang::quo(!! rlang::sym("pm.hasAlpha.m"))
  }

  # detect pattern
  if (locale == "us"){

    # detect pattern
    if (position == "any"){
      .data <- dplyr::mutate(.data, !!varQ := stringr::str_detect(pm.house, "[A-z]"))
    } else if (position == "front"){
      .data <- dplyr::mutate(.data, !!varQ := stringr::str_detect(pm.house, "^[A-z]+"))
    } else if (position == "end"){
      .data <- dplyr::mutate(.data, !!varQ := stringr::str_detect(pm.house, "[A-z]+$"))
    } else if (position == "middle"){

      .data %>%
        dplyr::mutate(...test = stringr::str_replace(string = pm.house, pattern = "^[A-z]+", replacement = "")) %>%
        dplyr::mutate(...test = stringr::str_replace(string = ...test, pattern = "[A-z]+$", replacement = "")) %>%
        dplyr::mutate(!!varQ := stringr::str_detect(...test, "[A-z]")) %>%
        dplyr::select(-...test) -> .data
    }

    # identify address ranges
    # .data %>%
    #  dplyr::mutate(...range = stringr::str_detect(pm.house, pattern = "-")) %>%
    #  dplyr::mutate(!!varQ := ifelse(...range == TRUE, NA, !!varQ)) %>%
    #  dplyr::select(-...range) -> .data

  }

  # return output
  return(.data)

}


#' Does House Suffix Dictionary Return Any Matches
#'
#' @description Determine whether the street suffix dictionary returns any matches.
#'
#' @usage pm_houseSuf_any(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_append} to be used
#'     as a master list for house suffixes
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains at
#'     least one house suffix in the given dictionary and \code{FALSE} if they do not.
#'
#' @export
pm_houseSuf_any <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # missing dictionary
  if (missing(dictionary) == TRUE){
    stop("A house suffix dictionary created with pm_append is required.")
  }

  # test dictionary
  .data <- pm_houseSuf_detect(.data, dictionary = dictionary, locale = locale)

  # create output
  out <- any(.data$pm.hasHouseSuf)

  # return output
  return(out)

}

#' Does House Suffix Dictionary Return a Match for All Observations
#'
#' @description Determine whether the street suffix dictionary returns matches for all observations.
#'
#' @usage pm_houseSuf_all(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_append} to be used
#'     as a master list for house suffixes.
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A logical scalar is returned that is \code{TRUE} if the data contains a house suffix
#'     for every observation in the data set and \code{FALSE} otherwise.
#'
#' @export
pm_houseSuf_all <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # missing dictionary
  if (missing(dictionary) == TRUE){
    stop("A house suffix dictionary created with pm_append is required.")
  }

  # test dictionary
  .data <- pm_houseSuf_detect(.data, dictionary = dictionary, locale = locale)

  # create output
  out <- all(.data$pm.hasHouseSuf)

  # return output
  return(out)

}

#' Detect Presence of House Suffix
#'
#' @description Determine the presence of street suffix names or abbreviations
#'     at the end of a string.
#'
#' @usage pm_houseSuf_detect(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_append} to be used
#'     as a master list for house suffixes
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble with a new logical variable \code{pm.hasHouseSuf} that is
#'     \code{TRUE} if a house suffix from the given dictionary is found at the beginning
#'     of the address and \code{FALSE} otherwise.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom stringr str_c
#' @importFrom stringr str_detect
#'
#' @export
pm_houseSuf_detect <- function(.data, dictionary, locale = "us"){

  # create bindings for global variables
  pm.address = pm.hasState = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # missing dictionary
  if (missing(dictionary) == TRUE){
    stop("A house suffix dictionary created with pm_append is required.")
  }

  # minimize dictionary
  if (locale == "us"){
    dict <- paste(dictionary$houseSuf.input, collapse = "|")
  }

  # check observations
  if (locale == "us"){
    .data <- dplyr::mutate(.data, pm.hasHouseSuf = stringr::str_detect(pm.address,
                                                                        pattern = stringr::str_c("^\\b(", dict, ")\\b")))
  }

  # return output
  return(.data)

}

#' Return Only Unmatched Observations From pm_houseSuf_detect
#'
#' @description Automatically subset the results of \link{pm_houseSuf_detect} to
#'    return only observations that were not found in the dictionary.
#'
#' @usage pm_houseSuf_none(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_append} to be used
#'     as a master list for house suffixes
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr select
#'
#' @export
pm_houseSuf_none <- function(.data, dictionary, locale = "us"){

  # global bindings
  pm.hasHouseSuf = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # missing dictionary
  if (missing(dictionary) == TRUE){
    stop("A house suffix dictionary created with pm_append is required.")
  }

  # create output
  .data %>%
    pm_houseSuf_detect(dictionary = dictionary, locale = locale) %>%
    dplyr::filter(pm.hasHouseSuf == FALSE) %>%
    dplyr::select(-pm.hasHouseSuf) -> out

  # return output
  return(out)

}

#' Parse House Suffix
#'
#' @description Parse a house suffix from a string. These data
#'     should be at the beginning of the string (i.e. the first word).
#'
#' @usage pm_houseSuf_parse(.data, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param dictionary A tbl created with \code{pm_append} to be used
#'     as a master list for house suffixes
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom stringr str_c
#' @importFrom stringr str_count
#' @importFrom stringr str_replace
#' @importFrom stringr word
#'
#' @export
pm_houseSuf_parse <- function(.data, dictionary, locale = "us"){

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("Error 2.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("Error 3.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # missing dictionary
  if (missing(dictionary) == TRUE){
    stop("A house suffix dictionary created with pm_append is required.")
  }

  # parse house suffix, but skip if NULL
  if (is.null(dictionary) == FALSE){

    if (locale == "us"){
      .data <- pm_parse_houseSuf_us(.data, dictionary = dictionary)
      vars <- pm_reorder(.data)
      .data <- dplyr::select(.data, vars)
    }

  }

  # return output
  return(.data)

}

# parse American house suffix values
pm_parse_houseSuf_us <- function(.data, dictionary, locale = "us"){

  # create bindings for global variables
  pm.address = pm.houseSuf = NULL

  # minimize dictionary
  dict <- paste(dictionary$houseSuf.input, collapse = "|")

  # parse
  .data <- dplyr::mutate(.data, pm.houseSuf =
                           stringr::str_extract(pm.address,
                                                pattern = stringr::str_c("^\\b(", dict, ")\\b")))

  # clean address data
  .data %>%
    dplyr::mutate(pm.address = ifelse(is.na(pm.houseSuf) == FALSE,
                                      stringr::word(pm.address, start = 1+stringr::str_count(pm.houseSuf, pattern = "\\w+"),
                                                    end = -1), pm.address)) %>%
    pm_houseSuf_std(var = pm.houseSuf, dictionary = dictionary) -> .data

}

#' Standardize Parsed House Suffix Names
#'
#' @description Convert house suffix values to desired outputs
#'
#' @usage pm_houseSuf_std(.data, var, dictionary, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable that may contain street suffix values
#' @param dictionary A tbl created with \code{pm_append} to be used
#'     as a master list for house suffixes
#' @param locale A string indicating the country these data represent; the only
#'    current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A tibble with an updated variable that contains standardized house suffix values.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
#' @export
pm_houseSuf_std <- function(.data, var, dictionary, locale = "us"){

  # global bindings
  . = houseSuf.input = houseSuf.output = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # missing dictionary
  if (missing(dictionary) == TRUE){
    stop("A house suffix dictionary created with pm_append is required.")
  }

  # standardize state names
  dictionary %>%
    dplyr::rename(!!varQN := houseSuf.input) -> dictionary

  # standardize
  .data %>%
    dplyr::left_join(., dictionary, by = varQN) %>%
    dplyr::mutate(!!varQ := ifelse(is.na(houseSuf.output) == FALSE, houseSuf.output, !!varQ)) %>%
    dplyr::select(-houseSuf.output) -> out

  # return output
  return(out)

}
