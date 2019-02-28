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


