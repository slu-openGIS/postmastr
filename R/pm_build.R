#' Re-build Parsed Street Addresses
#'
#' This is a work in progress!
#'
pm_build <- function(.data, houseNum, ...){

  ldots <- rlang::chr(...)

  if (rlang::is_empty(ldots) == FALSE){

    vars <- as.formula(ldots)

  } else if (rlang::is_empty(ldots) == TRUE){

    # create address string
    if (houseNum == TRUE) {

      add <- dplyr::mutate(.data, stFull = paste(houseNum, houseSuf, stPreDir, stName, stType, stSufDir, sep = " "))

    } else if (houseNum == FALSE) {

      add <- dplyr::mutate(.data, stFull = paste(houseSuf, stPreDir, stName, stType, stSufDir, sep = " "))

    }

    # remove NA values
    add %>%
      dplyr::mutate(stFull = tm::removeWords(stFull, "NA")) %>%
      dplyr::mutate(stFull = stringr::str_squish(stFull)) -> out

  }

  return(out)

}
