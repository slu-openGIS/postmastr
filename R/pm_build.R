#' Re-build Parsed Street Addresses
#'
#' This is a work in progress!
#'
pm_build <- function(.data, ...){

  ldots <- rlang::chr(...)

  if (rlang::is_empty(ldots) == FALSE){

    vars <- as.formula(ldots)

  } else if (rlang::is_empty(ldots) == TRUE){

    .data %>%
      dplyr::mutate(ADRRESS = paste(houseNum, houseAlpha, houseSuf, stDir, stName, stType, stSufDir, sep = " ", collapse = NULL)) %>%
      dplyr::mutate(ADRRESS = tm::removeWords(ADRRESS, "NA")) %>%
      dplyr::mutate(ADRRESS = stringr::str_squish(ADRRESS)) -> out

  }

  return(out)

}
