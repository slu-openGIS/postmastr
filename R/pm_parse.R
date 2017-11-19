#' Parse Address
#'
#' @export
pm_parse <- function(.data, address, houseNum = TRUE, overwrite = TRUE, keepVars = TRUE) {

  origVars <- names(.data)

  # save parameters to list
  paramList <- as.list(match.call())

  # parse address
  .data %>%
    pm_houseNum(address = streetStr) %>%
    pm_houseAlpha(houseNum = houseNum) %>%
    pm_stName(address = streetStr) %>%
    pm_stAlpha(stName = stName) %>%
    pm_stPrefixDir(stName = stName) %>%
    pm_stSuffixDir(stName = stName) %>%
    pm_stSuffix(stName = stName) %>%
    pm_stdStreet(stName = stName) -> .data

  # convert address parameter to quosure
  if (!is.character(paramList$address)) {
    var <- rlang::enquo(address)
  } else if (is.character(paramList$address)) {
    var <- rlang::quo(!! rlang::sym(address))
  }

  varQ <- rlang::quo_name(rlang::enquo(var))

  # re-construct full address
  if (houseNum == TRUE) {
    .data <- dplyr::mutate(.data, stFull = paste(houseNum, houseSuf, stPreDir, stName, stType, stSufDir, sep = " "))
  } else if (houseNum == FALSE) {
    .data <- dplyr::mutate(.data, stFull = paste(houseSuf, stPreDir, stName, stType, stSufDir, sep = " "))
  }

  # remove NAs
  .data %>%
    dplyr::mutate(stFull = stringr::str_replace(stFull, " NA ", " ")) %>%
    dplyr::mutate(stFull = ifelse(stringr::word(stFull, -1) == "NA",
                                  stringr::str_replace(stFull, "NA", ""), stFull)) %>%
    dplyr::mutate(stFull = stringr::str_replace(stFull, "  ", " ")) %>%
    dplyr::mutate(stFull = trimws(stFull)) -> .data

  # if keepVars is FALSE, remove all of the parsed address components
  if (keepVars == FALSE & houseNum == TRUE) {
    .data <- dplyr::select(.data, -c(houseNum, houseNumL, houseNumU, stPreDir, stName, stType, stSufDir))
  } else if (keepVars == FALSE & houseNum == FALSE) {
    .data <- dplyr::select(.data, -c(-houseSuf, stPreDir, stName, stType, stSufDir))
  }

  # keeVars is TRUE, reorder and check to see if all address components are needed
  if (keepVars == TRUE & houseNum == TRUE) {
    .data <- dplyr::select(.data, one_of(origVars), stFull, houseNum, houseNumL, houseNumU, houseSuf,
                           stPreDir, stName, stType, stSufDir)

    if (all(is.na(.data$houseNumU)) == TRUE) {
      .data <- dplyr::select(.data, -c(houseNumL, houseNumU))
    }
  } else if (keepVars == TRUE & houseNum == FALSE) {
    .data <- dplyr::select(.data, one_of(origVars), stFull, houseSuf, stPreDir, stName, stType, stSufDir)
  }

  # if keepVars is TRUE, check directional variables to see if they are needed
  if (keepVars == TRUE) {
    if (all(is.na(.data$houseSuf)) == TRUE) {
      .data <- dplyr::select(.data, -c(houseSuf))
    }
    if (all(is.na(.data$stPreDir)) == TRUE) {
      .data <- dplyr::select(.data, -c(stPreDir))
    }
    if (all(is.na(.data$stSufDir)) == TRUE) {
      .data <- dplyr::select(.data, -c(stSufDir))
    }
  }

  # if overwrite is TRUE, replace original address variable with stFull
  if (overwrite == TRUE){
    .data %>%
      dplyr::mutate(!!varQ := stFull) %>%
      dplyr::select(-stFull) -> .data
  }

  # return tibble
  .data <- dplyr::as_tibble(.data)
  return(.data)
}
