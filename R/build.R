#' Rebuild Street Address
#'
#' @export
pm_rebuild <- function(.data, start, end, locale = "us"){

  # global bindings
  pm.rebuilt = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
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

  # rebuild
  .data %>%
    tidyr::unite(pm.rebuilt, !!startQ:!!endQ, sep = " ", remove = FALSE) %>%
    dplyr::mutate(pm.rebuilt = stringr::str_replace_all(pm.rebuilt, pattern = "\\bNA\\b", replacement = "")) %>%
    dplyr::mutate(pm.rebuilt = stringr::str_squish(pm.rebuilt)) -> .data

  # return output
  return(.data)

}

#' Add Address to Source Data
#'
#' @export
pm_replace <- function(.data, source, newVar, keep_elements = FALSE, keep_ids = FALSE){

  # global bindings
  pm.id = pm.uid = pm.rebuilt = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$newVar)) {
    varQ <- rlang::enquo(newVar)
  } else if (is.character(paramList$newVar)) {
    varQ <- rlang::quo(!! rlang::sym(newVar))
  }

  # optionally retain parsed elements
  if (keep_elements == FALSE){

    .data <- dplyr::select(.data, pm.uid, pm.rebuilt)

  }

  # optionally rename output variable
  if (missing(newVar) == FALSE){

    .data <- rename(.data, !!varQ := pm.rebuilt)

  }

  # join parsed and source data
  out <- dplyr::left_join(source, .data, by = "pm.uid")

  # optionally retain id variables
  if (keep_ids == FALSE){

    out <- dplyr::select(out, -pm.id, -pm.uid)

  }

  # return output
  return(out)

}
