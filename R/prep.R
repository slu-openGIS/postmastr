#' Prepare Data
#'
#' @param .data A tbl or data frame
#' @param var A character variable containing address data to be parsed
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#' @importFrom tibble rowid_to_column
#'
#' @export
pm_prep <- function(.data, var){

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # add id numbers to each row
  full <- tibble::rowid_to_column(.data, var = "pm.id")

  # add unique id numbers for each address string
  .data %>%
    dplyr::distinct(!!varQ) %>%
    tibble::rowid_to_column(var = "pm.uid") %>%
    dplyr::left_join(full, ., by = varQN) %>%
    dplyr::select(pm.id, pm.uid, dplyr::everything()) -> out

  # return output
  return(out)

}
