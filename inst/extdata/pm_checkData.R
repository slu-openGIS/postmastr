#' Check data
#'
#' Since \code{postmastr} uses a set of default variable names for returning parsed output,
#' this function can be used to ensure that there will be no conflicts between user data
#' and \code{posmastr}.
#'
#' @param .data A tbl
#'
#' @export
pm_checkData <- function(.data) {

  # for address parsing
  if ( any(names(.data) == "houseNum") == TRUE ) {
    stop('Your data cannot contain a variable named houseNum')
  }
  if ( any(names(.data) == "houseNumL") == TRUE ) {
    stop('Your data cannot contain a variable named houseNumL')
  }
  if ( any(names(.data) == "houseNumU") == TRUE ) {
    stop('Your data cannot contain a variable named houseNumU')
  }
  if ( any(names(.data) == "houseSuf") == TRUE ) {
    stop('Your data cannot contain a variable named houseSuf')
  }
  if ( any(names(.data) == "stFull") == TRUE ) {
    stop('Your data cannot contain a variable named stFull')
  }
  if ( any(names(.data) == "stDir") == TRUE ) {
    stop('Your data cannot contain a variable named stDir')
  }
  if ( any(names(.data) == "stName") == TRUE ) {
    stop('Your data cannot contain a variable named stName')
  }
  if ( any(names(.data) == "stType") == TRUE ) {
    stop('Your data cannot contain a variable named stType')
  }
  if ( any(names(.data) == "stSufDir") == TRUE ) {
    stop('Your data cannot contain a variable named stSufDir')
  }

  # for pm_stSuffix()
  if ( any(names(.data) == "suf_com") == TRUE ) {
    stop('data cannot contain a variable named suf_com')
  }
  if ( any(names(.data) == "suf_cor") == TRUE ) {
    stop('data cannot contain a variable named suf_cor')
  }
  if ( any(names(.data) == "suf_pri") == TRUE ) {
    stop('data cannot contain a variable named suf_pri')
  }
  if ( any(names(.data) == "suf_std") == TRUE ) {
    stop('data cannot contain a variable named suf_std')
  }

  # print success
  cat('Success: No conflicts found in your data. Parse away!')
}
