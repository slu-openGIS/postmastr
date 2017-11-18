#' Standardized Street Suffixes
#'
#' \code{gw_suffix} standardizes a given set of street suffix values to the USPS preferred suffix abbreviation.
#'
#' @usage gw_suffix(.data, suffix, overwrite = TRUE, newSuffix)
#'
#' @param .data A tbl
#' @param suffix A character vector within \code{.data} that contains street suffixes
#' @param overwrite A logical scalar. Should the output overwrite the given variable?
#' @param newSuffix A name for a new vector to be created if \code{overwrite = FALSE}
#'
#' @return \code{gw_suffix} returns a tibble with the requested output - either the suffix variable has
#'     been overwritten or a new variable with corrected suffix abbreviations has been added.
#'
#' @note \code{gw_suffix} requires that a number variable names be unused in the original data - \code{suf_com},
#'     \code{suf_cor}, \code{suf_pri} and \code{suf_std}. If these names are present, \code{gw_suffix} will
#'     return an error. These variables are created temporarily as part of the matching process.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr as_tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate
#' @importFrom rlang :=
#'
#' @export
gw_suffix <- function(.data, suffix, overwrite = TRUE, newSuffix){

  # ensure no conflicts with user's data:
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

  # check newSuffix argument
  if (missing(newSuffix)) {
    newSuffix <- "nullSuffix"
  }

  # save parameters to list
  paramList <- as.list(match.call())

  if (!is.character(paramList$suffix)) {
    var <- rlang::enquo(suffix)
  } else if (is.character(paramList$suffix)) {
    var <- rlang::quo(!! rlang::sym(suffix))
  }

  varQ <- rlang::quo_name(rlang::enquo(var))

  if (!is.character(paramList$newSuffix)) {
    newVar <- rlang::enquo(newSuffix)
  } else if (is.character(paramList$newSuffix)) {
    newVar <- rlang::quo(!! rlang::sym(newSuffix))
  }

  newVarQ <- rlang::quo_name(rlang::enquo(newVar))

  # prevents R CMD check note for undefined gloabl variable:
  id <- NULL
  suf_com <- NULL
  suf_cor <- NULL
  suf_id <- NULL
  suf_pri <- NULL
  suf_std <- NULL

  # create identification variable
  input <- .data
  input <- mutate(input, suf_id = as.numeric(rownames(input)))

  # load standardized data
  correct <- get("stdSuffixTbl")

  # convert street suffix variable to Title Case
  input <- dplyr::mutate(input, "suf_com" := stringr::str_to_title(.data[[varQ]]))

  # fix common issues
  input %>%
    dplyr::mutate(suf_cor = ifelse(suf_com %in% correct$suf_std, suf_com, NA)) %>%
    dplyr::mutate(suf_cor = ifelse(suf_com == "Street", "St", suf_cor)) %>%
    dplyr::mutate(suf_cor = ifelse(suf_com == "Avenue", "Ave", suf_cor)) %>%
    dplyr::mutate(suf_cor = ifelse(suf_com == "Av", "Ave", suf_cor)) %>%
    dplyr::mutate(suf_cor = ifelse(suf_com == "Drive", "Dr", suf_cor)) -> input

  # subset data
  matched <- dplyr::filter(input, is.na(suf_cor) == FALSE)
  input %>%
    dplyr::filter(is.na(suf_cor) == TRUE) %>%
    dplyr::select(-suf_cor) -> unmatched

  # join unmatched data with standardized data
  unmatched <- left_join(unmatched, correct, by = "suf_com")

  unmatched %>%
    dplyr::select(-suf_pri) %>%
    dplyr::rename(suf_cor = suf_std) -> unmatched

  # combine data
  output <- dplyr::bind_rows(matched, unmatched)
  output <- dplyr::arrange(output, suf_id)

  # overwrite data
  if (overwrite == TRUE){

    output <- dplyr::mutate(output, !!varQ := suf_cor)

  } else if (overwrite == FALSE) {

    output <- dplyr::mutate(output, !!newVarQ := suf_cor)

  }

  # remove suffix variables
  output <- dplyr::select(output, -c(suf_com, suf_cor, suf_id))

  # return tibble
  output <- dplyr::as_tibble(output)
  return(output)
}
