#' Create Dictionary Objects
#'
#' @description This function allows for the creation of dictionary objects that are
#'     either optional or required elements of other \code{postmastr} functions.
#'
#' @usage pm_dictionary(locale = "us", type, append, filter)
#'
#' @param locale A string indicating the country these data represent; the only
#'     current option is "us" but this is included to facilitate future expansion.
#' @param type A string indicating the grammatical address element the dictionary
#'     should represent. Current options are \code{"state"}.
#' @param append An optional dictionary appendix object created with \code{\link{pm_append}}
#' @param filter An optional character scalar or vector with output elements that should
#'     be retained.
#'
#' @return A \code{postmastr} dictionary object, which will always include an input column
#'     of possible terms for the given grammatical address element and an output column
#'     with the desired output for each input.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#'
#' @export
pm_dictionary <- function(locale = "us", type, append, filter){

  if (locale == "us" & type == "state"){
    if (missing(append) == FALSE & missing(filter) == FALSE){
      out <- pm_dictionary_us_states(append = append, filter = filter)
    } else if (missing(append) == FALSE & missing(filter) == TRUE){
      out <- pm_dictionary_us_states(append = append)
    } else if (missing(append) == TRUE & missing(filter) == FALSE){
      out <- pm_dictionary_us_states(filter = filter)
    } else if (missing(append) == TRUE & missing(filter) == TRUE){
      out <- pm_dictionary_us_states()
    }
  }

  # return output
  return(out)

}

# us states
pm_dictionary_us_states <- function(append, filter){

  out <- postmastr::dic_us_states

  # optionally append
  if (missing(append) == FALSE){

    # bind rows
    out <- dplyr::bind_rows(out, append)

    # re-order observations
    out <- out[order(out$state.output),]

  }

  # optionally filter
  if (missing(filter) == FALSE){
    out <- dplyr::filter(out, state.output %in% filter)
  }

  return(out)

}

#' Append Custom Vectors to Dictionary Objects
#'
#' @description This function allows for the creation of dictionary objects that are
#'     either optional or required elements of other \code{postmastr} functions.
#'
#' @usage pm_dictionary(locale = "us", type, input, output)
#'
#' @param locale A string indicating the country these data represent; the only
#'     current option is "us" but this is included to facilitate future expansion.
#' @param type A string indicating the grammatical address element the dictionary
#'     should represent. Current options are \code{"state"}.
#' @param input A character scalar or vector containing possible terms existing in
#'     the data. This should be the same length as \code{output}.
#' @param output A character scalar or vector containing desired output for each input.
#'     This should be the same length as \code{input}.
#'
#' @importFrom dplyr as_tibble
#'
#' @export
pm_append <- function(locale, type, input, output){

  if (locale == "us" & type == "state"){

    out <- data.frame(
      state.output = c(output),
      state.input = c(input),
      stringsAsFactors = FALSE
    )

    out <- dplyr::as_tibble(out)

  }

  # return output
  return(out)

}

#' State Dictionary, United States of America
#'
#' @description A listing of abbreviations and full names for U.S. states, territories,
#'     and military "states" used for addressing mail to members of the U.S. armed forces.
#'     This dictionary includes all territories given two-letter state (or equivalent)
#'     abbreviations by the U.S. Postal Service.
#'
#' @docType data
#'
#' @usage data(dic_us_states)
#'
#' @format A tibble with 124 rows and 2 variables:
#' \describe{
#'   \item{state.output}{standard two-letter abbreviations}
#'   \item{state.input}{standard full names and two-letter abbreviations}
#' }
#'
#' @seealso \href{https://pe.usps.com/text/pub28/28apb.htm}{U.S. Postal Service, Publication 28, Appendix B}
#'
#' @examples
#' str(sushi1)
#' head(sushi1)
#'
"dic_us_states"
