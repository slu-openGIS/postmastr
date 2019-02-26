#' Create Dictionary Objects
#'
#' @description This function allows for the creation of dictionary objects that are
#'     either optional or required elements of other \code{postmastr} functions.
#'
#' @usage pm_dictionary(locale = "us", type, append, filter, case = c("title", "lower", "upper"))
#'
#' @param locale A string indicating the country these data represent; the only
#'     current option is "us" but this is included to facilitate future expansion.
#' @param type A string indicating the grammatical address element the dictionary
#'     should represent. Current options are \code{"state"} and \code{"city"}.
#' @param append An optional dictionary appendix object created with \code{\link{pm_append}}
#' @param filter An optional character scalar or vector with output elements that should
#'     be retained.
#' @param case An optional character scalar or vector containing one or more of three valid
#'     options - \code{"title"} (e.g. "Missouri"), \code{"lower"} (e.g. "missouri), or
#'     \code{"upper"} (e.g. "MISSOURI"). These are used to create a more robust dictionary of
#'     input terms.
#'
#' @return A \code{postmastr} dictionary object, which will always include an input column
#'     of possible terms for the given grammatical address element and an output column
#'     with the desired output for each input.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#'
#' @export
pm_dictionary <- function(locale = "us", type, append, filter, case = c("title", "lower", "upper")){

  if (locale == "us"){

    if (type == "state"){

      if (missing(append) == FALSE & missing(filter) == FALSE){
        working <- pm_dictionary_us_states(append = append, filter = filter)
      } else if (missing(append) == FALSE & missing(filter) == TRUE){
        working <- pm_dictionary_us_states(append = append)
      } else if (missing(append) == TRUE & missing(filter) == FALSE){
        working <- pm_dictionary_us_states(filter = filter)
      } else if (missing(append) == TRUE & missing(filter) == TRUE){
        working <- pm_dictionary_us_states()
      }

      out <- pm_case(working, locale = locale, type = type, case = case)

    } else if (type == "city"){

      if (missing(filter) == TRUE){
        stop("The argument 'filter' is required for constructing city dictionaries.")
      }

      if (missing(append) == FALSE){
        out <- pm_dictionary_us_cities(append = append, states = filter)
      } else if (missing(append) == TRUE){
        out <- pm_dictionary_us_cities(states = filter)
      }

    } else if (type == "directional"){

      if (missing(append) == FALSE & missing(filter) == FALSE){
        working <- pm_dictionary_us_dir(append = append, filter = filter)
      } else if (missing(append) == FALSE & missing(filter) == TRUE){
        working <- pm_dictionary_us_dir(append = append)
      } else if (missing(append) == TRUE & missing(filter) == FALSE){
        working <- pm_dictionary_us_dir(filter = filter)
      } else if (missing(append) == TRUE & missing(filter) == TRUE){
        working <- pm_dictionary_us_dir()
      }

      out <- pm_case(working, locale = locale, type = type, case = case)

    }

  }

  # return output
  return(out)

}

# us states
pm_dictionary_us_states <- function(append, filter){

  # global bindings
  state.output = NULL

  # load data
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

  # return output
  return(out)

}

# us cities
pm_dictionary_us_cities <- function(append, states){

  out <- pm_get_tidycensus(states = states)

  # optionally append
  if (missing(append) == FALSE){

    # bind rows
    out <- dplyr::bind_rows(out, append)

    # re-order observations
    out <- out[order(out$city.input),]

  }

  # return output
  return(out)

}

# us cities via tidycensus
pm_get_tidycensus <- function(states){

  # global bindings
  state.abb = NAME = NULL

  # download data
  states %>%
    base::split(states) %>%
    purrr::map_df(~ suppressMessages(
      tidycensus::get_acs(year = 2017, state = .x, geography = "place", variable = "B01003_001"))) -> data

  # create state dictionary
  dict <- data.frame(
    state.name = c(datasets::state.name),
    state.abb = c(datasets::state.abb),
    stringsAsFactors = FALSE
  )

  dict <- dplyr::filter(dict, state.abb %in% states)
  dict <- dict$state.name

  # parse state names
  data %>%
    dplyr::select(NAME) %>%
    dplyr::mutate(NAME = stringr::str_replace_all(NAME, pattern = ",", replace = "")) %>%
    pm_parse_place(dictionary = dict) %>%
    dplyr::mutate(NAME = stringr::str_trim(NAME, side = "right")) -> data

  # create directory
  dict <- c("city", "town", "village", "CDP")

  # parse place types
  data %>%
    pm_parse_place(dictionary = dict) -> data

  # clean-up output
  data %>%
    dplyr::mutate(NAME = stringr::str_trim(NAME, side = "right")) %>%
    dplyr::distinct(NAME, .keep_all = TRUE) %>%
    dplyr::rename(city.input = NAME) -> data

  # re-order observations
  data <- data[order(data$city.input),]

  # return output
  return(data)

}

# parse components of tidycensus data
pm_parse_place <- function(.data, dictionary){

  # global bindings
  NAME = pm.place = NULL

  # iterate over observations
  .data %>%
    dplyr::mutate(pm.place = purrr::map(NAME, ~ pm_extract_pattern(.x, dictionary = dictionary, end = TRUE))) -> .data

  # clean address data
  .data %>%
    tidyr::unnest(pm.place) %>%
    dplyr::filter(is.na(pm.place) == FALSE) %>%
    dplyr::mutate(pm.place = as.character(pm.place)) %>%
    dplyr::mutate(NAME = stringr::str_replace(NAME,
                                              pattern = stringr::str_c("\\b", pm.place, "\\b$"),
                                              replacement = "")) %>%
    dplyr::select(-pm.place) -> .data

  return(.data)

}

# us states
pm_dictionary_us_dir <- function(append, filter){

  # global bindings
  dir.output = NULL

  # load data
  out <- postmastr::dic_us_dir

  # optionally append
  if (missing(append) == FALSE){

    # bind rows
    out <- dplyr::bind_rows(out, append)

    # re-order observations
    out <- out[order(out$dir.output),]

  }

  # optionally filter
  if (missing(filter) == FALSE){
    out <- dplyr::filter(out, dir.output %in% filter)
  }

  # return output
  return(out)

}

# Dictionary Case
pm_case <- function(.data, locale, type, case){

  if (locale == "us"){

    if (type == "state"){
      out <- pm_convert_case(.data, var = "state.input", orderVar = "state.output", case = case)
    } else if (type == "directional"){
      out <- pm_convert_case(.data, var = "dir.input", orderVar = "dir.output", case = case)
    }

  }

  return(out)

}

# Convert Case
pm_convert_case <- function(.data, var, orderVar, case){

  # global binding
  ...case = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # reformat address variable
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # convert
  title <- dplyr::mutate(.data, ...case = "title")
  lower <- dplyr::mutate(.data,
                         !!varQN := stringr::str_to_lower(!!varQ),
                         ...case = "lower")
  upper <- dplyr::mutate(.data,
                         !!varQN := stringr::str_to_upper(!!varQ),
                         ...case = "upper")

  dplyr::bind_rows(title, lower, upper) %>%
    dplyr::filter(...case %in% case) %>%
    dplyr::distinct(!!varQ, .keep_all = TRUE) %>%
    dplyr::select(-...case) -> out

  # re-order observations
  out <- out[order(out[[orderVar]]),]

}

#' Append Custom Vectors to Dictionary Objects
#'
#' @description This function allows for the creation of dictionary objects that are
#'     either optional or required elements of other \code{postmastr} functions.
#'
#' @usage pm_append(locale = "us", type, input, output)
#'
#' @param locale A string indicating the country these data represent; the only
#'     current option is "us" but this is included to facilitate future expansion.
#' @param type A string indicating the grammatical address element the dictionary
#'     should represent. Current options are \code{"state"} and \code{"city"}.
#' @param input A character scalar or vector containing possible terms existing in
#'     the data. This should be the same length as \code{output}.
#' @param output A character scalar or vector containing desired output for each input.
#'     This should be the same length as \code{input}. This argument is required for
#'     \code{locale = "state"} and optional for \code{locale = "city"}.
#'
#' @importFrom dplyr as_tibble
#'
#' @export
pm_append <- function(locale = "us", type, input, output){

  # global binding
  state.output = state.input = city.output = city.input = NULL

  if (locale == "us"){

    if (type == "state"){

      out <- data.frame(
        state.output = c(output),
        state.input = c(input),
        stringsAsFactors = FALSE
      )

      out <- dplyr::mutate(out,
                           state.output = stringr::str_to_upper(state.output),
                           state.input = stringr::str_to_title(state.input))

      out <- dplyr::as_tibble(out)

    } else if (type == "city"){

      if (missing(output) == FALSE){
        out <- data.frame(
          city.output = c(output),
          city.input = c(input),
          stringsAsFactors = FALSE
        )

      } else if (missing(output) == TRUE){
        out <- data.frame(
          city.input = c(input),
          stringsAsFactors = FALSE
        )

      }

      out <- dplyr::as_tibble(out)

    }

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
#' str(dic_us_states)
#' head(dic_us_states)
#'
"dic_us_states"

#' Directional Dictionary, United States of America
#'
#' @description A list of abbreviations for full names for prefix and suffix directionals
#'    in English.
#'
#' @docType data
#'
#' @usage data(dic_us_dir)
#'
#' @format A tibble with...
#' \decribe{
#'   \item{dir.output}{standard directional abbreviations}
#'   \item{dir.input}{standard full names and directional abbreviations}
#' }
"dic_us_dir"
