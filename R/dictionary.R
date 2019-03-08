#' Create Dictionary Objects
#'
#' @description This function allows for the creation of dictionary objects that are
#'     either optional or required elements of other \code{postmastr} functions. Once created,
#'     dictionary objects should not be modified as the formatting of dictionary elements provides
#'     other \code{postmastr} functions with predictable inputs. The \code{"state"},
#'     \code{"directional"}, and \code{"suffix"} dictionaries are based on tables that are also
#'     exported so that they can be previewed and tested.
#'
#'     Street dictionaries cannot be created using \code{pm_dictionary}. Users who wish to standardize
#'     street names in their data should create a dictionary appendix with \code{pm_append} and use
#'     that as the primary dictionary for functions that accept street dictionaries. The same is true
#'     for house suffix dictionaries.
#'
#' @details The city dictionary functionality is powered by the \link[tidycensus]{get_acs} function
#'     from the \pkg{tidycensus} package. This requires a Census Bureau API key, which can be
#'     obtained at \url{http://api.census.gov/data/key_signup.html}. Once you have a key, the
#'     \link[tidycensus]{census_api_key} function from \pkg{tidycensus} should be used to
#'     set-up the key before proceeding with the creation of any dictionary objects for cities.
#'
#' @usage pm_dictionary(type, append, filter, case = c("title", "lower", "upper"), locale = "us")
#'
#' @param type A string indicating the grammatical address element the dictionary
#'     should represent. Current options are \code{"state"}, \code{"city"},
#'     \code{"directional"}, and \code{"suffix"}.
#' @param append An optional dictionary appendix object created with \code{\link{pm_append}}
#' @param filter An optional character scalar or vector with output elements that should
#'     be retained. Filter inputs are based on the expected output: states should be specified
#'     with their two-letter abbrevation (i.e. \code{"MO"}), cities should be specified in title
#'     case (i.e. \code{"St. Louis"}), directionals should use one- or two-letter abbreivations
#'     (i.e. \code{"N"}), and street suffixes should use the \code{suf.output} for the desired
#'     suffix (i.e. \code{"Ave"} or \code{"Rd"}).
#' @param case An optional character scalar or vector containing one or more of three valid
#'     options - \code{"title"} (e.g. "Missouri"), \code{"lower"} (e.g. "missouri), or
#'     \code{"upper"} (e.g. "MISSOURI"). These are used to create a more robust dictionary of
#'     input terms.
#' @param locale A string indicating the country these data represent; the only
#'     current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A \code{postmastr} dictionary object, which will always include an input column
#'     of possible terms for the given grammatical address element. All dictionary objects
#'     except for city dictionaries will also contain an output column with the
#'     preferred output for each input. For American addresses, these outputs follow United States
#'     Postal Service guidelines.
#'
#' @seealso \code{\link{pm_append}}, \code{\link{dic_us_dir}}, \code{\link{dic_us_states}},
#'     \code{\link{dic_us_suffix}}
#'
#' @examples
#' # build state dictionary, title case only
#' pm_dictionary(type = "state", filter = "MO", case = "title", locale = "us")
#'
#' # build state dictionary, title and upper case
#' pm_dictionary(type = "state", filter = "MO", case = c("title", "upper"), locale = "us")
#'
#' # build New England state dictionary, all cases
#' pm_dictionary(type = "state", filter = c("CT", "MA", "ME", "NH", "RI", "VT"),
#'     case = c("title", "upper", "lower"), locale = "us")
#'
#' # add custom abbreviation for Massachusetts
#' ## create dictionary appendix
#' ma <- pm_append(type = "state", input = "Mass", output = "MA", locale = "us")
#'
#' ## add dictionary appendix to dictionary
#' pm_dictionary(type = "state", filter = c("CT", "MA", "ME", "NH", "RI", "VT"),
#'     append = ma, case = "title", locale = "us")
#'
#' # build Missouri city dictionary
#' \notrun{
#' # tidycensus::census_api_key("YOUR API KEY GOES HERE")
#' pm_dictionary(type = "city", filter = "MO", case = "upper", locale = "us")
#' }
#'
#' # build directional dictionary
#' pm_dictionary(type = "directional", filter = c("N", "S", "E", "W"), locale = "us")
#'
#' # build suffix dictionary
#' pm_dictionary(type = "suffix", filter = c("Ave", "Rd"), locale = "us")
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#'
#' @export
pm_dictionary <- function(type, append, filter, case = c("title", "lower", "upper"), locale = "us"){

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

    } else if (type == "suffix"){

      if (missing(append) == FALSE & missing(filter) == FALSE){
        working <- pm_dictionary_us_suffix(append = append, filter = filter)
      } else if (missing(append) == FALSE & missing(filter) == TRUE){
        working <- pm_dictionary_us_suffix(append = append)
      } else if (missing(append) == TRUE & missing(filter) == FALSE){
        working <- pm_dictionary_us_suffix(filter = filter)
      } else if (missing(append) == TRUE & missing(filter) == TRUE){
        working <- pm_dictionary_us_suffix()
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

# us directions
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

# us street suffixes
pm_dictionary_us_suffix <- function(append, filter){

  # global bindings
  suf.output = NULL

  # load data
  out <- postmastr::dic_us_suffix

  # optionally append
  if (missing(append) == FALSE){

    # bind rows
    out <- dplyr::bind_rows(out, append)

    # re-order observations
    out <- out[order(out$suf.output),]

  }

  # optionally filter
  if (missing(filter) == FALSE){
    out <- dplyr::filter(out, suf.output %in% filter)
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
    } else if (type == "suffix"){
      out <- pm_convert_case(.data, var = "suf.input", orderVar = "suf.output", case = case)
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

#' Create Dictionary Object Appendicies
#'
#' @description This function allows for the creation of dictionary objects that are
#'     either optional or required elements of other \code{postmastr} functions. These
#'     objects are appendicies that can serve either as stand-alone dictionaries or
#'     used in a \code{\link{pm_dictionary}} call as the input  for the \code{append}
#'     parameter.
#'
#'     Once created, dictionary objects should not be modified as the formatting of dictionary
#'     elements provides other \code{postmastr} functions with predictable inputs. The
#'     \code{"state"}, \code{"directional"}, and \code{"suffix"} dictionaries are based on
#'     tables that are also exported so that they can be previewed and tested.
#'
#' @usage pm_append(type, input, output, locale = "us")
#'
#' @param type A string indicating the grammatical address element the dictionary
#'     should represent. Current options are \code{"state"}, \code{"city"},
#'     \code{"street"}, \code{"house suffix"}, \code{"directional"}, and \code{"suffix"}.
#' @param input A character scalar or vector containing possible terms existing in
#'     the data. This should be the same length as \code{output}.
#' @param output A character scalar or vector containing desired output for each input.
#'     This should be the same length as \code{input}. You may use \code{NA} values
#'     in the \code{output} vector for inputs that already are in the preferred,
#'     standardized form. This argument is optional when \code{type} is equal to
#'     \code{"city"}.
#' @param locale A string indicating the country these data represent; the only
#'     current option is \code{"us"} but this is included to facilitate future expansion.
#'
#' @return A \code{postmastr} dictionary object, which will always include an input column
#'     of possible terms for the given grammatical address element. All dictionary objects
#'     except for city dictionaries will also contain an output column with the
#'     preferred output for each input. For city dictionaries, the \code{output} argument
#'     is optional.
#'
#'     For American addresses, these outputs should follow
#'     United States Postal Service guidelines. These dictionary objects can be used as
#'     stand-alone dictionaries or fed into a \code{\link{pm_dictionary}} call as the input
#'     for the \code{append} parameter.
#'
#' @seealso \code{pm_dictionary}
#'
#' @examples
#' # create stand-alone state dictionary
#' mo <- pm_append(type = "state", input = c("Missouri", "MO", "MISSOURI"),
#'     output = c("MO", "MO", "MO"), locale = "us")
#'
#' # add custom abbreviation for Massachusetts to state dictionary
#' ## create dictionary appendix
#' ma <- pm_append(type = "state", input = "Mass", output = "MA", locale = "us")
#'
#' ## add dictionary appendix to dictionary
#' pm_dictionary(type = "state", filter = c("CT", "MA", "ME", "NH", "RI", "VT"),
#'     append = ma, case = "title", locale = "us")
#'
#' # create stand-alone city dictionary, with spelling correction for some cities
#' cities <- pm_append(type = "city",
#'     input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood", "St. Louis",
#'               "SAINT LOUIS", "Webster Groves"),
#'     output = c(NA, NA, "Clayton", NA, NA, "St. Louis", NA),
#'     locale = "us")
#'
#' # create stand-alone street dictionary
#' cities <- pm_append(type = "street",
#'     input = c("MLK", "M.L.K."),
#'     output = c("Dr. Martin Luther King, Jr.", "Dr. Martin Luther King, Jr."),
#'     locale = "us")
#'
#' @importFrom dplyr as_tibble
#'
#' @export
pm_append <- function(type, input, output, locale = "us"){

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

    } else if (type == "city"){

      if (missing(output) == TRUE){

        out <- data.frame(
          city.input = c(input),
          stringsAsFactors = FALSE)

      } else if (missing(output) == FALSE){

        out <- data.frame(
          city.output = c(output),
          city.input = c(input),
          stringsAsFactors = FALSE)

      }

    } else if (type == "directional"){

      out <- data.frame(
        dir.output = c(output),
        dir.input = c(input),
        stringsAsFactors = FALSE)

    } else if (type == "suffix"){

      out <- data.frame(
        suf.output = c(output),
        suf.input = c(input),
        stringsAsFactors = FALSE)

    } else if (type == "street"){

      out <- data.frame(
        st.output = c(output),
        st.input = c(input),
        stringsAsFactors = FALSE)

    } else if (type == "house suffix"){

      out <- data.frame(
        houseSuf.output = c(output),
        houseSuf.input = c(input),
        stringsAsFactors = FALSE)

    }

  }

  # create and return output
  out <- dplyr::as_tibble(out)
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
#' @format A tibble with 20 rows and 2 variables:
#' \describe{
#'   \item{dir.output}{standard directional abbreviations}
#'   \item{dir.input}{standard full names and directional abbreviations}
#' }
#'
#' @seealso \href{https://pe.usps.com/text/pub28/28c2_014.htm}{U.S. Postal Service, Publication 28, Part 2-33}
#'
#' @examples
#' head(dic_us_dir)
#'
"dic_us_dir"

#' Street Suffix Dictionary, United States of America
#'
#' @description A list of abbreviations for full names for street suffix types for the
#'     United States.
#'
#' @docType data
#'
#' @usage data(dic_us_suffix)
#'
#' @format A tibble with 502 rows and 3 variables:
#' \describe{
#'   \item{suf.type}{suffix type}
#'   \item{suf.output}{standard suffix abbreviation}
#'   \item{suf.input}{standard full names and suffix abbreviations}
#' }
#'
#' @seealso \href{https://pe.usps.com/text/pub28/28apc_002.htm}{U.S. Postal Service, Publication 28, Appendix C1}
#'
#' @examples
#' head(dic_us_suffix)
#'
"dic_us_suffix"
