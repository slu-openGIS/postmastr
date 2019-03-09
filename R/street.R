#' Parse Street Names
#'
#' @description Converts the remaining text of \code{pm.address} to title case and stores
#'     it in a new variable named \code{pm.street}.
#'
#' @usage pm_street_parse(.data, dictionary, ordinal = TRUE, drop = TRUE, locale = "us")
#'
#' @details This is typically the last function to be executed before rebuilding and replacing.
#'
#' @param .data A \code{postmastr} object created with \link{pm_prep}
#' @param dictionary Optional; a tbl created with \code{pm_append} to be used to standardize
#'     specific street names.
#' @param ordinal A logical scalar; if \code{TRUE}, street names that contain numeric words values
#'     (i.e. "Second") will be converted and standardized to ordinal values (i.e. "2nd"). The
#'     default is \code{TRUE} because it returns much more compact clean addresses (i.e.
#'     "168th St" as opposed to "One Hundred Sixty Eigth St").
#' @param drop A logical scalar; if \code{TRUE}, the \code{pm.address} variable will
#'     be dropped from the \code{postmastr} object.
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @return A tibble with a new character variable \code{pm.street} that contains
#'     the two-letter abbreviation for the given U.S. state. Variables are automatically
#'     re-ordered, so the new vector will not be in the last position of the tibble.
#'
#' @export
pm_street_parse <- function(.data, dictionary, ordinal = TRUE, drop = TRUE, locale = "us"){

  # global bindings
  pm.address = pm.street = NULL

  # check for object and key variables
  if (pm_has_uid(.data) == FALSE){
    stop("The variable 'pm.uid' is missing from the given object. Create a postmastr object with pm_identify and pm_prep before proceeding.")
  }

  if (pm_has_address(.data) == FALSE){
    stop("The variable 'pm.address' is missing from the given object. Create a postmastr object with pm_prep before proceeding.")
  }

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # parse
  .data <- dplyr::mutate(.data, pm.street = pm.address)

  # reorder output
  vars <- pm_reorder(.data)
  .data <- dplyr::select(.data, vars)

  # set dictionary to null if not specified
  if (missing(dictionary) == TRUE){
    dictionary <- NULL
  }

  # standardize street names
  .data <- pm_street_std(.data, var = "pm.street", dictionary = dictionary, ordinal = ordinal, locale = locale)

  # optionally drop pm.address
  if (drop == TRUE){

    .data <- dplyr::select(.data, -pm.address)

  }

  # return output
  return(.data)

}


#' Standardize Street Names
#'
#' @description Standardize street names by converting to title case, removing punctuation,
#'     and optionally applying ordinal conversion as well as a dictionary to the data.
#'
#' @usage pm_street_std(.data, var, dictionary, ordinal = TRUE, locale = "us")
#'
#' @param .data A postmastr object created with \link{pm_prep}
#' @param var A character variable that may contain street suffixes
#' @param dictionary Optional; a tbl created with \code{pm_append} to be used to standardize
#'     specific street names.
#' @param ordinal A logical scalar; if \code{TRUE}, street names that contain numeric words values
#'     (i.e. "Second") will be converted and standardized to ordinal values (i.e. "2nd"). The
#'     default is \code{TRUE} because it returns much more compact clean addresses (i.e.
#'     "168th St" as opposed to "One Hundred Sixty Eigth St").
#' @param locale A string indicating the country these data represent; the only
#'    current option is "us" but this is included to facilitate future expansion.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang :=
#' @importFrom rlang enquo
#' @importFrom rlang quo
#' @importFrom rlang sym
#'
#' @export
pm_street_std <- function(.data, var, dictionary, ordinal = TRUE, locale = "us"){

  # global bindings
  . = st.input = st.output = NULL

  # save parameters to list
  paramList <- as.list(match.call())

  # unquote
  if (!is.character(paramList$var)) {
    varQ <- rlang::enquo(var)
  } else if (is.character(paramList$var)) {
    varQ <- rlang::quo(!! rlang::sym(var))
  }

  varQN <- rlang::quo_name(rlang::enquo(var))

  # locale issues
  if (locale != "us"){
    stop("At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
  }

  # convert to title case
  .data <- dplyr::mutate(.data, !!varQ := stringr::str_to_title(!!varQ))

  # remove punctuation
  .data <- dplyr::mutate(.data, !!varQ := stringr::str_replace(!!varQ, "[.]", ""))

  # optionally convert ordinal street names
  # i.e. Second to 2nd
  if (ordinal == TRUE){

    .data <- pm_street_ord(.data, var = !!varQ, locale = locale)

  }

  # optionally standardize further with dictionary
  if (missing(dictionary) == FALSE){

    if (is.null(dictionary) == FALSE){

      # set-up dictionary
      dictionary %>%
        dplyr::rename(!!varQ := st.input) -> dictionary

      # standardize
      .data %>%
        dplyr::left_join(., dictionary, by = varQN) %>%
        dplyr::mutate(!!varQ := ifelse(is.na(st.output) == FALSE, st.output, !!varQ)) %>%
        dplyr::select(-st.output) -> .data

    }

  }

  # return output
  return(.data)

}

# Convert Ordinal Street Names
#
pm_street_ord <- function(.data, var, locale = "us"){

  # quote input
  varQ <- rlang::enquo(var)

  # parse ordinals
  if (locale == "us"){

    .data <- pm_street_ord_us(.data, var = !!varQ)

  }

  # return output
  return(.data)

}

# U.S. ordinal street names
pm_street_ord_us <- function(.data, var){

  # global bindings
  ...ordSt = ...oid = ...street = pm.street = pm.uid = NULL

  # quote input
  varQ <- rlang::enquo(var)

  # rename input
  .data <- dplyr::rename(.data, ...street := !!varQ)

  # create dictionary of numeric words
  dict <- c("One", "First", "Two", "Second", "Three", "Third", "Four", "Fourth",
                "Five", "Fifth", "Six", "Sixth", "Seven", "Seventh", "Eight", "Nine",
                "Ninth", "Ten", "Tenth", "Eleven", "Eleventh", "Twelve", "Twelfth",
                "Thirteen", "Thirteenth", "Fourteen", "Fourteenth", "Fifteen", "Fifteenth",
                "Sixteen", "Sixteenth", "Seventeen", "Seventeenth", "Eighteen", "Eighteenth",
                "Nineteen", "Nineteenth", "Twenty", "Twentieth", "Thirty", "Thirtieth",
                "Forty", "Fortieth", "Fifty", "Fiftieth", "Sixty", "Sixtieth",
                "Seventy", "Seventieth", "Eighty", "Eightieth", "Ninety", "Ninetieth")

  # minimize dictionary
  dict <- paste(dict, collapse = "|")

  # add id and identify ordinal streets
  .data %>%
    dplyr::mutate(...ordSt = stringr::str_detect(stringr::word(...street, 1), pattern = dict)) %>%
    tibble::rowid_to_column(var = "...oid") -> .data

  # subset
  yesOrd <- dplyr::filter(.data, ...ordSt == TRUE)
  noOrd <- dplyr::filter(.data, ...ordSt == FALSE)

  # convert
  yesOrd$...street <- sapply(yesOrd$...street, pm_word2num, USE.NAMES = FALSE)
  yesOrd$...street <- sapply(yesOrd$...street, toOrdinal::toOrdinal, USE.NAMES = FALSE)
  yesOrd$...street <- as.character(yesOrd$...street)

  # bind
  dplyr::bind_rows(noOrd, yesOrd) %>%
    dplyr::arrange(...oid) %>%
    dplyr::select(-...ordSt, -...oid) %>%
    dplyr::rename(!!varQ := ...street) -> .data

}

# convert words to numbers
pm_word2num <- function(word){

  # reformat input
  word <- gsub("-", " ", word)
  wsplit <- strsplit(tolower(word)," ")[[1]]

  # lists of values
  one_digits <- list(zero=0, one=1, first=1, two=2, second=2, three=3, third=3, four=4, fourth=4,
                     five=5, fifth=5, six=6, sixth=6, seven=7, seventh=7, eight=8, eighth=8,
                     nine=9, ninth=9)
  teens <- list(eleven=11, eleventh=11, twelve=12, twelfth=12, thirteen=13, thirteenth=13,
                fourteen=14, fourteenth=14, fifteen=12, fifteenth=15, sixteen=16,
                sixteenth=16, seventeen=17, seventeenth=17, eighteen=18, eighteenth=18,
                nineteen=19, nineteenth=19)
  ten_digits <- list(ten=10, tenth=10, twenty=20, twentieth=20, thirty=30, thirtieth=30,
                     forty=40, fortieth=40, fifty=50, fiftieth=50, sixty=60, sixtieth=60,
                     seventy=70, seventieth=70, eighty=80, eightieth=80,
                     ninety=90, ninetieth=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1

  # process inputs
  while(i <= length(wsplit)){
    j <- 1
    if(i==1 && wsplit[i]=="hundred")
      temp <- 100
    else if(i==1 && wsplit[i]=="thousand")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="hundred"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }

  # return value
  return(out)
}

# https://stackoverflow.com/questions/18332463/convert-written-number-to-number-in-r
