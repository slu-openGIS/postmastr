#' Parse apartment and unit into standarazed formte
#'
#' \code{pm_aptsuffix.R} identifies aparemnt and unit numbers (i.e. 123 N Washington St apt 3E ) and
#' parses those directions into their own variable.
#'
#' @param .data A tbl
#' @param unit  Name of the variable containing unit/apt
#' @param output Optional name for output variable
#'
#' @export


# this function reutrns a  true or false  vecotr if the input address has an apt or unit
ifUnit <- function(.data, var ) {

    # save parameters to list
    paramList <- as.list(match.call())

    # nse
    if (!is.character(paramList$var)) {
      varQ <- rlang::enquo(var)
    } else if (is.character(paramList$var)) {
      varQ <- rlang::quo(!! rlang::sym(var))
    }

    # create vector through iteration
    out <- dplyr::mutate(.data, aptsuffx = purrr::map_lgl(!!varQ, isApt))

    # return output
    return(out)

  }

# define sub function
isApt <- function(x){

    # create pattern vector
    patternVector <- c("APT", "BSMT", "BLDG", "DEPT", "FL", "FRNT", "HNGR", "KEY",
                    "LBBY", "LOT", "LOWR", "OFC", "PH", "PIER", "REAR", "RM",
                    "SIDE", "SLIP", "SPC", "STOP", "STE", "TRLR", "UNIT", "UPPR")


    # iterate over items in pattern vector
    patternVector %>%
      base::split(patternVector) %>%
      purrr::map_lgl( ~ stringr::str_detect(string = x, pattern = .x)) %>%
      base::any(.) -> out

    # return output
    return(out)

}


# function that parses out the unit and adds it to a colums vector in data frame
parseunit <- function(.data) {
  newve <- stringr::word(.data$streetStr,start = -2, end = -1) # how apt suffix is removed from address
  tester1 <- .data %>%
    dplyr::mutate(isaptsuffix = newve)
  count <- 0
  for(i in tester1$aptsuffx) {
    count <- count +1
    if(i  == FALSE) {
      tester1$isaptsuffix[count] <- NA
    }
  }
  return(tester1)
}

# re-work Bayard's code
pm_unit_parse <- function(.data, stName){

  varQ <- rlang::quo_name(rlang::enquo(stName))

  # load suffix data
  correct <- get("stdSuffixTbl")

  .data %>%
    dplyr::mutate(count = stringr::str_count(stName, pattern = "\\S+")) -> out

  return(out)

}
