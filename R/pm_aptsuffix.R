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
pm_aptsuffix <- function(.data) {
  # function that parses the unit and associted number in type colume and value colum
  checker <- ifUnit(.data)
  return(checker)

}


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

# for running and simple test of code

#checking pm_aptsuffix
library(dplyr)  # ask about where these go
library(stringr)


test_data2
length(wordCheck)
holder1 <-ifUnit(test_data2, streetStr)
holder1
str(holder1)

# checking if Unit
test_data2
str(test_data2)
holder2 <- ifUnit(test_data2)
holder2
str(holder2)


# original fuction
# look for funciotn in stringger for parssing 12/18/18
# words funciton
# stringger spluts based on words()

#sapply(test_data2$streetStr, function(x) stringr::str_detect(x, approvedWords))
#.data %>%
#  mutate(approvedWords = suppressWarnings(str_detect(streetStr, pattern = approvedWords)))

# from pm_sName.R
#if (houseNum == TRUE){
#  .data %>%
#    dplyr::rename(stFull := !!varQ) %>%
#    dplyr::mutate(count = stringr::str_count(stFull, pattern = "\\S+")) %>%
#    dplyr::mutate(stName = ifelse(count >= 3, stringr::word(stFull, start = 2, end = count), NA)) %>%
#    dplyr::mutate(stName = stringr::str_to_title(stName)) %>%
#    dplyr::select(-count) %>%
#    dplyr::rename(!!varQ := stFull) -> .data

parseunit <- function(.data) {
  newve <- stringr::word(tester$streetStr,start = 4, end = 5) # how to soft for different address sizes
  tester1 <- .data %>%
    dplyr::mutate(isaptsuffix = newve)
  for( i in  .data$aptsuffx) {
    if( i   == TRUE){
      tester1 <- tester1 %>%
        dplyr::mutate(isaptsuffix = TRUE)
      stringger::word(.data$streetStr) #to find the suffix
       #dplyr::mutate(stName = ifelse(count >= 3, stringr::word(stFull, start = 2, end = count), NA)) %>%
       #     dplyr::mutate(stName = stringr::str_to_title(stName)) %>%
    }
  }
  return(tester1)
}


holder2 <-parseunit(holder1)
holder2
holder1
length(holder1$aptsuffx)



tester <-  holder1  %>%
       dplyr::mutate(newcolume = TRUE)
tester
test2 <- stringr::word(tester$streetStr,start = 4, end = 5)
test2

holder2$isaptsuffix[2] == FALSE

test3 <- stringr::word(tester$streetStr[5],start = 4, end = 4)
test3

test3 <- stringr::word(tester$streetStr[1],start = 4, end = 5)
test3




