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
ifUnit <- function(.data) {
  # this function reutrns a  true or false  vecotr if the input address has an apt or unit
  approvedWords <- c("APT", "BSMT", "BLDG", "DEPT", "FL", "FRNT", "HNGR", "KEY",
       "LBBY", "LOT", "LOWR", "OFC", "PH", "PIER", "REAR", "RM",
        "SIDE", "SLIP", "SPC", "STOP", "STE", "TRLR", "UNIT", "UPPR")
  sapply(test_data2$streetStr, function(x) stringr::str_detect(x, approvedWords))
}

# for running and simple test of code

#checking pm_aptsuffix
holder1 <-pm_aptsuffix(test_data2)
holder1
str(holder1)

# checking if Unit
test_data2
str(test_data2)
holder2 <- ifUnit(test_data2)
holder2
str(holder2)






