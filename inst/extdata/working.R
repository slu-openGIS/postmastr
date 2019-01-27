# second funciotn to parse units still not wokring  1/22/19
test1 <- purrr::map(holder1, fun2)

fun2 <- function(.data){
  if( x$aptsuffx == TRUE)
    stringr::word(x$streetStr,start = -2, end = -1)
}


# for running and simple test of code
library(dplyr)  # ask about where these go
library(stringr)

test_data2 #  test_data2 comes for testData.R in solutions
length(wordCheck)
holder1 <-ifUnit(test_data2, streetStr)
holder1
holder2 <-parseunit(holder1)
holder2
