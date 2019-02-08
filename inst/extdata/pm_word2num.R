#' Convert Words to Numbers
#'
#' \code{pm_word2num} converts words (e.g. "one" and "first") to numeric values (e.g. "1").
#'
#' @param word A character string to be converted
#'
#' @return A numeric value
#'
#' @export
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
