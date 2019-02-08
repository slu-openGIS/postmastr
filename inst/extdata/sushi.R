# create example data

library(readr)
library(usethis)

sushi1 <- read_csv("inst/extdata/sushi1.csv")
use_data(sushi1, overwrite = TRUE)

sushi2 <- read_csv("inst/extdata/sushi2.csv")
use_data(sushi2, overwrite = TRUE)
