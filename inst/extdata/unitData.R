# Create U.S. Apartment Dictionary

# need to check to make sure both colums accept #
unit.full <- c("Apartment","Basement","Building","Department","Floor","Front","Hanger","key","Lobby","Lot",
               "Lower","Office","Penthouse","Pier","Rear","Room","Side","Slip","Space","Stop","Suite","Trailer",
               "Unit","Upper")
unit.abv <- c("APT","BSMT**","BLDG","DEPT","FL","FRNT**","HNGR","KEY","LBBY**","LOT","LOWR**","OFC**","PH**",
              "PIER","REAR**","RM","SIDE**","SLIP","SPC","STOP","STE","TRLR","UNIT","UPPR**")

# set up data from with optional output of abv or full
if(output == "full") {
  dic_us_unit <- data.frame(
    unit.input = c(unit.full,unit.abe),
    state.output = c(unit.full,unit.full),
    stringsAsFactors = FALSE
  )
}else(ouput == "input") {
  dic_us_unit <- data.frame(
    unit.input = c(unit.full, unit.abv),
    state.ouput = c(unit.abv, unit.abv),
    stringsAsFactors = FALSE
  )
}


dic_us_unit <- dic_us_unit[order(dic_us_unit$unit.output),] # what do these two lines do?
dic_us_unit <- dplyr::as_tibble(dic_us_unit)

usethis::use_data(dic_us_unit, overwrite = TRUE)
