# Create U.S. Directional Dictionary

dic_us_dir <- data.frame(
  dir.output = c("N", "N", "S", "S", "E", "E", "W", "W",
                 "NE", "NE", "NE", "NW", "NW", "NW",
                 "SE", "SE", "SE", "SW", "SW", "SW"),
  dir.input = c("N", "North", "S", "South", "E", "East", "W", "West",
                "NE", "Northeast", "North East", "NW", "Northwest", "North West",
                "SE", "Southeast", "South East", "SW", "Southwest", "South West"),
  stringsAsFactors = FALSE
)

dic_us_dir <- dic_us_dir[order(dic_us_dir$dir.output),]
dic_us_dir <- dplyr::as_tibble(dic_us_dir)

usethis::use_data(dic_us_dir, overwrite = TRUE)
