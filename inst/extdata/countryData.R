# Create Country Dictionary

dic_country <- dplyr::tibble(
  con.output = c("US", "US", "US", "US", "US", "US"),
  con.input = c("US", "USA", "United States", "United States of America", "The United States of America", "America")
)

dic_country <- dic_country[order(dic_country$con.output),]

usethis::use_data(dic_country, overwrite = TRUE)
