# Create U.S. State Dictionary

dic_us_intersect <- tibble(
  intersect.output = c("at", "at", "at", "at", "at"),
  intersect.input = c("at", "/", "@", "and", "&")
)

dic_us_intersect <- dic_us_intersect[order(dic_us_intersect$intersect.output),]

usethis::use_data(dic_us_intersect, overwrite = TRUE)
