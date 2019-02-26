load("inst/extdata/suffixDataOrigal.RData")

dic_us_suffix <- stdSuffixTbl %>%
  rename(
    suf.type = suf_pri,
    suf.input = suf_com,
    suf.output = suf_std
  )

dic_us_suffix <- dic_us_suffix[order(dic_us_suffix$suf.output),]
dic_us_suffix <- dplyr::as_tibble(dic_us_suffix)

usethis::use_data(dic_us_suffix, overwrite = TRUE)

