# iterate over directory items
pm_has_pattern <- function(x, dictionary, end = TRUE){

  # create pattern vector
  patternVector <- dictionary

  patternVector %>%
    base::split(patternVector) %>%
    purrr::map_lgl( ~ stringr::str_detect(x, pattern = ifelse(end == TRUE,
                                                              stringr::str_c("\\b", .x, "\\b$"),
                                                              stringr::str_c("\\b", .x, "\\b")))) %>%
    any() -> out

  return(out)

}

# iterate over dictionary items per observations
pm_extract_pattern <- function(x, dictionary, end = TRUE){

  # create pattern vector
  patternVector <- dictionary

  patternVector %>%
    base::split(patternVector) %>%
    purrr::map( ~ stringr::str_extract(x, pattern = ifelse (end == TRUE,
                                                            stringr::str_c("\\b", .x, "\\b$"),
                                                            stringr::str_c("\\b", .x, "\\b")))) -> out

  return(out)

}
