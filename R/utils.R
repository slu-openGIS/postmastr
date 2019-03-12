# Re-order Columns
#
#
#
pm_reorder <- function(.data, locale = "us"){

  if (locale == "us"){

    # master list of variables for pm objects
    master <- data.frame(
      master.vars = c("pm.uid", "pm.address", "pm.rebuilt", "pm.hasHouse", "pm.house", "pm.hasHouseRange", "pm.houseRange",
                      "pm.hasHouseFrac", "pm.houseFrac", "pm.hasAlpha", "pm.hasHouseSuf", "pm.houseSuf",
                      "pm.hasDir", "pm.preDir", "pm.street", "pm.hasStreetSuf", "pm.streetSuf", "pm.sufDir",
                      "pm.hasUnit", "pm.unitType", "pm.unitNum", "pm.hasCity", "pm.city",
                      "pm.hasState", "pm.state", "pm.hasZip", "pm.zip", "pm.zip4"),
      stringsAsFactors = FALSE
    )

    # create data frame of current variables
    working <- data.frame(
      master.vars = names(.data),
      working.vars = names(.data),
      stringsAsFactors = FALSE
    )

    # join master and working data
    joined <- dplyr::left_join(master, working, by = "master.vars")

    # create vector of re-ordered variables
    out <- stats::na.omit(joined$working.vars)

  }

  # return output
  return(out)

}

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
