devtools::load_all()

postmastr::sushi2 %>%
  pm_identify(var = address) %>%
  filter(pm.uid %in% c(3:4) == FALSE) -> sushi2

sushi2_min <- pm_prep(sushi2, var = "address")

sushi2_alpha <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407 R Olive St", pm.address))
sushi2_alpha <- pm_house_parse(sushi2_alpha)

houseSufs <- pm_append(type = "house suffix", input = c("Front", "Rear", "F", "R"), output = c("Front", "Rear", "Front", "Rear"))

pm_houseSuf_any(sushi2_alpha, dictionary = houseSufs)
pm_houseSuf_all(sushi2_alpha, dictionary = houseSufs)

pm_houseSuf_detect(sushi2_alpha, dictionary = houseSufs)

pm_houseSuf_none(sushi2_alpha, dictionary = houseSufs)

pm_houseSuf_parse(sushi2_alpha, dictionary = houseSufs)

sushi2_range <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407-3409 Olive St", pm.address))
pm_house_parse(sushi2_range)

sushi2_range2 <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407-09 Olive St", pm.address))
pm_house_parse(sushi2_range2)

sushi2_range3 <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407-9 Olive St", pm.address))
pm_house_parse(sushi2_range3)

sushi2_range4 <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407-409 Olive St", pm.address))
pm_house_parse(sushi2_range4)

rm(sushi2_range, sushi2_range2, sushi2_range3, sushi2_range4)

sushi2_frac <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407 1/2 Olive St", pm.address))
sushi2_frac <- pm_house_parse(sushi2_frac)

pm_houseFrac_parse(sushi2_frac)

rm(sushi2_frac)

dirs <- pm_dictionary(locale = "us", type = "directional", filter = c("N", "S", "E", "W"))
sushi2_min <- pm_house_parse(sushi2_min)

sushi2_suf <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407 Olive St South", pm.address))

pm_streetDir_parse(sushi2_suf, dictionary = dirs)

sushi2_min <- pm_streetDir_parse(sushi2_min, dictionary = dirs)

sufs <- pm_dictionary(locale = "us", type = "suffix")

pm_streetSuf_parse(sushi2_min, dictionary = sufs)

rm(sushi2, sushi2_frac, sushi2_min, sushi2_suf)

# =============================

postmastr::sushi2 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  mutate(address = ifelse(name == "BaiKu Sushi Lounge", "3407-11 SECOND AVE", address)) %>%
  mutate(address = ifelse(name == "SUSHI KOI", "7-11R 1/2 SECOND AVE", address)) %>%
  pm_identify(var = address) %>%
  pm_prep(var = "address") %>%
  pm_house_parse() %>%
  pm_houseRange_parse() %>%
  pm_houseFrac_parse() %>%
  pm_streetDir_parse() %>%
  pm_streetSuf_parse() %>%
  pm_street_parse()


postmastr::sushi2 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  mutate(address = ifelse(name == "BaiKu Sushi Lounge", "3407-11 SECOND AVE", address)) %>%
  mutate(address = ifelse(name == "SUSHI KOI", "7-11R 1/2 SECOND AVE", address)) %>%
  pm_identify(var = address) -> sushi_unnest

sushi_unnest %>%
  pm_prep(var = "address") %>%
  pm_house_detect() %>%
  pm_house_parse() %>%
  pm_houseRange_detect() %>%
  pm_houseRange_parse() %>%
  pm_houseFrac_detect() %>%
  pm_houseFrac_parse() %>%
  pm_streetDir_detect() %>%
  pm_streetDir_parse() %>%
  pm_streetSuf_detect() %>%
  pm_streetSuf_parse() %>%
  pm_street_parse() %>%
  pm_replace(source = sushi_unnest) %>%
  pm_rebuild(start = pm.house, end = pm.streetSuf, new_address = clean_address, keep_parsed = "no",
              side = "middle", left_vars = name:address, keep_ids = FALSE)




parse_range <- function(x){

  # convert item to numeric
  vector <- as.numeric(x)

  # expand vector to include every other integer between low and high values
  out <- seq.int(from = vector[1], to = vector[2], by = 2)

  # convert to string
  out <- as.character(out)

  # return output
  return(out)

}

add_fraction <- function(x){

  frac <- stringr::str_c(x[length(x)], " ", "1/2")

  # add frac to end of vector
  vector <- c(x, frac)

  # create output
  # out <- list(vector)

}

# =============================

devtools::load_all()

postmastr::sushi2 %>%
  pm_identify(var = address) %>%
  filter(pm.uid %in% c(3:4) == FALSE) -> sushi2

dirs <- pm_dictionary(locale = "us", type = "directional", filter = c("N", "S", "E", "W"))
sufs <- pm_dictionary(locale = "us", type = "suffix")

sushi2 %>%
  pm_prep(var = "address") %>%
  pm_parse_house() %>%
  pm_parse_street_dir(dictionary = dirs) %>%
  pm_parse_street_suf(dictionary = sufs) %>%
  pm_parse_street() %>%
  pm_rebuild() %>%
  pm_replace(source = sushi2, newVar = clean_address)

postmastr::sushi2 %>%
  pm_identify(var = address) %>%
  filter(pm.uid %in% c(3:4) == FALSE) %>%
  pm_prep(var = "address") %>%
  pm_house_parse() %>%
  pm_streetDir_parse(dictionary = dirs) %>%
  pm_streetSuf_parse(dictionary = sufs) %>%
  pm_street_parse() %>%
  pm_rebuild(start = pm.house, end = pm.streetSuf) %>%
  pm_replace(source = sushi2, newVar = clean_address)

postmastr::sushi2 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(style = "short", newVar = clean_address, dirDict = dirs, suffixDict = sufs)

mo <- pm_dictionary(locale = "us", type = "state", filter = "MO", case = c("title", "upper"))
cities <- pm_append(type = "city",
                      input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood",
                                "St. Louis", "SAINT LOUIS", "Webster Groves"),
                      output = c(NA, NA, "Clayton", NA, NA, "St. Louis", NA))

postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_identify(var = address) %>%
  pm_prep(var = "address") %>%
  pm_postal_parse() %>%
  pm_state_parse(dictionary = moDict) %>%
  pm_city_parse(dictionary = cityDict) %>%
  pm_house_parse() %>%
  pm_houseFrac_parse() %>%
  pm_streetDir_parse(dictionary = dirs) %>%
  pm_streetSuf_parse(dictionary = sufs) %>%
  pm_street_parse(ordinal = TRUE) %>%
  pm_rebuild(start = pm.house, end = "end", add_commas = TRUE)


postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_identify(var = address) %>%
  pm_prep(var = "address") %>%
  pm_postal_parse() %>%
  pm_state_parse(dictionary = moDict) %>%
  pm_city_parse(dictionary = cityDict) %>%
  pm_house_parse() %>%
  pm_houseFrac_parse() %>%
  pm_streetDir_parse(dictionary = dirs) %>%
  pm_streetSuf_parse(dictionary = sufs) %>%
  pm_street_parse(ordinal = TRUE) %>%
  pm_rebuild(start = pm.house, end = pm.streetSuf) %>%
  pm_replace(source = sushi1, newVar = clean_address, keep_parsed = "limited")

postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_identify(var = address) -> sushi1

sushi1 %>%
  pm_prep(var = "address") %>%
  pm_postal_parse() %>%
  pm_state_parse(dictionary = moDict) %>%
  pm_city_parse(dictionary = cityDict) %>%
  pm_house_parse() %>%
  pm_houseFrac_parse() %>%
  pm_streetDir_parse(dictionary = dirs) %>%
  pm_streetSuf_parse(dictionary = sufs) %>%
  pm_street_parse(ordinal = TRUE) %>%
  pm_rebuild(start = pm.house, end = pm.streetSuf) %>%
  pm_replace(source = sushi1, keep_parsed = "limited")

postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(input = "full",
           var = "address",
           output = "full",
           dirDict = dirs,
           suffixDict = sufs,
           cityDict = cities,
           stateDict = mo)

postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(input = "full",
           var = address,
           output = "full",
           dirDict = dirs,
           suffixDict = sufs,
           cityDict = cities,
           stateDict = mo)

postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(input = "full",
           var = address,
           output = "short",
           dirDict = dirs,
           suffixDict = sufs,
           cityDict = cities,
           stateDict = mo)


postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(input = "full",
           var = address,
           output = "short",
           keep_parsed = "limited",
           dirDict = dirs,
           suffixDict = sufs,
           cityDict = cityDict,
           stateDict = moDict)

postmastr::sushi2 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(input = "short",
           address = address,
           dir_dict = dirs,
           suffix_dict = sufs)

postmastr::sushi2 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(input = "short",
           address = address)

postmastr::sushi2 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  mutate(address = ifelse(name == "BaiKu Sushi Lounge", "3407 FORTY NINTH ST PEDESTRIAN MALL", address)) %>%
  pm_parse(input = "short",
           address = address)

postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(input = "full",
           address = address,
           output = "short",
           city_dict = cities)

