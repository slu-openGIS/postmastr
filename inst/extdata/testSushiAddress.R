devtools::load_all()

postmastr::sushi2 %>%
  pm_identify(var = address) %>%
  filter(pm.uid %in% c(3:4) == FALSE) -> sushi2

sushi2_min <- pm_prep(sushi2, var = "address")

pm_has_house(sushi2_min)

pm_any_house(sushi2_min)

pm_all_house(sushi2_min)

pm_no_house(sushi2_min)

pm_parse_house(sushi2_min)

sushi2_range <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407-3409 Olive St", pm.address))
pm_parse_house(sushi2_range)

sushi2_range2 <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407-09 Olive St", pm.address))
pm_parse_house(sushi2_range2)

sushi2_range3 <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407-9 Olive St", pm.address))
pm_parse_house(sushi2_range3)

sushi2_range4 <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407-409 Olive St", pm.address))
pm_parse_house(sushi2_range4)

rm(sushi2_range, sushi2_range2, sushi2_range3, sushi2_range4)

sushi2_frac <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407 1/2 Olive St", pm.address))
sushi2_frac <- pm_parse_house(sushi2_frac)

pm_has_houseFrac(sushi2_frac)

pm_any_houseFrac(sushi2_frac)

pm_all_houseFrac(sushi2_frac)

pm_no_houseFrac(sushi2_frac)

pm_parse_houseFrac(sushi2_frac)

dirs <- pm_dictionary(locale = "us", type = "directional", filter = c("N", "S", "E", "W"))
sushi2_min <- pm_parse_house(sushi2_min)

pm_has_street_dir(sushi2_min, dictionary = dirs)

pm_any_street_dir(sushi2_min, dictionary = dirs)

pm_all_street_dir(sushi2_min, dictionary = dirs)

pm_no_street_dir(sushi2_min, dictionary = dirs)

sushi2_suf <- dplyr::mutate(sushi2_min, pm.address = ifelse(pm.uid == 1, "3407 Olive St South", pm.address))

pm_parse_street_dir(sushi2_suf, dictionary = dirs)

sushi2_min <- pm_parse_street_dir(sushi2_min, dictionary = dirs)

sufs <- pm_dictionary(locale = "us", type = "suffix")

pm_has_street_suf(sushi2_min, dictionary = sufs)

pm_any_street_suf(sushi2_min, dictionary = sufs)

pm_all_street_suf(sushi2_min, dictionary = sufs)

pm_no_street_suf(sushi2_min, dictionary = sufs)

pm_parse_street_suf(sushi2_min, dictionary = sufs)


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
  pm_parse_house() %>%
  pm_parse_street_dir(dictionary = dirs) %>%
  pm_parse_street_suf(dictionary = sufs) %>%
  pm_parse_street() %>%
  pm_rebuild() %>%
  pm_replace(source = sushi2, newVar = clean_address)

postmastr::sushi2 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(style = "short", newVar = clean_address, dirDictionary = dirs, suffixDictionary = sufs)

moDict <- pm_dictionary(locale = "us", type = "state", filter = "MO", case = c("title", "upper"))
cityDict <- pm_append(type = "city",
                      input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood", "St. Louis", "SAINT LOUIS", "Webster Groves"),
                      output = c(NA, NA, "Clayton", NA, NA, "St. Louis", NA))

postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  pm_parse(style = "full", newVar = clean_address,
           dirDictionary = dirs,
           suffixDictionary = sufs,
           cityDictionary = cityDict,
           stateDictionary = moDict)
