devtools::load_all()

# prep
sushi1_full <- postmastr::sushi1
sushi1_full <- dplyr::filter(sushi1_full, name != "Drunken Fish - Ballpark Village")

sushi1_full <- pm_identify(sushi1_full, var = "address")

pm_evaluate(sushi1_full)

sushi1_min <- pm_prep(sushi1_full, var = "address", type = "street")
sushi1_min

# postal codes
pm_postal_detect(sushi1_min)

pm_postal_any(sushi1_min)
pm_postal_all(sushi1_min)
pm_postal_none(sushi1_min)

sushi1_min <- pm_postal_parse(sushi1_min)
sushi1_min

# states
moDict <- pm_dictionary(locale = "us", type = "state", filter = "MO", case = "title")
moDict

pm_state_detect(sushi1_min, dictionary = moDict)

pm_state_any(sushi1_min, dictionary = moDict)
pm_state_all(sushi1_min, dictionary = moDict)
pm_state_none(sushi1_min, dictionary = moDict)

moDict <- pm_dictionary(locale = "us", type = "state", filter = "MO", case = c("title", "upper"))
moDict

pm_state_none(sushi1_min, dictionary = moDict)

sushi1_min <- pm_state_parse(sushi1_min, dictionary = moDict)
sushi1_min

# cities
cityDict <- pm_append(type = "city",
                      input = c("Brentwood", "Clayton", "Maplewood", "St. Louis", "Webster Groves"))

pm_city_detect(sushi1_min, dictionary = cityDict)

pm_city_any(sushi1_min, dictionary = cityDict)
pm_city_all(sushi1_min, dictionary = cityDict)
pm_city_none(sushi1_min, dictionary = cityDict)

cityDict <- pm_append(type = "city",
                      input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood", "St. Louis", "SAINT LOUIS", "Webster Groves"))

pm_city_none(sushi1_min, dictionary = cityDict)

cityDict <- pm_append(type = "city",
                      input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood", "St. Louis", "SAINT LOUIS", "Webster Groves"),
                      output = c(NA, NA, "Clayton", NA, NA, "St. Louis", NA))

sushi1_min <- pm_city_parse(sushi1_min, dictionary = cityDict)
sushi1_min

# house
pm_house_detect(sushi1_min)

pm_house_any(sushi1_min)
pm_house_all(sushi1_min)
pm_house_none(sushi1_min)

sushi1_min <- pm_house_parse(sushi1_min)
sushi1_min

# street directionals
pm_streetDir_detect(sushi1_min)

pm_streetDir_any(sushi1_min)
pm_streetDir_all(sushi1_min)
pm_streetDir_none(sushi1_min)

sushi1_min <- pm_streetDir_parse(sushi1_min)
sushi1_min

# street suffix
pm_streetSuf_detect(sushi1_min)

pm_streetSuf_any(sushi1_min)
pm_streetSuf_all(sushi1_min)
pm_streetSuf_none(sushi1_min)

sushi1_min <- pm_streetSuf_parse(sushi1_min)
sushi1_min

# street name
sushi1_min <- pm_street_parse(sushi1_min, ordinal = FALSE, drop = TRUE)

# replace
sushi1_parsed <- pm_replace(sushi1_min, source = sushi1_full)
sushi1_parsed_a <- pm_rebuild(sushi1_parsed, output = "full", keep_parsed = "no")
sushi1_parsed_b <- pm_rebuild(sushi1_parsed, output = "short", keep_parsed = "no")
sushi1_parsed_c <- pm_rebuild(sushi1_parsed, output = "short", keep_parsed = "limited")
sushi1_parsed_d <- pm_rebuild(sushi1_parsed, output = "short", keep_parsed = "yes")

# high level
pm_parse(sushi1_full, input = "full", address = address, output = "full", keep_parsed = "no",
         city_dict = cityDict, state_dict = moDict)
