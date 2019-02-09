devtools::load_all()

# prep
sushi1 <- pm_identify(postmastr::sushi1, var = "address")

sushi1_min <- pm_prep(sushi1, var = "address")
sushi1_min

# postal codes
pm_has_postal(sushi1_min)
pm_has_postal(sushi1_min, scalar = FALSE)

sushi1_min <- pm_parse_postal(sushi1_min)
sushi1_min

# states
moDict <- pm_dictionary(locale = "us", type = "state", filter = "MO")

pm_has_state(sushi1_min, dictionary = moDict)
pm_has_state(sushi1_min, dictionary = moDict, scalar = FALSE)

sushi1_min <- pm_parse_state(sushi1_min, dictionary = moDict)
sushi1_min

# cities
cityDict <- pm_append(type = "city",
                      input = c("Brentwood", "Clayton", "Maplewood", "St. Louis", "Webster Groves"))

pm_has_city(sushi1_min, dictionary = cityDict)

pm_no_city(sushi1_min, dictionary = cityDict)

cityDict <- pm_append(type = "city",
                      input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood", "St. Louis", "SAINT LOUIS", "Webster Groves"))

pm_no_city(sushi1_min, dictionary = cityDict)

cityDict <- pm_append(type = "city",
                      input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood", "St. Louis", "SAINT LOUIS", "Webster Groves"),
                      output = c(NA, NA, "Clayton", NA, NA, "St. Louis", NA))


sushi1_min <- pm_parse_city(sushi1_min, dictionary = cityDict)
sushi1_min

