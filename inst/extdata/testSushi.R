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
