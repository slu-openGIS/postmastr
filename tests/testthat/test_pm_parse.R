context("test pm_parse function")

# load test data ------------------------------------------------

# target data
data(sushi1, package = "postmastr")
data(sushi2, package = "postmastr")

# construct dictionaries
dirs <- pm_dictionary(type = "directional", filter = c("N", "S", "E", "W"), locale = "us")
sufs <- pm_dictionary(type = "suffix", locale = "us")
mo <- pm_dictionary(type = "state", filter = "MO", case = c("title", "upper"), locale = "us")
cities <- pm_append(type = "city",
   input = c("Brentwood", "Clayton", "CLAYTON", "Maplewood", "St. Louis",
             "SAINT LOUIS", "Webster Groves"),
   output = c(NA, NA, "Clayton", NA, NA, "St. Louis", NA))

# temporary code to subset unit
sushi1 <- dplyr::filter(sushi1, name != "Drunken Fish - Ballpark Village")
sushi2 <- dplyr::filter(sushi2, name != "Drunken Fish - Ballpark Village")

# test errors ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(pm_parse(sushi2, input = "short", address = "address",
                        dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo,
                        locale = "fr"),
               "At this time, the only locale supported is 'us'. This argument is included to facilitate further expansion.")
})

# test inputs ------------------------------------------------

test_that("correctly specified functions execute without error", {
  expect_error(pm_parse(sushi1, input = "full", address = "address", output = "full",
                        dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo), NA)
  expect_error(pm_parse(sushi1, input = "full", address = address, output = "full",
                        dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo), NA)
  expect_error(pm_parse(sushi1, input = "full", address = address, output = "short", new_address = "clean_address",
                        dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo), NA)
  expect_error(pm_parse(sushi1, input = "full", address = address, output = "short", new_address = clean_address,
                        dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo), NA)
  expect_error(pm_parse(sushi1, input = "full", address = address, output = "short", keep_parsed = "limited",
                        dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo), NA)
})

test_that("correctly specified functions execute without error", {
  expect_error(pm_parse(sushi2, input = "short", address = "address",
                        dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo), NA)
  expect_error(pm_parse(sushi2, input = "short", address = "address", keep_parsed = TRUE,
                        dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo), NA)
  expect_error(pm_parse(sushi2, input = "short", address = "address",
                        dir_dict = dirs, suffix_dict = sufs, city_dict = cities, state_dict = mo), NA)
})

test_that("correctly specified functions execute without error", {
  expect_error(pm_parse(sushi2, input = "short", address = "address",
                        dir_dict = dirs, city_dict = cities, state_dict = mo), NA)
})

