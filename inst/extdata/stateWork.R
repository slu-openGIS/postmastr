test_data <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("225 1st St St. Louis, New Jersey 63108",
                "486 First St St. Louis, MO 63110",
                "4256-4258 MLK Boulevard St. Louis, Missouri",
                "181-191 RED FOX Blvd St. Louis",
                "5768 grand Blvd St. Louis, MO 63111",
                "245 SECOND St St. Francis, MO 63110-1234"),
  stringsAsFactors = FALSE
)

result <- pm_parseZip(test_data, var = streetStr)

pm_isState(result, var = pm.address)

pm_parseState(result, var = pm.address)

test_data2 <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  stateStr = c("Missouri",
                "New York",
                "Vermont",
                "Colorado",
                "California",
                "Alaska"),
  stringsAsFactors = FALSE
)

pm_stdState(test_data2, var = stateStr)
