test_data <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("225 1st St St. Louis, MO 63108",
                "486 First St St. Louis, MO 63110",
                "4256-4258 MLK Boulevard St. Louis, MO",
                "181-191 RED FOX Blvd St. Louis, MO",
                "5768 grand Blvd St. Louis, MO 63111",
                "245 SECOND St St. Francis, MO 63110-1234"),
  stringsAsFactors = FALSE
)

test_data %>%
  pm_parseZip(var = streetStr) %>%
  pm_parseState(var = pm.address) -> result

dir <- c("St. Louis", "Saint Louis", "St. Francis")

pm_isCity(result, var = pm.address, dictionary = dir)

pm_parseCity(result, var = pm.address, dictionary = dir)
