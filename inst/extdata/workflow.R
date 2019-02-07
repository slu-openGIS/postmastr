test_data <- data.frame(
  id = c(1, 2, 3, 4, 5, 6, 7),
  streetStr = c("225 1st St St. Louis, MO 63108",
                "486 First St St. Louis, MO 63110",
                "486 First St St. Louis, MO 63110",
                "4256-4258 MLK Boulevard St. Louis, MO",
                "181-191 RED FOX Blvd St. Louis, MO",
                "5768 grand Blvd St. Louis, MO 63111",
                "245 SECOND St St. Francis, 63110-1234"),
  stringsAsFactors = FALSE
)

devtools::load_all()

test_data_id <- pm_identify(test_data, var = streetStr)

working_data <- pm_prep(test_data_id, var = streetStr)

pm_has_uid(working_data)
pm_has_address(working_data)

pm_has_postal(working_data)
pm_has_postal(working_data, scalar = FALSE)

(working_data <- pm_parse_postal(working_data))

pm_has_state(working_data)
pm_has_state(working_data, scalar = FALSE)

pm_parse_state(working_data)
