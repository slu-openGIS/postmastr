library(dplyr)
devtools::load_all()

intersections <- tibble(
  address = c("123 Main St", "4259 LINDELL BLVD", "Main St and 1st Ave", "North Tucker at Market",
              "128 Main St", "1111 Main St", "987 Main St", "4579 Tower Grove Ave")
)

intersections <- tibble(
  address = c("123 Main St", "4259 LINDELL BLVD",
              "128 Main St", "1111 Main St", "987 Main St", "4579 Tower Grove Ave")
)

intersections <- pm_identify(intersections, var = address)

pm_parse(intersections, input = "short", address = address, output = "short", keep_parsed = "no")

pm_evaluate(intersections)

pm_type_unknown(intersections)

pm_prep(intersections, var = address, type = "intersection") %>%
  pm_intersect_longer() %>%
  pm_streetDir_parse() %>%
  pm_streetSuf_parse() %>%
  pm_street_parse() %>%
  pm_intersect_wider() -> intersections_parsed

pm_prep(intersections, var = address, type = "street") %>%
  pm_house_parse() %>%
  pm_streetDir_parse() %>%
  pm_streetSuf_parse() %>%
  pm_street_parse() -> houses_parsed

intersections2 <- pm_replace(intersections, street = houses_parsed, intersect = intersections_parsed)

pm_rebuild(intersections2, output = "short", keep_parsed = "no", side = "right")

# =====================


intersections <- tibble(
  address = c("123 Main St St. Louis MO 63110", "4259 LINDELL BLVD SAINT LOUIS MO", "Main St and 1st Ave St Louis MO 63110", "North Tucker at Market St Louis MO 63110",
              "128 Main St St Louis MO 63110", "1111 Main St St Louis MO 63110", "987 Main St St. Louis MO 63110", "4579 Tower Grove Ave St Louis MO 63110")
)

intersections <- pm_identify(intersections, var = address)

pm_evaluate(intersections)

pm_type_unknown(intersections)

state_dict <- pm_dictionary(locale = "us", type = "state", filter = "MO", case = "title")
city_dict <- pm_append(type = "city",
                      input = c("St. Louis", "SAINT LOUIS", "St Louis"),
                      output = c("St. Louis", "St. Louis", "St. Louis"))

pm_prep(intersections, var = address, type = "intersection") %>%
  pm_intersect_longer() %>%
  pm_postal_parse() %>%
  pm_state_parse(dictionary = state_dict) %>%
  pm_city_parse(dictionary = city_dict) %>%
  pm_streetDir_parse() %>%
  pm_streetSuf_parse() %>%
  pm_street_parse() %>%
  pm_intersect_wider() -> intersections_parsed

pm_prep(intersections, var = address, type = "street") %>%
  pm_postal_parse() %>%
  pm_state_parse(dictionary = state_dict) %>%
  pm_city_parse(dictionary = city_dict) %>%
  pm_house_parse() %>%
  pm_streetDir_parse() %>%
  pm_streetSuf_parse() %>%
  pm_street_parse() -> houses_parsed

intersections2 <- pm_replace(intersections, street = houses_parsed, intersect = intersections_parsed)

pm_rebuild(intersections2, output = "full", keep_parsed = "no", side = "right", include_commas = TRUE)
