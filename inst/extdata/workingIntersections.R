library(dplyr)
devtools::load_all()

intersections <- tibble(
  address = c("123 Main St", "4259 LINDELL BLVD", "Main St and 1st Ave", "North Tucker at Market",
              "128 Main St", "1111 Main St", "987 Main St", "4579 Tower Grove Ave")
)

intersections <- pm_identify(intersections, var = address)

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

pm_replace(intersections, street = houses_parsed, intersect = intersections_parsed)
