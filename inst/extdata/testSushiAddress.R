devtools::load_all()

sushi2 <- pm_identify(postmastr::sushi2, var = address)
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
