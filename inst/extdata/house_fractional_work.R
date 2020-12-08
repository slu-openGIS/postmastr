devtools::load_all()

sushi2 %>%
  pm_identify(var = address) %>%
  filter(pm.uid %in% c(3:4) == FALSE) -> sushi_test

sushi_test <- dplyr::mutate(sushi_test, address = ifelse(pm.uid == 1, "3407-09 Olive St", address))

pm_parse(sushi_test, input = "short", address = address,
         output = "short", new_address = address_clean,
         houseSuf_dict = gateway::stl_std_houseSuffix,
         dir_dict = gateway::stl_std_directions,
         street_dict = gateway::stl_std_streets,
         suffix_dict = gateway::stl_std_suffix,
         unnest = TRUE)

sushi_test %>%
  pm_prep(var = "address", type = "short") %>%
  pm_house_parse() %>%
  pm_houseRange_parse() %>%
  pm_houseFrac_parse() %>%
  pm_houseSuf_parse(dictionary = gateway::stl_std_houseSuffix) %>%
  pm_streetDir_parse(dictionary = gateway::stl_std_directions) %>%
  pm_streetSuf_parse(dictionary = gateway::stl_std_suffix) %>%
  pm_street_parse(dictionary = gateway::stl_std_streets, ordinal = TRUE) -> out

out %>%
  dplyr::select(-dplyr::starts_with("pm.has")) %>%
  dplyr::select_if(function(x) !(all(is.na(x)))) -> out2

out2 <- dplyr::left_join(sushi_test, out2, by = "pm.uid")

out2 %>%
  tidyr::unnest(cols = "pm.houseRange")


pm_replace(sushi_test, street = out, unnest = TRUE)
