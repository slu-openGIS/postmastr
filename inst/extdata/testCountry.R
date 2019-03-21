postmastr::sushi1 %>%
  filter(name != "Drunken Fish - Ballpark Village") %>%
  mutate(address = stringr::str_c(address, "USA", sep = " ")) %>%
  pm_identify(var = address) %>%
  pm_prep(var = "address") %>%
  pm_country_trim()
