# Create U.S. State Dictionary

territory.abb <- c("AS", "DC", "FM", "GU", "MH", "MP", "PW", "PR", "VI", "AE", "AP", "AA")
territory.name <- c("American Samoa", "District of Columbia", "Federated States of Micronesia", "Guam",
                    "Marshall Islands", "Northern Mariana Islands", "Palau", "Puerto Rico", "Virgin Islands",
                    "Armed Forces Europe, the Middle East, and Canada", "Armed Forces Pacific",
                    "Armed Forces Americas")

dic_us_states <- data.frame(
  state.output = c(state.abb, state.abb, territory.abb, territory.abb),
  state.input = c(state.abb, state.name,  territory.abb, territory.name),
  stringsAsFactors = FALSE
)

dic_us_states <- dic_us_states[order(dic_us_states$state.output),]
dic_us_states <- dplyr::as_tibble(dic_us_states)

usethis::use_data(dic_us_states, overwrite = TRUE)
