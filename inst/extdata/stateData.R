# create state data

states <- as_tibble(data.frame(
  stateName = c(datasets::state.name),
  stateAbb = c(datasets::state.abb),
  stringsAsFactors = FALSE
))

usethis::use_data(states)
