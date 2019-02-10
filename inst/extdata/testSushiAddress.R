devtools::load_all()

sushi2 <- pm_identify(postmastr::sushi2, var = address)
sushi2 <- pm_prep(sushi2, var = "address")

pm_has_house(sushi2)

pm_any_house(sushi2)

pm_all_house(sushi2)

pm_no_house(sushi2)
