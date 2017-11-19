test_data <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("225 1st St", "225 1ST St", "4256-4258 MLK Boulevard", "181-191 RED FOX Blvd", "5768 grand Blvd", "245 SECOND St"),
  stringsAsFactors = FALSE
)

test_data2 <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("225 1st St", "225 Rear 1ST St", "4256 MLK Boulevard", "181 RED FOX Blvd", "5768 grand Blvd", "245 SECOND St"),
  stringsAsFactors = FALSE
)

test_data3 <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("225R 1st St", "225 1ST St", "4256 MLK Boulevard", "181F RED FOX Blvd", "5768 grand Blvd", "245 SECOND St"),
  stringsAsFactors = FALSE
)

test_data4 <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("225R 1st St NW", "225 1ST St North", "4256 MLK Boulevard Southwest", "181F RED FOX Blvd SW", "5768 grand Blvd E", "245 SECOND St"),
  stringsAsFactors = FALSE
)

test_data5 <- data.frame(
  id = c(1, 2, 3, 4, 5, 6),
  streetStr = c("225R NW 1st St", "225 North 1ST St", "4256 Southwest MLK Boulevard", "181F SW RED FOX Blvd", "5768 E. grand Blvd", "245 SECOND St"),
  stringsAsFactors = FALSE
)


