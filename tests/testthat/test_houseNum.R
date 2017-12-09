context("test house number parsing")

# test results ------------------------------------------------
## simple addresses -------------------------------------------

testData <- data.frame(
  id = c(1, 2, 3),
  streetStr = c("225 1st St", "486 First St", "4256 MLK Boulevard"),
  stringsAsFactors = FALSE
)

resultData <- data.frame(
  id = c(1, 2, 3),
  streetStr = c("225 1st St", "486 First St", "4256 MLK Boulevard"),
  houseNum = c("225", "486", "4256"),
  stringsAsFactors = FALSE
)

testData <- pm_houseNum(testData, streetStr)

test_that("house number is correctly parsed", {
  expect_equal(testData$houseNum, resultData$houseNum)
})
