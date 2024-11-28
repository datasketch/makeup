test_that("makeup month works", {

  x <- rev(seq.Date(as.Date("2023-01-24"), as.Date("2023-12-24"), by = "month"))

  expect_equal(
    makeup_month(x),
    rev(lubridate::month(1:12, label = TRUE, abbr = FALSE))
  )
})
