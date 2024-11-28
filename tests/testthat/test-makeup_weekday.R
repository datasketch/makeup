test_that("makeup weekday works", {

  x <- seq.Date(as.Date("2024-11-27"), as.Date("2024-12-04"), by = "day")
  # 2024-11-27 wednesday
  expect_equal(
    makeup_weekday(x),
    lubridate::wday(3:10 %% 7 + 1, label = TRUE, abbr = FALSE)
  )

  expect_equal(as.character(makeup_weekday(x, "es")[1]), "miércoles")
  expect_equal(as.character(makeup_weekday(x, "de")[1]), "Mittwoch")
  expect_equal(as.character(makeup_weekday(x, "es-CO")[1]), "miércoles")

})
