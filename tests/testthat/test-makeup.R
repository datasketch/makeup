# test_that("makeup", {
#
#   x <- c("2020-03-05","2020-06-20")
#   makeup(as.Date(x))
#
#   x <- c(1234.56, 432141, 0.12)
#   makeup(x, sample = "1'432.1")
#   makeup(x, sample = "1,432.1")
#   makeup(x, sample = "10k")
#   makeup(x, sample = "10%")
#
#   x <- c("hello", "WoRlD","Hello world")
#   makeup(x, sample = "down")
#   makeup(x, sample = "UPPER")
#   makeup(x, sample = "Title phrase")
#   makeup(x, sample = "Title Case")
#
#   f <- makeup_format(sample = "3 de abril 1900")
#   f(as.Date("2020-04-28"))
#   f <- makeup_format(sample = "abril 3 1900", type = "dat")
#   f("2020-04-28")
#
# })
