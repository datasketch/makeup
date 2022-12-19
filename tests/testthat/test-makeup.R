 test_that("makeup dates", {

   ### Fechas
   # x <- c("2020-03-05","2020-06-20")
   # makeup(as.Date(x))

   #locale <- "es-CO"

   v = as.Date("2020-03-04")
   expect_equal(makeup(v, sample = "3/4/2020"), "03/04/2020")

   expect_equal(makeup(v, sample = "2000-12-31"),as.character(v))
   # Guess locale from month name PT
   #expect_equal(makeup_dat(v, sample = "Janeiro 4"), "Março 04") # Fails
   # Guess locale from month name ES
   expect_equal(makeup_dat(v, sample = "Junio 4 2011"), "Marzo 04 2020")
   # Guess locale from month name
   expect_equal(makeup_dat(v, sample = "4 de junio de 2011"), "04 de marzo de 2020")

   # v <- as.Date("03-04-2020")
   # makeup(v, locale = "es-MX") # FAILS

})

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

