test_that("Guess date formats", {

  library(lubridate)

  format <- "June 24th 2010"
  expect_equivalent(guess_date_format(format),"%B %dth %Y")
  format <- "24 janeiro 2010"
  expect_equivalent(guess_date_format(format, locale = "pt-BR"),"%d %B %Y")
  format <- "24 de enero de 2010"
  expect_equivalent(guess_date_format(format, locale = "es-CO"), "%d de %B de %Y")

  expect_equal(guess_date_locale("Junio 4 de 2011"),"es-ES")

})

test_that("Format datse",{

  format_date(as.Date("2001-03-31"), "%Y__%m>>%d")

})


test_that("dates", {


  locale <- "es-CO"
  get_locale(locale)$date

  v <- as.Date("2020-03-04")
  expect_equal(makeup_dat(v),"3/4/2020") # default US locale
  expect_equal(makeup_dat(v, format = "2000-12-31"),as.character(v))

  # Guess locale from month name PT
  expect_equal(makeup_dat(v, format = "Janeiro 4"), "Março 04")
  # Guess locale from month name ES
  expect_equal(makeup_dat(v, format = "Junio 4 2011"), "marzo 04 2020")
  # Guess locale from month name
  expect_equal(makeup_dat(v, format = "4 de junio de 2011"), "04 de marzo de 2020")
  ## TODO keep consistency with caps in Months
  ## TODO remove leading 0 for single digit days and months



})
