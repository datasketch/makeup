test_that("Guess date formats", {

  library(lubridate)

  sample <- "June 24th 2010"
  expect_equivalent(guess_date_fmt(sample),"%B %dth %Y")
  sample <- "24 janeiro 2010"
  expect_equivalent(guess_date_fmt(sample, locale = "pt-BR"),"%d %B %Y")
  sample <- "24 de enero de 2010"
  expect_equivalent(guess_date_fmt(sample, locale = "es-CO"), "%d de %B de %Y")

  expect_equal(guess_date_locale("Junio 4 de 2011"),"es-ES")

})

test_that("Format datse",{

  format_date(as.Date("2001-03-31"), "%Y__%m>>%d")

})


test_that("dates", {

  locale <- "es-CO"
  get_locale(locale)$date

  v <- as.Date("2020-03-04")
  # sample = NULL
  # format = NULL
  # locale = NULL
  expect_equal(makeup_dat(v), "3/4/2020") # default to US locale

  # format = NULL
  # locale = NULL
  expect_equal(makeup_dat(v, sample = "2000-12-31"),as.character(v))
  # Guess locale from month name PT
  expect_equal(makeup_dat(v, sample = "Janeiro 4"), "Março 04")
  # Guess locale from month name ES
  expect_equal(makeup_dat(v, sample = "Junio 4 2011"), "marzo 04 2020")
  # Guess locale from month name
  expect_equal(makeup_dat(v, sample = "4 de junio de 2011"), "04 de marzo de 2020")
  ## TODO keep consistency with caps in Months
  ## TODO remove leading 0 for single digit days and months

  # sample = NULL
  # format = NULL
  expect_equal(makeup_dat(v, locale = "es-CO"), "04/03/2020")
  expect_equal(makeup_dat(v, locale = "es-MX"), "04/03/2020")
  expect_equal(makeup_dat(v, locale = "es-PE"), "04/03/2020")
  expect_equal(makeup_dat(v, locale = "de-DE"), "04.03.2020")
  # TODO trim single-digit
  expect_equal(makeup_dat(v, locale = "es-US"), "04/03/2020")





})
