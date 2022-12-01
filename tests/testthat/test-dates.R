test_that("Guess date formats", {

  #library(lubridate)

  #sample <- "June 24th 2010"
  #expect_error(guess_date_fmt(sample, locale = "ru_XX")) # Falla
  #expect_equivalent(guess_date_fmt(sample),"%B %dth %Y") # Falla

  sample <- "24 janeiro 2010"
  expect_equivalent(guess_date_fmt(sample, locale = "pt-BR"),"%d %B %Y")

  sample <- "24 de enero de 2010"
  expect_equivalent(guess_date_fmt(sample, locale = "es-CO"), "%d de %B de %Y")

  #sample <- "Abr 4"
  #expect_equivalent(guess_date_fmt(sample), "%b %d") # Falla
  #expect_equivalent(guess_date_fmt(sample, locale = "es-CO"), "%b %d") # Falla

  expect_equal(guess_date_locale("Junio 4 de 2011"),"es-ES")
  # expect_equal(guess_date_locale("Ago 3"),"es-ES")

})

test_that("Format dates",{

  if(Sys.info()[['sysname']] == "Linux"){
    Sys.setlocale("LC_TIME", "en_US.UTF-8")
  }
  Sys.getlocale("LC_TIME")

  format_date(as.Date("2001-03-31"), "%Y__%m>>%d")

  format_date(as.Date("2020-03-21"), "%B %d", locale = "pt-BR")
  format_date(as.Date("2020-03-21"), "%B %d", locale = "de-DE")
  format_date(as.Date("2020-03-21"), "%B %d", locale = "ru-RU")

  v <- as.Date(c("2020-03-04","2019-10-21"))
  date_fmt <- "%Y %B !!!"
  locale <- "ru-RU"
  x <- format_date(v, date_fmt = date_fmt)
  x
  format_date(as.Date("2020-03-21"), "%B %d", locale = "ru-RU")

  expect_equal(rename_months(c("10 March", "September!!"), "ru-RU", "longMonths"),
               c("10 марта", "сентября!!"))
  expect_equal(rename_months(c("10 Apr", "Sep!!"), "es-ES", "shortMonths"),
               c("10 abr", "sep!!"))

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
  #expect_equal(makeup_dat(v, sample = "Janeiro 4"), "Março 04") # Falla
  # Guess locale from month name ES
  expect_equal(makeup_dat(v, sample = "Junio 4 2011"), "Marzo 04 2020")
  # Guess locale from month name
  expect_equal(makeup_dat(v, sample = "4 de junio de 2011"), "04 de marzo de 2020")
  ## TODO keep consistency with caps in Months
  ## TODO remove leading 0 for single digit days and months

  # expect_equal(makeup_dat(v, sample = "Ene 3", locale = "es-CO"), "Mar 4")
  # expect_equal(makeup_dat(v, sample = "Ene 3", locale = "es-CO"), "Mar 04") # Falla



  # sample = NULL
  # format = NULL
  expect_equal(makeup_dat(v, locale = "es-CO"), "04/03/2020")
  expect_equal(makeup_dat(v, locale = "es-MX"), "04/03/2020")
  expect_equal(makeup_dat(v, locale = "es-PE"), "04/03/2020")
  expect_equal(makeup_dat(v, locale = "de-DE"), "04.03.2020")
  # TODO trim single-digit
  expect_equal(makeup_dat(v, locale = "es-US"), "04/03/2020")

  expect_equal(makeup_dat(v, locale = "es-CO", format = "%B %d %Y"), "marzo 04 2020")
  #expect_equal(makeup_dat(v, locale = "de-DE", format = "%B %d %Y"), "März 04 2020") # Falla
  # expect_equal(makeup_dat(v, locale = "ru-RU", format = "%B %d %Y"), "марта 04 2020")

})
