test_that("Format numbers", {

  library(d3.format)

  expect_true(is_pct("100%"))
  expect_equal(which_num_format("1%")$specifier, ".0%")
  # TODO pct with decimals
  expect_equal(makeup_num(0.1, "100%"),"10%")
  expect_equal(makeup_num(0.1111, "10%"),"11%")


  expect_true(is_si_num("40M"))
  expect_true(is_si_num("1.5k"))
  expect_true(is_si_num("40µ"))
  expect_equal(makeup_num(0.0001, "10k"),"100µ")
  expect_equal(makeup_num(10e-5, "10k"),"100µ")
  # OJO
  # expect_equal(makeup_num(1234.56, "10k"),"1.2k")


  expect_equal(number_separators("1234.56"),
               list(n_decimal = 2, decimal = ".", thousands = ""))
  expect_equal(number_separators("1'234,56"),
               list(n_decimal = 2, decimal = ",", thousands = "'"))
  expect_equal(number_separators("1234"),
               list(n_decimal = 0, decimal = "", thousands = ""))
  expect_equal(number_separators("1'000.234,00"),
               list(n_decimal = 2, decimal = ",", thousands = "."))
  # expect_equal(number_separators("1'000.234"),
  #              list(decimal = ",", thousands = "."))

  # Formats

  custom_locale <- get_locale("es-CO")[c("decimal", "thousands")]
  d3.format(",", locale = custom_locale)(1234.56)
  expect_equal(makeup_num(1234.56, locale = "es-CO"), "1,234.56")
  expect_equal(makeup_num(1234.56, locale = "es-ES"), "1.234,56")


  # sample <- "1,234."
  # expect_equal(makeup_num(1234.56, sample = sample), sample)

  sample <- "1 234.56"
  expect_equal(makeup_num(1234.56, sample = sample), sample)
  sample <- "1 234.56"
  expect_equal(makeup_num(1234.56, sample = sample), sample)
  sample <- "1234.56"
  expect_equal(makeup_num(1234.56, sample = sample), sample)
  sample <- "1'234,56"
  expect_equal(makeup_num(1234.56, sample = sample), sample)
  sample <- "1'234,56"
  expect_equal(makeup_num(1234.56, sample = sample, si_prefix = TRUE), "1.23K")
  # TODO implement millions separators
  # format <- "1'000,234.56"
  # expect_equal(makeup_num(1000234.56, format = format), format) # "1,000,234.56"

})
