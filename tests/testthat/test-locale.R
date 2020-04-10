test_that("Fallbacks work", {

  # Fallbacks

  expect_equal(which_locale_fallback("es-CO"), "es-MX")
  expect_null(which_locale_fallback("es-MX"))
  expect_equal(which_locale_fallback("pt-PT"), "pt-BR")
  expect_null(which_locale_fallback("es-ES"))


  # Fallback works
  expect_false("es-CO" %in% makeup::available_locales)
  expect_equal(get_locale("es-CO"),  get_locale("es-MX"))


  # Partial fallback works
  # Bolivia exists but doesn't have the date local info
  # Should auto-complete
  locale <- "es-BO"
  expect_true("es-BO" %in% makeup::available_locales)
  bo <- get_locale("es-BO")
  expect_null(makeup::locales[["es-BO"]]$date)
  expect_equal(bo$currency, c("Bs\u00a0", ""))
  mx <- get_locale("es-MX")
  expect_equal(bo$dateTime, mx$dateTime)


  expect_equal(get_locale("es-ES"),removeNulls(makeup::locales[["es-ES"]]))
  expect_equal(get_locale("es-MX"),removeNulls(makeup::locales[["es-MX"]]))

  # Without fallback
  # OJO... need to removeNulls
  expect_equal(get_locale("zh-CN"), removeNulls(makeup::locales[["zh-CN"]]))

  expect_equal(get_locale("en-US"), removeNulls(makeup::locales[["en-US"]]))

})
