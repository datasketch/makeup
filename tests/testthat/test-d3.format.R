test_that("d3.format works with locale",{
  library(d3.format)
  custom_locale <- list(
    decimal = ",",
    thousands = "|"
  )
  x <- d3.format(",.9r", locale = custom_locale)(43243.342)
  expect_equal(number_separators(x),custom_locale)

})

test_that("d3.format examples",{
  # num
  d3.format(",.2r")(4223.04)
  d3.format(",.4")(4223.4324)
  d3.format(",d")(4223) #4,223 rounded to integer
  d3.format(".2f")(4223.4) #4,223 rounded to integer
  d3.format(".2f", locale = "es-MX")(4223)
  d3.format(".2f", locale = "es-ES")(4223)




  # pct
  d3.format(".0%")(0.123) # rounded percentage, "12%"
  d3.format(",%")(0.123) # rounded percentage, "12%"
  d3.format(",.1%")(0.55123456) #4,223 rounded to integer
  d3.format(",.3%")(0.55123456) #4,223 rounded to integer

  # big num
  d3.format(".2s")(1340500) # "1.5k"
  d3.format(",.2s")(1340500) # "1.5k"
  d3.format("s")(1500) # "1.50000k"
  d3.format("~s")(1500) # "1.5k"


  d3.format("($.2f")(-3.5) # localized fixed-point currency, "(£3.50)"
  d3.format("+20")(42) # space-filled and signed, "                 +42"
  d3.format(".^20")(42) # dot-filled and centered, ".........42........."
  d3.format(".2s")(42e6) # SI-prefix with two significant digits, "42M"
  d3.format("#x")(48879) # prefixed lowercase hexadecimal, "0xbeef"
  d3.format(",.2r")(4223) # grouped thousands with two significant digits, "4,200"

})
