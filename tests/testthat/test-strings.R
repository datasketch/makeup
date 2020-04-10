test_that("strings", {

  expect_true(is_lower("si si ohhh"))
  expect_false(is_lower("SUPER ohhh"))

  expect_true(is_upper("HELLO!"))
  expect_false(is_upper("SUPER ohhh"))

  expect_true(is_title("Hello!"))
  expect_true(is_title("Hello World"))
  expect_false(is_title("Super ohhh"))
  expect_equal(totitle("hello wORld"),"Hello World")

  expect_true(is_firstupper("Super ohhh"))
  expect_false(is_firstupper("hello World!"))
  expect_equal(tofirstupper("hello wORld"),"Hello world")

  expect_equal(match_caps("Hello WoRlD", "Hello World"),"Hello World")
  expect_equal(match_caps("hello woRlD", "Hello world"),"Hello world")

  expect_equal(makeup_chr("hello", sample = "UPS"), "HELLO")
  expect_equal(makeup_chr("WoRlD", sample = "down"), "world")
  expect_equal(makeup_chr("hello wOrld", sample = "Title Case"),"Hello World")
  expect_equal(makeup_chr("hello wOrld", sample = "First case"),"Hello world")

})
