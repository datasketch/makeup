test_that("utils", {

  x <- ''
  expect_true(is.empty(x))
  x <- character(0)
  expect_true(is.empty(x))
  x <- integer(0)
  expect_true(is.empty(x))
  x <- NULL
  expect_true(is.empty(x))
})
