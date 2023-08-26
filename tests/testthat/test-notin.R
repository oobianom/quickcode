test_that("Not in function", {
  expect_equal(5 %!in% c(1:6), FALSE)
})
