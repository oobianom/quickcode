test_that("Generate one random string", {
  expect_equal(length(randString(1,5)),1)
})

test_that("Generate one random string with 5 characters", {
  expect_equal(length(randString(5,5)),5)
})


test_that("Generate five random string", {
  expect_equal(nchar(randString(1,5)),5)
})


test_that("Generate five random string and 10 characters each", {
  expect_equal(nchar(randString(5,10)),rep(10,5))
})
