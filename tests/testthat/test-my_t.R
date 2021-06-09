# generate random simulation with 100 values
set.seed(302)
x <- rnorm(100, mean = 0, sd = 1)

test_that("returns output of correct type", {
  expect_type(my_t.test(x, alternative = "less", mu = 0), "list")
})

test_that("throws error if wrong input for alternative", {
  expect_error(my_t.test(x, alternative = "lss", mu = 0))
})

