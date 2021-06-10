data(mtcars)
test_that("returns output of correct type", {
  expect_type(my_lm(formula = mpg ~ hp + wt, data = mtcars), "double")
})
