# generate random simulation with 100 values
set.seed(302)
x <- rnorm(100, mean = 0, sd = 1)

test_that("performs proper t-test", {
  expect_type(is.list(my_t.test(x, alternative = "less", mu = 0)), length(my_t.test(x, alternative = "less", mu = 0)) == 4)
})

