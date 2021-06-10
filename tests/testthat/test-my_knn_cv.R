# load data for test 

data("my_penguins")

# manipulate data for test
my_penguins <- na.omit(my_penguins)
my_penguins_meas <- my_penguins %>% 
  dplyr::select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

test_that("produces expected output", {
  expect_type(my_knn_cv(train = my_penguins_meas, cl = my_penguins$species, k_nn = 1, k_cv = 5), "list")
})

