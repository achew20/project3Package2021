penguins <- project3Package2021::my_penguins
penguins_meas <- penguins %>% 
  dplyr::select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
penguins_meas <- na.omit(penguins_meas)

test_that("produces expected output", {
  expect_type(my_rf_cv(5), "double")
})