#' Random Forest Cross-Validation Function
#' 
#' Predicts an output class of a given training data using the Random Forest
#'    statistical prediction method and cross-validation. 
#' 
#' @param k Numeric value representing the number of folds for cross-validation.
#' @keywords prediction 
#' 
#' @return Numeric value representing the cross-validation error. 
#' 
#' @examples 
#' my_rf_cv(5)
#' 
#' @export

# Function: my_rf_cv, predicts output with random forest cv
# Input: k (must be numeric)
# Output: mse (mean standard error)
my_rf_cv <- function(k) {
  penguins <- project3Package2021::my_penguins
  penguins_meas <- penguins %>% 
    dplyr::select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
  penguins_meas <- na.omit(penguins_meas)
  
  n <- nrow(penguins_meas)
  # randomly assign observations to a fold 
  fold <- sample(rep(1:k, length = n)) 
  # create data frame for penguin data with output and covariates 
  data_pen <- data.frame("y" = penguins_meas$body_mass_g, 
                         "x" = penguins_meas$bill_length_mm, 
                         "z" = penguins_meas$bill_depth_mm, 
                         "c" = penguins_meas$flipper_length_mm, 
                         "split" = fold)
  
  #create empty vector for mse 
  mse_list <- rep(NA, k)
  
  # iterate through trainin and test data 
  for (i in 1:k) {
    # establish training data as everything but fold 
    data_train <- data_pen %>% dplyr::filter(split != i) 
    data_test <- data_pen %>% dplyr::filter(split == i)
    
    # train random forest model 
    rf_model <- randomForest::randomForest(y ~ x + z + c, data = 
                               data_train, ntree = 100)
    
    # predict 
    prediction <- predict(rf_model, data_test[, -c(1,5)])
    
    # calculate mse 
    mse <- mean((prediction - data_test$y)^2)
  }
  return(mse)
}