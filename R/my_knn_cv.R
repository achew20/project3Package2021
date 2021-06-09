#' k-Nearest Neighbors Cross-Validation Function
#' 
#' Predicts an output class of a given training data using the k-Nearest Neighbors non-parametric 
#'    statistical prediction method and cross-validation. 
#' 
#' @param train Data frame of numeric and string values intended as training data (with no NA values). 
#' @param cl The true class value of the training data, \code{train} (with no NA values). 
#' @param k_nn Numeric value representing the number of neighbors. 
#' @param k_cv Numeric value representing the number of folds for cross-validation.
#' @keywords prediction 
#' 
#' @return List containing predicted class (y-hat) for all observations in \code{train}, and a numeric
#'   value representing the cross-validation misclassification error. 
#' 
#' @examples 
#' my_knn_cv(train = my_penguins[-c(1, 2)], cl = my_penguins$species, k_nn = 1, k_cv = 5)
#' 
#' @export

# Function: my_knn_cv, predicts class with k-nearest neighbors 
# Input: train (data frame), cl (true class value), 
#   k_nn (must be numeric), k_cv (must be numeric)
# Output: list with elements: 'predicted class', 'cv_err'
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  train <- na.omit(train)
  # randomly assigns observations to folds 
  fold <- sample(rep(1:k_cv, length = nrow(train))) 
  
  # create empty vectors for cv error and class lists
  cv_err <- rep(NA, k_cv)
  class_list <- rep(NA, nrow(train))
  
  # iterate through training and test data
  for (i in 1:k_cv) {
    x_train <- train[fold != i,]
    x_test <- train[fold == i,]
    y_train <- cl[fold != i]
    y_test <- cl[fold ==i]
    
    # perform knn predictions 
    knn_predict <- (class::knn(train = x_train,
                        test = x_test,
                        cl = y_train,
                        k = k_nn 
    ))
    
    # record and calculate cv error 
    cv_err[i] <- mean(knn_predict != y_test)
  }
  
  full_knn <- class::knn(train = train, 
                  test = train,
                  cl = cl,
                  k = k_nn)
  
  output_dataframe <- list("Predicted CLass" = full_knn,
                           "Misclassification Error" = mean(cv_err))
  return(output_dataframe)
}

