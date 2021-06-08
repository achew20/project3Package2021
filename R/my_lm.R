#' Linear Model Function 
#' 
#' Fits a linear model to a data frame. 
#' 
#' @param formula Input of formula class. 
#' @param data Data frame of numeric values.  
#' @keywords inference 
#' 
#' @return Table where coefficients are represented by a row, and columns are the estimate (numeric), 
#'    standard error (numeric), t-value (numeric), and the p-value of the t-test (numeric). 
#' 
#' @examples 
#' my_lm(formula = x ~ y, data = mydata)
#' my_lm(formula = x + z ~ y, data = dataset)
#' 
#' @export

# Function: my_lm, performs linear regression, outputs a summary of linear 
#     model
# Input: formula(e.g. y ~ x + y), data
# Output: table with rows for each coefficient and columns with 'Estimate', 
#     'Std. Error', 't value' and 'Pr(>\t\)'
my_lm <- function(formula, data) {
  frame <- model.frame(formula, data)
  x_matrix <- model.matrix(formula, data)
  y_matrix <- model.response(frame)
  # solve for beta hat
  beta_hat <- solve(t(x_matrix) %*% x_matrix) %*% t(x_matrix) %*% y_matrix
  # solve for degrees of freedom 
  df <- nrow(x_matrix) - ncol(x_matrix)
  # solve for residual se 
  sigma_hat_sqr <- sum(((y_matrix - (x_matrix %*% beta_hat))^2) / df)
  # solve for beta standard error 
  standard_err <- sqrt(diag(solve(t(x_matrix) %*% x_matrix)) * sigma_hat_sqr)
  # solve for t-value of betas 
  test_stat <- (beta_hat - 0) / standard_err
  # calculate Pr(>\t\)
  p_val <- 2 * pt(-abs(test_stat), df, lower.tail = TRUE)
  # place results in data frame
  results <- data.frame(
    "Estimate" = beta_hat,
    "Std. Error" = standard_err,
    "t value" = test_stat,
    "Pr(>|t|)" = p_val,
    check.names = FALSE
  )
  # print data frame as a matrix
  as.matrix(results)
}