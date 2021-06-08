#' t-Test Function
#' 
#' Performs one or two-sided t-tests on numerical data.
#' 
#' @param x Vector of numeric data (with no NA values) that t-test will be performed on. 
#' @param alternative String input of either: "greater", "less", or "two-sided". 
#' @param mu Numeric value that represents the true mean of the input data, \code{x}. 
#' @keywords inference 
#' 
#' @return List containing the test statistic (numeric), degrees of freedom (numeric), 
#'   initial input value of \code{alternative}, and the p-value (numeric). 
#' 
#' @examples 
#' my_t.test(x = data[1:100], alternative = "less", mu = 0)
#' my_t.test(x = mydata[5:200], alternative = "two-sided", mu = 1)
#' 
#' @export

# Function: my_t.test, performs t-test
# Input: x (must be numeric), alternative ("two-sided", "less", or "greater"), 
#   mu (must be numeric)
# Output: list with elements: 'test_stat', 'df', 'alternative', 'p_val'
my_t.test <- function(x, alternative = c("two-sided", "less", "greater"), mu) {
  
  # throws error for incorrect alternative input 
  if (alternative != "two-sided" & alternative != "less" & alternative != "greater") {
    stop("The alternative input must be: two-sided, less, or greater.")
  }
  
  # calculate sample size 
  n <- length(x)
  # calculate degress of freedom
  df <- (n - 1)
  # calculate test statistic 
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(n))
  # calculate p-value if alternative hypothesis is greater than null
  if (alternative == "greater") {
    p_val <- 1 - pt(test_stat, df, lower.tail = TRUE)
  }
  # calculate p-value if alternative hypothesis is less than null
  else if (alternative == "less") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  }
  # calculate p_value for two-sided hypothesis test 
  else if (alternative == "two-sided") {
    p_val <- 2 * pt(-abs(test_stat), df, lower.tail = TRUE)
  }
  # generate list of output 
  t_test_out <- list("test_stat" = test_stat,
                     "df" = df,
                     "alternative" = alternative,
                     "p_val" = p_val)
  # print list 
  t_test_out
}
