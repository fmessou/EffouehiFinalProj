# Constructor function
#' Perform a t-test or Welch's test on two samples
#'
#' This function performs a t-test or Welch's test on two samples to test the NULL hypothesis
#' that the underlying means of the two populations are the same. It takes into consideration
#' the variances and whether the samples are paired or not.
#'
#' @param x A numeric vector representing the first sample
#' @param y A numeric vector representing the second sample
#' @param paired A logical value indicating whether the samples are paired (default: FALSE)
#' @param alpha The significance level for hypothesis testing (default: 0.05)
#'
#' @return A list with the following components:
#' \itemize{
#'   \item test_type: A character string indicating the test performed ("T-test", "Welch", or "Paired")
#'   \item reject_null: A logical value indicating whether the NULL hypothesis should be rejected
#'   \item summary: A summary object containing the results of the appropriate t.test function
#'   \item data: A list containing the input samples (x and y)
#' }
#' @export
#'
#' @examples
#' set.seed(32)
#' x <- rnorm(30, mean = 10, sd = 15)
#' y <- rnorm(30, mean = 8, sd = 15)
#' ans1 <- myttest(x, y, alpha = 0.05, paired = FALSE)
#' print(ans1)
#' plot(ans1)
#'
myttest <- function(x, y, paired = FALSE, alpha = 0.05) {
  # Check for evidence of different variances
  f_test <- var.test(x, y)
  var_equal <- f_test$p.value > alpha

  # Perform the appropriate t-test
  if (paired) {
    t_test <- t.test(x, y, paired = TRUE)
    test_type <- "Paired"
  } else {
    t_test <- t.test(x, y, var.equal = var_equal)
    test_type <- ifelse(var_equal, "T-test", "Welch")
  }

  # Conclusion
  reject_null <- t_test$p.value <= alpha

  # Create the result list
  result <- list(
    test_type = test_type,
    reject_null = reject_null,
    summary = t_test,
    data = list(x = x, y = y)
  )

  class(result) <- "Rttest"
  invisible(result)
}

# S3 method for print.Rttest
#' Print method for Rttest objects
#'
#' This function prints the results of an Rttest object in a human-readable format,
#' displaying the test type, whether the NULL hypothesis should be rejected, and
#' the confidence interval for the difference of means.
#'
#' @param x An object of class "Rttest" returned by the myttest function
#' @param ... Additional arguments passed to the print function (currently unused)
#'
#' @return NULL
#' @export
#'
#' @examples
#' set.seed(32)
#' x <- rnorm(30, mean = 10, sd = 15)
#' y <- rnorm(30, mean = 8, sd = 15)
#' ans1 <- myttest(x, y, alpha = 0.05, paired = FALSE)
#' print(ans1)
print.Rttest <- function(x, ...) {
  cat("Test Type:", x$test_type, "\n")
  cat("Reject Null Hypothesis:", ifelse(x$reject_null, "Yes", "No"), "\n")
  cat("Confidence Interval for the difference of means:\n")
  print(x$summary$conf.int)
}


# S3 method for plot.Rttest
#' Plot method for Rttest objects
#'
#' This function creates boxplots for non-paired data or boxplot of differences for paired data,
#' and includes the confidence interval for the difference of means when plotting.
#'
#' @param x An object of class "Rttest" returned by the myttest function
#' @param ... Additional arguments passed to the plot function (currently unused)
#'
#' @return NULL
#' @export
#'
#' @examples
#' set.seed(32)
#' x <- rnorm(30, mean = 10, sd = 15)
#' y <- rnorm(30, mean = 8, sd = 15)
#' ans1 <- myttest(x, y, alpha = 0.05, paired = FALSE)
#' plot(ans1)
plot.Rttest <- function(x, ...) {
  if (x$test_type == "Paired") {
    diff_data <- x$data$x - x$data$y
    boxplot(diff_data, main = "Boxplot of Differences (Paired Data)",
            xlab = "Differences", ylab = "Value")
    abline(h = x$summary$conf.int, col = "red", lwd = 2)
  } else {
    boxplot(x$data$x, x$data$y, main = "Boxplots of Data (Non-paired)",
            xlab = "Groups", ylab = "Value")
  }
}
