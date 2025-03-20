#' @title Function: compare_means
#'
#' @description
#' Function `compare_means` compares the means of two distributions by
#' performing a t-test on the data stored in objects of class 'StatsSummary'.
#' This function checks if the difference in means of two samples is statistically
#' significant. That means that test checks if mean_1 = mean_2 approximately.
#'
#' @param stats1 An object of class 'StatsSummary'.
#' @param stats2 An object of class 'StatsSummary'.
#' @param alpha Significance level for the t-test (default is 0.05).
#' @param alternative Specifies the alternative hypothesis: "two.sided" (default), "greater", or "less".
#'
#' @return A list containing mean difference, p-value, and test conclusion.
#'
#' @examples
#' stats1 <- calculate_stats(rnorm(50))
#' stats2 <- calculate_stats(rnorm(50, mean = 0.5))
#' result <- compare_means(stats1, stats2)
#' print(result)
#' @export
compare_means <- function(stats1, stats2, alpha = 0.05, alternative = 'two.sided') {
  # Checking that both objects are of class 'StatsSummary'
  stopifnot(class(stats1) == "StatsSummary")
  stopifnot(class(stats2) == "StatsSummary")

  # Checking that significance level alpha is a numeric value between 0 and 1
  stopifnot(is.numeric(alpha), alpha > 0, alpha < 1)

  # Checking that alternative is a valid choice
  stopifnot(alternative %in% c("two.sided", "less", "greater"))

  # Extracting data from the StatsSummary objects
  data1 <- stats1$data
  data2 <- stats2$data

  # Calculating diffrence of means
  mean_diff <- stats1$mean - stats2$mean

  # Performing t-test using tryCatch to handle potential errors (e.g., identical samples)
  test <- tryCatch({
    t.test(x = data1, y = data2, alternative = alternative)
  }, error = function(e) {
    stop("Error in t-test: ", e$message)
  })

  # Extracting p-value of our test
  p_value <- test$p.value

  # If p_value < alpha we reject null hypothesis
  conclusion <- if (p_value < alpha) {
    if (alternative == "two.sided") {
      "Means are significantly different"
    } else if (alternative == "greater") {
      "Mean of stats1 is significantly greater than mean of stats2"
    } else {
      "Mean of stats1 is significantly smaller than mean of stats2"
    }
  } else { # accept null hypothesis
    "No significant difference between means"
  }

  return(list(mean_diff = mean_diff, p_value = p_value, conclusion = conclusion))
}
