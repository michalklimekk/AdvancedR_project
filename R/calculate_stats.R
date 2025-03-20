#' @title Function: calculate_stats
#'
#' @description
#' Function `calculate_stats` calculates basic summary statistics for a numeric
#' vector x. It computes mean, first quartile (Q1), median, third quartile (Q3),
#' variance, standard deviation, and interquartile range (IQR). NA values are
#' removed during preprocessing if na.rm is TRUE.
#'
#' @param x A numeric vector.
#' @param na.rm Boolean flag. If TRUE, NA values are removed before calculations
#'          (default is TRUE). If FALSE, errors may occur (e.g., mean(c(NA)) is NA).
#'
#' @return Returns an object of class `StatsSummary`, which is a list containing:
#'
#'          - mean: Mean of x.
#'          - Q1: First quartile (25th percentile) of x.
#'.         - median: Median of x.
#'          - Q3: Third quartile (75th percentile) of x.
#'          - variance: Variance of x.
#'          - sd: Standard deviation of x.
#'          - IQR: Interquartile range of x.
#'          - data: The processed data (after removing NAs if na.rm is TRUE).
#'
#' @examples
#' calculate_stats(x = 1:5)
#' calculate_stats(c(1, 3, 5, 72, 1234))
#' @export
calculate_stats <- function(x, na.rm = TRUE) {
  # Checking that x is a numeric vector
  stopifnot(is.numeric(x))

  # Checking that na.rm is logical
  stopifnot(is.logical(na.rm))

  # Removing NA values if requested
  if (na.rm) {x <- x[!is.na(x)]}

  # Checking that the vector is not empty after NA removal
  stopifnot(length(x) > 0)

  # Checking that x does not contain infinite values
  stopifnot(!any(is.infinite(x)))

  # Calculating summary statistics
  mean_val <- mean(x)
  Q1_val <- as.numeric(quantile(x, probs = 0.25))
  median_val <- median(x)
  Q3_val <- as.numeric(quantile(x, probs = 0.75))
  variance_val <- var(x)
  sd_val <- sd(x)
  IQR_val <- IQR(x)

  # Getting results together
  result <- list(
    mean = mean_val,
    Q1 = Q1_val,
    median = median_val,
    Q3 = Q3_val,
    variance = variance_val,
    sd = sd_val,
    IQR = IQR_val,
    data = x # input after NA removal
  )

  # Adding class attribute
  class(result) <- "StatsSummary"

  # Returning result list
  return(result)
}
