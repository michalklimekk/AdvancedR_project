#' Example Dataset
#'
#' A dataset containing 100 random points from normal and uniform distributions.
#'
#' @docType data
#' @usage data (example_data)
#' @format A data frame with 100 observations and 3 variables:
#' \describe{
#'   \item{x}{Randomly generated x-values from a normal distribution N(50, 10).}
#'   \item{y}{Randomly generated y-values from a normal distribution N(100, 20).}
#'   \item{z}{Randomly generated z-values from a uniform distribution U(0,1).}
#' }
#' @keywords dataset example_data
#' @examples
#' data(example_data)
#' plot_data(example_data, "x", "y")
"example_data"
