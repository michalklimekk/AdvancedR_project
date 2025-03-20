#' @title Function: plot_scatter
#'
#' @description
#' This function creates an interactive scatter plot using the specified columns from the data.
#' The columns are identified by their names passed as arguments to the function.
#'
#'
#' @param data A data frame containing the data to be plotted.
#' @param x_column The name of the column to be used for the x-axis. (default is "x")
#' @param y_column The name of the column to be used for the y-axis. (default is "y")
#' @return An interactive plotly scatter plot.
#' @import plotly
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' plot_data(data, x_column = "x", y_column = "y")
#' @export
plot_data <- function(data, x_column="x", y_column="y") {
  # Loading plotly library
  library(plotly)

  # Checking if specified columns exist
  stopifnot(
    x_column %in% colnames(data),
    y_column %in% colnames(data)
  )

  # Creating interactive scatter plot
  plot <- plot_ly(data, x = ~data[[x_column]], y = ~data[[y_column]],
                  type = 'scatter', mode = 'markers') %>%
    plotly::layout(
      title = paste("Scatter Plot of '", x_column, "' vs '", y_column, "'"),
      xaxis = list(title = x_column),
      yaxis = list(title = y_column)
    )

  return(plot)
}
