#' Interactive Heatmap
#'
#' This function generates an interactive heatmap of variable correlations or other metrics.
#' All variables in the data must be numeric.
#'
#' @param data A data frame containing the dataset.
#' @param method The method for calculating correlations (e.g., "pearson", "spearman").
#'
#' @return An interactive heatmap.
#'
#' @examples
#' # Create an interactive heatmap of variable correlations using Pearson correlation
#' interactive_heatmap(data = mtcars, method)
#'
#' @export
interactive_heatmap <- function(data, method) {
  # Check if the data contains only numeric variables
  if (!all(sapply(data, is.numeric))) {
    stop("Correlation calculation requires all variables to be numeric.")
  }

  # Calculate correlations
  correlations <- cor(data, method = method)

  # Create heatmap
  heatmap <- plotly::plot_ly(z = correlations,
                             x = colnames(correlations),
                             y = colnames(correlations),
                             type = "heatmap",
                             colorscale = "Viridis",
                             colorbar = list(title = "Correlation"),
                             hoverinfo = "z")

  # Add labels to axes
  heatmap <- heatmap %>% plotly::layout(xaxis = list(title = ""),
                                        yaxis = list(title = ""))

  return(heatmap)
}
