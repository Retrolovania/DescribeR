#' Visualize Data
#'
#' This function creates varying visualizations depending on the input data and plot type specified.
#'
#' @param data A data frame containing the dataset to be visualized.
#' @param x_var The name of the variable to be plotted on the x-axis.
#' @param y_var (Optional) The name of the variable to be plotted on the y-axis (if applicable).
#' @param plot_type The type of plot to be generated (e.g., "histogram", "boxplot").
#'
#' @examples
#' visualize_data(data = iris, x_var = "Sepal.Length", plot_type = "histogram")
#' visualize_data(data = iris, x_var = "Species", y_var = "Petal.Length", plot_type = "boxplot")
#'
#' @export
visualize_data <- function(data, x_var, y_var = NULL, plot_type) {
  # Validate plot_type
  allowed_plot_types <- c("histogram", "boxplot", "scatterplot")
  if (!plot_type %in% allowed_plot_types) {
    stop("Invalid plot_type. Please specify one of: ", paste(allowed_plot_types, collapse = ", "))
  }

  # Plot based on plot_type
  if (plot_type == "histogram") {
    if (!is.numeric(data[[x_var]])) {
      stop("x_var must be numeric for histogram plot.")
    }
    hist(data[[x_var]], main = paste("Histogram of", x_var), xlab = x_var)
  } else if (plot_type == "boxplot") {
    if (!is.numeric(data[[y_var]])) {
      stop("y_var must be numeric for boxplot.")
    }
    boxplot(data[[y_var]] ~ data[[x_var]], data = data, main = paste("Boxplot of", y_var, "by", x_var), xlab = x_var, ylab = y_var)
  } else if (plot_type == "scatterplot") {
    if (!is.numeric(data[[x_var]]) || !is.numeric(data[[y_var]])) {
      stop("x_var and y_var must be numeric for scatterplot.")
    }
    plot(data[[x_var]], data[[y_var]], main = paste("Scatterplot of", y_var, "vs", x_var), xlab = x_var, ylab = y_var)
  }
}
