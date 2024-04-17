\name{visualize_data}
\alias{visualize_data}
\title{Visualize Data}
\usage{visualize_data(data, x_var, y_var, plot_type)}

\arguments{
  \item{data}{A data frame containing the dataset.}
  \item{x_var}{The name of the variable to be plotted on the x-axis.}
  \item{y_var}{The name of the variable to be plotted on the y-axis.}
  \item{plot_type}{The type of plot to be generated (e.g., "histogram", "boxplot").}
}
\description{
  This function creates varying visualizations depending on the input data and plot type specified.
}
\examples{
  \dontrun{
    # Load the iris dataset
    data(iris)

    # Generate a histogram for a numeric variable
    visualize_data(data = iris, x_var = "Sepal.Length", plot_type = "histogram")

    # Generate a box plot for a numeric variable grouped by a categorical variable
    visualize_data(data = iris, x_var = "Species", y_var = "Petal.Length", plot_type = "boxplot")
  }
}
\seealso{
  \code{\link{hist}}, \code{\link{boxplot}}
}
\keyword{utilities}
