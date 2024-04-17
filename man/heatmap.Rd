\name{interactive_heatmap}
\alias{interactive_heatmap}
\title{Interactive Heatmap}
\usage{
  interactive_heatmap(data, method)
}
\arguments{
  \item{data}{A data frame containing the dataset.}
  \item{method}{The method for calculating correlations (e.g., "pearson", "spearman").}
}
\description{
  This function generates an interactive heatmap of variable correlations or other metrics.
}
\examples{
  \dontrun{
    # Create an interactive heatmap of variable correlations using Pearson correlation
    interactive_heatmap(data = mtcars, method = "pearson")
  }
}
\seealso{
  \code{\link{cor}}, \code{\link[plotly]{plot_ly}}, \code{\link[plotly]{layout}}
}
\keyword{utilities}
