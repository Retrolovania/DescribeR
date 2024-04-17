\name{detect_outliers}
\alias{detect_outliers}
\title{Detect Outliers}
\usage{detect_outliers(data, vars, threshold)}

\arguments{
  \item{data}{A data frame containing the dataset to be analyzed.}
  \item{vars}{The names of the variables/columns in which outliers need to be detected.}
  \item{threshold}{The threshold above which data points could be considered outliers.}
}
\description{
  This function identifies potential outliers in a given dataset based on a specified threshold.
}
\examples{
  \dontrun{
    # Detect outliers in the 'Sepal.Length' variable of the iris dataset with a threshold of 2.5
    outliers <- detect_outliers(data = iris, vars = "Sepal.Length", threshold = 2.5)
    print(outliers)
  }
}
\seealso{
  \code{\link{boxplot.stats}}, \code{\link{quantile}}
}
\keyword{utilities}
