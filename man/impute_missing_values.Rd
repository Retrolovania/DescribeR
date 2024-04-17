\name{impute_missing_values}
\alias{impute_missing_values}
\title{Impute Missing Values}
\usage{impute_missing_values(data, method)}

\arguments{
  \item{data}{A data frame containing the dataset with missing values.}
  \item{method}{The imputation method to be used.}
}
\description{
  This function attempts to add in values missing from a dataset using specified imputation methods.
}
\examples{
  \dontrun{
    # Load the airquality dataset
    data(airquality)

    # Print the first few rows of the airquality dataset
    head(airquality)

    # mean
    imputed_data_mean <- impute_missing_values(data = airquality, method = "mean")

    # median
    imputed_data_median <- impute_missing_values(data = airquality, method = "median")

    # mode
    imputed_data_mode <- impute_missing_values(data = airquality, method = "mode")

    # zero
    imputed_data_zero <- impute_missing_values(data = airquality, method = "zero")
  }
}
\seealso{
  \code{\link{mean}}, \code{\link{median}}, \code{\link{Mode}}
}
\keyword{utilities}
