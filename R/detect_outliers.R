#' Detect Outliers
#'
#' This function identifies potential outliers in a given dataset based on a specified threshold.
#'
#' @param data A data frame containing the dataset to be analyzed.
#' @param vars The names of the variables/columns in which outliers need to be detected.
#' @param threshold The threshold above which data points could be considered outliers.
#'
#' @return A list containing the indices of potential outliers for each variable specified.
#'
#' @examples
#' outliers <- detect_outliers(data = iris, vars = "Sepal.Length", threshold = 2.5)
#' print(outliers)
#'
#' @export
detect_outliers <- function(data, vars, threshold) {
  outliers_list <- list()

  for (var in vars) {
    if (!is.numeric(data[[var]])) {
      warning(paste("Variable", var, "is not numeric. Skipping."))
      next
    }

    var_outliers <- which(data[[var]] > threshold)
    outliers_list[[var]] <- var_outliers
  }

  return(outliers_list)
}
