#' Impute Missing Values
#'
#' This function attempts to add in values missing from a dataset using specified imputation methods.
#'
#' @param data A data frame containing the dataset with missing values.
#' @param method The imputation method to be used.
#'
#' @return A data frame with missing values imputed using the specified method.
#'
#' @examples
#' # mean
#' imputed_data_mean <- impute_missing_values(data = airquality, method = "mean")
#'
#' # median
#' imputed_data_median <- impute_missing_values(data = airquality, method = "median")
#'
#' # mode
#' imputed_data_mode <- impute_missing_values(data = airquality, method = "mode")
#'
#' # zero
#' imputed_data_zero <- impute_missing_values(data = airquality, method = "zero")
#'
#' @export
impute_missing_values <- function(data, method) {  
  impute_func <- switch(
    method,
    mean = function(x) mean(x, na.rm = TRUE),
    median = function(x) median(x, na.rm = TRUE),
    mode = function(x) Mode(x, na.rm = TRUE),
    zero = function(x) 0,  # Fill missing values with zero
    stop("Invalid imputation method. Please use: 'mean', 'median', 'mode', or 'zero'")
  )

  imputed_data <- data

  for (col in colnames(imputed_data)) {
    if (any(is.na(imputed_data[[col]]))) {
      imputed_data[[col]][is.na(imputed_data[[col]])] <- impute_func(imputed_data[[col]])
    }
  }

  return(imputed_data)
}
