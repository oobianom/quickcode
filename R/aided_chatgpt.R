#' Calculate the Mode of a Numeric or Character Vector
#'
#' This function calculates the mode (most frequently occurring value(s)) of a numeric or character vector.
#'
#' @param x A numeric or character vector for which the mode is to be calculated.
#' @return The mode(s) of the input vector. If multiple values have the same highest frequency, all modes are returned.
#'         Returns `NA` if the input vector is empty.
#' @examples
#' # Example with a numeric vector
#' numeric_vector <- c(1, 2, 2, 3, 3, 3, 4, 5)
#' mode.calc(numeric_vector)
#'
#' # Example with a character vector
#' character_vector <- c("apple", "banana", "apple", "orange", "banana", "banana")
#' mode.calc(character_vector)
#'
#' @export
mode.calc <- function(x) {
  # Check for empty input
  if (length(x) == 0) {
    return(NA)
  }

  # Calculate frequency of each element
  freq_table <- table(x)

  # Find the maximum frequency
  max_freq <- max(freq_table)

  # Return the element(s) with the highest frequency
  mode_values <- names(freq_table[freq_table == max_freq])

  # If numeric, convert to numeric type
  if (is.numeric(x)) {
    mode_values <- as.numeric(mode_values)
  }

  return(mode_values)
}


#' Calculate the Range Difference of a Numeric Vector
#'
#' This function calculates the difference between the maximum and minimum values of a numeric vector.
#'
#' @param x A numeric vector for which the range difference is to be calculated.
#' @return A numeric value representing the difference between the maximum and minimum values of the input vector.
#'         Returns `NA` if the input is empty or contains only `NA` values.
#' @examples
#' # Example with a numeric vector
#' numeric_vector <- c(1, 5, 3, 8, 2)
#' sub.range(numeric_vector)
#'
#' # Example with missing values
#' sub.range(c(NA, 4, 7, NA, 10))
#'
#' @export
sub.range <- function(x) {
  # Check if input is valid
  if (length(x) == 0 || all(is.na(x))) {
    return(NA)
  }

  # Calculate the range difference
 max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
}


#' Cumulative Sum with NA Removal
#'
#' This function calculates the cumulative sum of a numeric vector, removing any `NA` values before computation.
#'
#' @param x A numeric vector for which the cumulative sum is to be calculated.
#' @return A numeric vector representing the cumulative sum with `NA` values removed.
#'         Returns an empty vector if the input is empty or contains only `NA` values.
#' @examples
#' # Example with numeric vector
#' numeric_vector <- c(1, 2, NA, 4, 5)
#' na.cumsum(numeric_vector)
#'
#' # Example with all NAs
#' na.cumsum(c(NA, NA))
#'
#' @export
na.cumsum <- function(x) {
  # Check if input is valid
  if (length(x) == 0 || all(is.na(x))) {
    return(numeric(0)) # Return an empty numeric vector if all values are NA
  }

  # Remove NA values
  # Calculate the cumulative sum
  cumsum(x[!is.na(x)])
}





#' Replicate Rows in a Data Frame
#'
#' This function replicates each row in a data frame a specified number of times.
#'
#' @param data A data frame whose rows are to be replicated.
#' @param n An integer specifying the number of times each row should be replicated.
#' @return A data frame with each row replicated `n` times. If `n` is less than or equal to 0, an empty data frame is returned.
#' @examples
#' # Example with a simple data frame
#' df <- data.frame(A = c(1, 2), B = c("x", "y"))
#' rows.rep(df, 3)
#'
#' # Example with no replication (n = 0)
#' rows.rep(df, 0)
#'
#' @export
rows.rep <- function(data, n) {
  # Validate inputs
  if (!is.data.frame(data))stop("Input 'data' must be a data frame.")
  if (missing(n) | n <= 0) return(data)
  # Replicate rows
  replicated_data <- data[rep(1:nrow(data), each = n), ]
  rownames(replicated_data) <- NULL # Reset row names for clarity
  replicated_data
}



#' Replicate Columns in a Data Frame
#'
#' This function replicates each column in a data frame a specified number of times.
#'
#' @param data A data frame whose columns are to be replicated.
#' @param n An integer specifying the number of times each column should be replicated.
#' @return A data frame with each column replicated `n` times. If `n` is less than or equal to 0, an empty data frame is returned.
#' @examples
#' # Example with a simple data frame
#' df <- data.frame(A = c(1, 2), B = c(3, 4))
#' cols.rep(df, 3)
#'
#' # Example with no replication (n = 0)
#' cols.rep(df, 0)
#'
#' @export
cols.rep <- function(data, n) {
  # Validate inputs
  if (!is.data.frame(data)) stop("Input 'data' must be a data frame.")
  if (missing(n) | n <= 0) return(data)
  # Replicate each column `n` times
  replicated_data <- do.call(cbind, replicate(n, data, simplify = FALSE))
  # Optionally reset column names to indicate replication
  colnames(replicated_data) <- make.names(rep(colnames(data), each = n), unique = TRUE)
  replicated_data
}



#' Normalize a Numeric Vector to the Range [0, 1]
#'
#' This function normalizes a numeric vector so that all values are scaled to the range [0, 1].
#'
#' @param x A numeric vector to be normalized.
#' @return A numeric vector with values scaled to the range [0, 1].
#'         If all values are identical, all values are set to 0.
#' @examples
#' # Example with a numeric vector
#' normalize.vector(c(1, 2, 3, 4, 5))
#'
#' # Example with a vector containing identical values
#' normalize.vector(c(3, 3, 3))
#'
#' @export
normalize.vector <- function(x) {
  # Check if the input is numeric
  if (!is.numeric(x)) {
    stop("Input 'x' must be a numeric vector.")
  }

  # Handle cases with no variation (all values identical or single element)
  if (length(unique(x)) == 1) {
    return(rep(0, length(x)))
  }

  # Calculate min and max
  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)

  # Normalize to the range [0, 1]
  (x - x_min) / (x_max - x_min)
}



#' Create Tensor-Like Slices from a Data Frame or Matrix
#'
#' This function converts a data frame or matrix into row-wise slices, similar to TensorFlow's `from_tensor_slices()`.
#'
#' @param data A data frame or matrix to be converted into slices.
#' @return A list of slices, each corresponding to a row of the input data.
#' @examples
#' df <- data.frame(A = 1:3, B = c("x", "y", "z"))
#' from_tensor_slices(df)
#'
#' @export
from_tensor_slices <- function(data) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("Input must be a data frame or matrix.")
  }
  split(data, seq(nrow(data)))
}



#' Multi-Head Attention
#'
#' This function implements a simplified version of multi-head attention for sequence data.
#'
#' @param query A numeric matrix representing the query.
#' @param key A numeric matrix representing the key.
#' @param value A numeric matrix representing the value.
#' @param num_heads An integer specifying the number of attention heads.
#' @return A numeric matrix representing the attention output.
#' @examples
#' query <- matrix(rnorm(12), nrow = 3)
#' key <- matrix(rnorm(12), nrow = 3)
#' value <- matrix(rnorm(12), nrow = 3)
#' multihead_att(query, key, value, num_heads = 2)
#'
#' @export
multihead_att <- function(query, key, value, num_heads) {
  if (ncol(query) != ncol(key) || ncol(key) != ncol(value)) {
    stop("Query, key, and value must have the same number of columns.")
  }
  # Simple dot-product attention
  attention_scores <- query %*% t(key)
  attention_weights <- exp(attention_scores) / rowSums(exp(attention_scores))
  attention_weights %*% value
}



#' Learning Rate Scheduler
#'
#' This function adjusts the learning rate at each epoch according to a custom schedule.
#'
#' @param initial_lr A numeric value representing the initial learning rate.
#' @param schedule A function that takes the epoch number as input and returns the adjusted learning rate.
#' @param epochs An integer specifying the total number of epochs.
#' @return A numeric vector representing the learning rate for each epoch.
#' @examples
#' schedule <- function(epoch) { 0.01 * (0.9 ^ epoch) }
#' learn_rate_scheduler(0.01, schedule, epochs = 10)
#'
#' @export
learn_rate_scheduler <- function(initial_lr, schedule, epochs) {
  if (!is.numeric(initial_lr) || initial_lr <= 0) {
    stop("Initial learning rate must be a positive number.")
  }
  sapply(0:(epochs - 1), schedule)
}




