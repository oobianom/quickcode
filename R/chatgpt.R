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
#' mode(numeric_vector)
#'
#' # Example with a character vector
#' character_vector <- c("apple", "banana", "apple", "orange", "banana", "banana")
#' mode(character_vector)
#'
#' @export
mode <- function(x) {
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
#' range.diff(numeric_vector)
#'
#' # Example with missing values
#' range.diff(c(NA, 4, 7, NA, 10))
#'
#' @export
range.diff <- function(x) {
  # Check if input is valid
  if (length(x) == 0 || all(is.na(x))) {
    return(NA)
  }

  # Calculate the range difference
  diff <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)

  return(diff)
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
#' cumsum.na.rm(numeric_vector)
#'
#' # Example with all NAs
#' cumsum.na.rm(c(NA, NA))
#'
#' @export
cumsum.na.rm <- function(x) {
  # Check if input is valid
  if (length(x) == 0 || all(is.na(x))) {
    return(numeric(0)) # Return an empty numeric vector if all values are NA
  }

  # Remove NA values
  x_no_na <- x[!is.na(x)]

  # Calculate the cumulative sum
  cumsum_result <- cumsum(x_no_na)

  return(cumsum_result)
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
#' rep.rows(df, 3)
#'
#' # Example with no replication (n = 0)
#' rep.rows(df, 0)
#'
#' @export
rep.rows <- function(data, n) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  if (n <= 0) {
    return(data.frame()) # Return an empty data frame if n <= 0
  }

  # Replicate rows
  replicated_data <- data[rep(1:nrow(data), each = n), ]
  rownames(replicated_data) <- NULL # Reset row names for clarity

  return(replicated_data)
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
#' rep.cols(df, 3)
#'
#' # Example with no replication (n = 0)
#' rep.cols(df, 0)
#'
#' @export
rep.cols <- function(data, n) {
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame.")
  }
  if (n <= 0) {
    return(data.frame()) # Return an empty data frame if n <= 0
  }

  # Replicate each column `n` times
  replicated_data <- do.call(cbind, replicate(n, data, simplify = FALSE))

  # Optionally reset column names to indicate replication
  colnames(replicated_data) <- make.names(rep(colnames(data), each = n), unique = TRUE)

  return(replicated_data)
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
  slices <- split(data, seq(nrow(data)))
  return(slices)
}


#' Interpolate a Numeric Matrix or Tensor
#'
#' This function resizes a matrix or 2D array using simple interpolation methods (nearest-neighbor or linear).
#'
#' @param data A numeric matrix or 2D array to be resized.
#' @param new_rows An integer specifying the new number of rows.
#' @param new_cols An integer specifying the new number of columns.
#' @return A resized matrix with interpolated values.
#' @examples
#' mat <- matrix(1:9, nrow = 3)
#' interpolate_tensor(mat, 6, 6)
#'
#' @export
interpolate_tensor <- function(data, new_rows, new_cols) {
  if (!is.matrix(data)) {
    stop("Input must be a matrix.")
  }
  approx(seq_len(nrow(data)), data, n = new_rows)

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
#' multihead_attention(query, key, value, num_heads = 2)
#'
#' @export
multihead_attention <- function(query, key, value, num_heads) {
  if (ncol(query) != ncol(key) || ncol(key) != ncol(value)) {
    stop("Query, key, and value must have the same number of columns.")
  }

  # Simple dot-product attention
  attention_scores <- query %*% t(key)
  attention_weights <- exp(attention_scores) / rowSums(exp(attention_scores))
  output <- attention_weights %*% value

  return(output)
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

  lr_schedule <- sapply(0:(epochs - 1), schedule)
  return(lr_schedule)
}




#' Detect Outliers in a Numeric Vector
#'
#' This function identifies outliers in a numeric vector using either the interquartile range
#' (IQR) method or the z-score method. The IQR method defines outliers as values below
#' Q1 - multiplier * IQR or above Q3 + multiplier * IQR, where Q1 and Q3 are the first and
#' third quartiles. The z-score method identifies outliers as values with an absolute
#' z-score exceeding a specified threshold.
#'
#' The function returns a list containing the outliers, their indices, detection bounds or
#' thresholds, and a logical vector indicating which elements are outliers. If a grouping
#' vector is provided via `groups`, outlier detection is performed separately for each group,
#' and results are returned as a nested list by group. If `na.rm = TRUE` (default), missing
#' values (`NA`) are removed before computation. If `na.rm = FALSE` and `NA` values are
#' present, the function stops with an error. The function also stops for non-numeric input,
#' insufficient valid data, or mismatched group lengths.
#'
#' @param x A numeric vector in which to detect outliers.
#' @param method A character string specifying the outlier detection method. Options are
#'   `"iqr"` (default) for the interquartile range method or `"zscore"` for the z-score method.
#' @param multiplier A positive numeric value specifying the multiplier for the IQR method.
#'   Default is `1.5`, typically used for moderate outliers; `3` is common for extreme outliers.
#'   Ignored if `method = "zscore"`.
#' @param z_threshold A positive numeric value specifying the z-score threshold for the
#'   `method = "zscore"` option. Default is `3`, meaning values with an absolute z-score
#'   greater than 3 are flagged as outliers. Ignored if `method = "iqr"`.
#' @param na.rm A logical value indicating whether to remove `NA` values before computation.
#'   Default is `TRUE`. If `FALSE` and `NA` values are present, the function stops with an error.
#' @param groups An optional vector of group names or labels corresponding to each value in
#'   `x`. If provided, must be the same length as `x`. Outlier detection is performed separately
#'   for each unique group, and results are returned as a nested list. Default is `NULL` (no grouping).
#' @param summary A logical value indicating whether to include a summary in the output.
#'   Default is `FALSE`. If `TRUE`, the output list includes a `summary` element with
#'   descriptive statistics and outlier counts, either overall or by group if `groups` is provided.
#' @param plot A logical value indicating whether to show plot
#'
#' @return If `groups = NULL` (default), a list with the following components:
#'   - `outliers`: A numeric vector of the outlier values.
#'   - `indices`: An integer vector of the indices where outliers occur in the input vector.
#'   - `bounds` (if `method = "iqr"`): A named numeric vector with the `lower` and `upper`
#'     bounds for outliers.
#'   - `threshold` (if `method = "zscore"`): A named numeric vector with the `lower` and
#'     `upper` z-score thresholds.
#'   - `is_outlier`: A logical vector of the same length as `x`, where `TRUE` indicates an
#'     outlier.
#'   - `summary` (if `summary = TRUE`): A list with summary statistics including the mean,
#'     median, standard deviation (for z-score), quartiles (for IQR), and number of outliers.
#'
#'   If `groups` is provided, a named list where each element corresponds to a unique group,
#'   containing the same components as above but computed separately for that group’s values.
#'
#' @details
#' The function requires at least two non-`NA` values per group (if `groups` is provided) or
#' overall (if `groups = NULL`) to compute meaningful statistics when `na.rm = TRUE`. If
#' `na.rm = FALSE`, the presence of `NA` values triggers an error. If all values in a group
#' are identical or there are insufficient data points, an error is thrown for that group.
#' The IQR method is robust to non-normal data, while the z-score method assumes approximate
#' normality and is sensitive to extreme values.
#'
#' @examples
#' # Example 1: Basic IQR method without groups
#' x <- c(1, 2, 3, 4, 100)
#' detect_outlier(x)
#'
#' # IQR method with summary
#' detect_outlier(x, summary = TRUE)
#'
#' # Z-score method with custom threshold
#' y <- c(-10, 1, 2, 3, 4, 5, 20)
#' detect_outlier(y, method = "zscore", z_threshold = 2.5)
#'
#' # Handling missing values
#' z <- c(1, 2, NA, 4, 5, 100)
#' detect_outlier(z, method = "iqr", multiplier = 3)
#'
#' x <- c(1, 2, 3, 4, 100)
#' detect_outlier(x, plot = TRUE)
#'
#' # Example 2: IQR method with groups
#' x2 <- c(1, 2, 3, 100, 5, 6, 7, 200)
#' groups2 <- c("A", "A", "A", "A", "B", "B", "B", "B")
#' detect_outlier(x2, groups = groups2)
#'
#' # Example 3: Z-score method with groups and summary
#' x3 <- c(-10, 1, 2, 20, 3, 4, 5, 50)
#' groups3 <- c("X", "X", "X", "X", "Y", "Y", "Y", "Y")
#' detect_outlier(x3, method = "zscore", z_threshold = 2, groups = groups3, summary = TRUE)
#'
#' # Example 4: IQR method with groups and NA values
#' x4 <- c(1, 2, NA, 100, 5, 6, 7, 200,1000)
#' groups4 <- c("G1", "G1", "G1", "G1", "G2", "G2", "G2", "G2","G1")
#' detect_outlier(x4, groups = groups4)
#'
#' # Error cases
#' \dontrun{
#' detect_outlier(c("a", "b"))  # Non-numeric input
#' detect_outlier(c(1), groups = c("A"))  # Insufficient data
#' detect_outlier(c(1, 2), groups = c("A"))  # Mismatched group length
#' detect_outlier(c(1, NA, 3), na.rm = FALSE)  # NA with na.rm = FALSE
#' }
#'
#' @export
detect_outlier <- function(x, method = "iqr", multiplier = 1.5, z_threshold = 3, na.rm = TRUE, groups = NULL, summary = FALSE, plot = FALSE) {
  # Check if input is numeric
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }

  # Validate groups if provided
  if (!is.null(groups)) {
    if (length(groups) != length(x)) {
      stop("Length of 'groups' must match length of 'x'")
    }
  }

  # Handle NA values based on na.rm
  if (!na.rm && any(is.na(x))) {
    stop("NA values are present and na.rm = FALSE")
  }

  # Validate method
  method <- tolower(method)
  if (!method %in% c("iqr", "zscore")) {
    stop("Method must be either 'iqr' or 'zscore'")
  }

  # Process by group if groups is provided
  if (!is.null(groups)) {
    # Split data by groups
    group_list <- split(data.frame(x = x, index = seq_along(x)), groups)
    result <- lapply(names(group_list), function(g) {
      x_group <- group_list[[g]]$x
      indices <- group_list[[g]]$index

      # Remove NA values if na.rm = TRUE
      x_clean <- if (na.rm) x_group[!is.na(x_group)] else x_group
      if (length(x_clean) < 2) {
        stop(sprintf("Group '%s' has fewer than 2 non-NA values", g))
      }

      # Initialize group result
      group_result <- list()

      if (method == "iqr") {
        q1 <- quantile(x_clean, 0.25, na.rm = TRUE)
        q3 <- quantile(x_clean, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        if (iqr == 0) {
          stop(sprintf("All values in group '%s' are identical; IQR is zero", g))
        }
        lower_bound <- q1 - multiplier * iqr
        upper_bound <- q3 + multiplier * iqr
        group_result$outliers <- x_clean[x_clean < lower_bound | x_clean > upper_bound]
        group_result$indices <- indices[x_group < lower_bound | x_group > upper_bound]
        group_result$bounds <- c(lower = lower_bound, upper = upper_bound)
        group_result$is_outlier <- x_group < lower_bound | x_group > upper_bound
        if (summary) {
          group_result$summary <- list(
            mean = mean(x_clean),
            median = median(x_clean),
            q1 = q1,
            q3 = q3,
            iqr = iqr,
            n_outliers = length(group_result$outliers)
          )
        }
      } else if (method == "zscore") {
        mean_x <- mean(x_clean)
        sd_x <- sd(x_clean)
        if (sd_x == 0) {
          stop(sprintf("All values in group '%s' are identical; standard deviation is zero", g))
        }
        z_scores <- (x_group - mean_x) / sd_x
        lower_z <- -z_threshold
        upper_z <- z_threshold
        group_result$outliers <- x_clean[abs(z_scores[!is.na(x_group)]) > z_threshold]
        group_result$indices <- indices[abs(z_scores) > z_threshold]
        group_result$threshold <- c(lower = lower_z, upper = upper_z)
        group_result$is_outlier <- abs(z_scores) > z_threshold
        if (summary) {
          group_result$summary <- list(
            mean = mean_x,
            median = median(x_clean),
            sd = sd_x,
            n_outliers = length(group_result$outliers)
          )
        }
      }
      group_result
    })
    names(result) <- names(group_list)
    return(result)
  }

  # Process without groups (original behavior)
  x_clean <- if (na.rm) x[!is.na(x)] else x
  if (length(x_clean) < 2) {
    stop("At least two non-NA values are required")
  }


  skewness = if(length(x_clean) > 2) {
    sum((x_clean - mean(x_clean))^3) /
      (length(x_clean) * sd(x_clean)^3)
  } else NA
  kurtosis = if(length(x_clean) > 2) {
    sum((x_clean - mean(x_clean))^4) /
      (length(x_clean) * sd(x_clean)^4) - 3
  } else NA

  result <- list()

  if (method == "iqr") {
    q1 <- quantile(x_clean, 0.25, na.rm = TRUE)
    q3 <- quantile(x_clean, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    if (iqr == 0) {
      stop("All values are identical; IQR is zero")
    }
    lower_bound <- q1 - multiplier * iqr
    upper_bound <- q3 + multiplier * iqr
    result$outliers <- x_clean[x_clean < lower_bound | x_clean > upper_bound]
    result$indices <- which(x < lower_bound | x > upper_bound)
    result$bounds <- c(lower = lower_bound, upper = upper_bound)
    result$is_outlier <- x < lower_bound | x > upper_bound
    if (summary) {
      result$summary <- list(
        n = length(x_clean),
        mean = mean(x_clean),
        median = median(x_clean),
        q1 = q1,
        q3 = q3,
        iqr = iqr,
        n_outliers = length(result$outliers),
        skewness = skewness,
        kurtosis = kurtosis
      )
    }
  } else if (method == "zscore") {
    mean_x <- mean(x_clean)
    sd_x <- sd(x_clean)
    if (sd_x == 0) {
      stop("All values are identical; standard deviation is zero")
    }
    z_scores <- (x - mean_x) / sd_x
    lower_z <- -z_threshold
    upper_z <- z_threshold
    result$outliers <- x_clean[abs(z_scores[!is.na(x)]) > z_threshold]
    result$indices <- which(abs(z_scores) > z_threshold)
    result$threshold <- c(lower = lower_z, upper = upper_z)
    result$is_outlier <- abs(z_scores) > z_threshold
    if (summary) {
      result$summary <- list(
        n = length(x_clean),
        mean = mean_x,
        median = median(x_clean),
        sd = sd_x,
        n_outliers = length(result$outliers),
        skewness = skewness,
        kurtosis = kurtosis
      )
    }
  }

  if (plot) {
    result$plots <- list()

    # Overall plots
    result$plots$overall <- list()

    # Boxplot
    result$plots$overall$boxplot <- function() {
      if (!is.null(groups)) {
        boxplot(split(x_clean, groups), main = "Boxplot by Group",
                ylab = "Value", col = "lightblue",
                outline = TRUE)
      } else {
        boxplot(x_clean, main = "Boxplot with Outliers",
                ylab = "Value", col = "lightblue",
                outline = TRUE)
        points(rep(1, length(x_clean)), x_clean, col = "red", pch = 20)
      }
    }

    # Density plot
    result$plots$overall$density <- function() {
      if (!is.null(groups)) {
        # Setup plot
        plot(NULL, xlim = range(x_clean, na.rm = TRUE),
             ylim = c(0, max(sapply(split(x_clean, groups), function(g) {
               max(density(g, na.rm = TRUE)$y)
             }))),
             main = "Density Plot by Group",
             xlab = "Value", ylab = "Density")

        # Plot each group with different colors
        colors <- rainbow(length(unique(groups)))
        for (i in seq_along(unique(groups))) {
          group_data <- x_clean[groups == unique(groups)[i]]
          d <- density(group_data, na.rm = TRUE)
          lines(d, col = colors[i])
          rug(group_data, col = colors[i])
        }
        legend("topright", legend = unique(groups),
               col = colors, lty = 1)
      } else {
        d <- density(x_clean, na.rm = TRUE)
        plot(d, main = "Density Plot of Data",
             xlab = "Value", ylab = "Density")
        polygon(d, col = "lightblue", border = "blue")
        rug(x_clean, col = "red")
      }
    }

    # Method comparison plot
    method = "all"
    if (method == "all") {
      result$plots$overall$comparison <- function() {
        par(mfrow = c(3, 1), mar = c(4, 4, 2, 2))
        methods <- c("IQR", "Z-score", "Modified Z-score")

        if (!is.null(groups)) {
          # Plot for each method with groups
          for(i in 1:3) {
            plot(NULL, xlim = range(x_clean, na.rm = TRUE), ylim = c(0, 1),
                 main = paste(methods[i], "Method"),
                 xlab = "Value", ylab = "Group")

            unique_groups <- unique(groups)
            for(j in seq_along(unique_groups)) {
              group_data <- result$by_group[[unique_groups[j]]]
              outliers <- switch(i,
                                 group_data$iqr$is_outlier,
                                 group_data$zscore$is_outlier,
                                 group_data$modified_zscore$is_outlier)

              points(x_clean[groups == unique_groups[j]],
                     rep(j/length(unique_groups), sum(groups == unique_groups[j])),
                     pch = ifelse(outliers, 16, 1),
                     col = rainbow(length(unique_groups))[j])
            }
            axis(2, at = seq_along(unique_groups)/length(unique_groups),
                 labels = unique_groups)
          }
        } else {
          # Original comparison plot for ungrouped data
          outliers_list <- list(
            result$overall$iqr$is_outlier,
            result$overall$zscore$is_outlier,
            result$overall$modified_zscore$is_outlier
          )

          for(i in 1:3) {
            plot(x_clean[!is.na(x_clean)], rep(0, sum(!is.na(x_clean))),
                 pch = ifelse(outliers_list[[i]], 16, 1),
                 col = ifelse(outliers_list[[i]], "red", "black"),
                 main = paste(methods[i], "Method"),
                 xlab = "Value", ylab = "",
                 yaxt = "n")
            abline(h = 0, lty = 2, col = "gray")
          }
        }
        par(mfrow = c(1, 1))
      }
    }

    # Generate separate plots for each group if requested
    if (!is.null(groups) && plot_groups) {
      result$plots$by_group <- list()
      unique_groups <- unique(groups)

      for (group in unique_groups) {
        group_data <- x_clean[groups == group]
        result$plots$by_group[[group]] <- list()

        # Boxplot for group
        result$plots$by_group[[group]]$boxplot <- function() {
          boxplot(group_data, main = paste("Boxplot -", group),
                  ylab = "Value", col = "lightblue",
                  outline = TRUE)
          points(rep(1, length(group_data)), group_data,
                 col = "red", pch = 20)
        }

        # Density plot for group
        result$plots$by_group[[group]]$density <- function() {
          d <- density(group_data, na.rm = TRUE)
          plot(d, main = paste("Density Plot -", group),
               xlab = "Value", ylab = "Density")
          polygon(d, col = "lightblue", border = "blue")
          rug(group_data, col = "red")
        }
      }
    }
  }

  result
}
