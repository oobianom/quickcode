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
#' @rdname outlierdetect1
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
detect_outlier <- function(x, method = "iqr", multiplier = 1.5, z_threshold = 3, na.rm = TRUE, groups = NULL, summary = FALSE) {
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

  return(result)
}









#' Advanced Outlier Detection in Numeric Data with Optional Grouping
#'
#' @description
#' Detects outliers in numeric data using multiple statistical methods and provides
#' comprehensive analysis including visualizations and summary statistics. Supports
#' grouped analysis when group vector is provided. The function supports three
#' detection methods: IQR, Z-score, and Modified Z-score.
#'
#' @param x A numeric vector containing the data to analyze
#' @param groups Optional vector of group labels, must be same length as x
#' @param method Character string specifying the outlier detection method.
#'   Options are "iqr", "zscore", "modified_zscore", or "all". Default is "iqr"
#' @param multiplier Numeric value specifying the IQR multiplier for
#'   outlier detection. Default is 1.5
#' @param z_threshold Numeric value specifying the Z-score threshold.
#'   Default is 3
#' @param modified_z_threshold Numeric value specifying the modified Z-score
#'   threshold. Default is 3.5
#' @param plot Logical indicating whether to generate visualization plots.
#'   Default is TRUE
#' @param plot_groups Logical indicating whether to create separate plots for each
#'   group. Only used when groups are provided. Default is TRUE
#'
#' @return A list of class "outlier_analysis" containing:
#'   \itemize{
#'     \item overall: Overall analysis results
#'     \item by_group: Group-specific results (if groups provided)
#'     \item plots: List of generated plots
#'   }
#'
#' @examples
#' # Example 1: Basic grouped analysis
#' set.seed(123)
#' data <- c(rnorm(50), rnorm(50, 2), rnorm(50, 4))
#' groups <- rep(c("A", "B", "C"), each = 50)
#' resultA <- detect_outlier2(data) # no groups
#' resultB <- detect_outlier2(data, groups = groups) # with groups
#'
#' # Example 2: Custom thresholds by group
#' test_scores <- c(65, 70, 75, 72, 68, 73, 78, 71, 69, 74,
#'                  90, 85, 92, 88, 95, 87, 91, 89, 86, 93)
#' class_groups <- rep(c("Morning", "Afternoon"), each = 10)
#' result <- detect_outlier2(test_scores,
#'                                    groups = class_groups,
#'                                    method = "all",
#'                                    z_threshold = 2)
#' result$overall
#' result$by_group
#' result$plots$overall$boxplot()
#' result$plots$overall$density()
#' result$plots$overall$comparison()
#' result$plots$by_group$Morning$boxplot()
#' result$plots$by_group$Morning$density()
#' result$plots$by_group$Afternoon$boxplot()
#' result$plots$by_group$Afternoon$density()
#'
#' @export
detect_outlier2 <- function(x,
                            groups = NULL,
                            method = c("iqr", "zscore", "modified_zscore", "all"),
                            multiplier = 1.5,
                            z_threshold = 3,
                            modified_z_threshold = 3.5,
                            plot = TRUE,
                            plot_groups = TRUE) {
  # Input validation
  if (!is.numeric(x)) {
    stop("Input must be a numeric vector")
  }

  if (!is.null(groups) && length(groups) != length(x)) {
    stop("groups must be the same length as x")
  }

  method <- match.arg(method)

  # Initialize results list
  results <- list()

  # Analyze overall data
  results$overall <- analyze_dataset(x, m = multiplier, m2 = method, zt = z_threshold,mzt = modified_z_threshold)

  # Analyze by group if groups provided
  if (!is.null(groups)) {
    results$by_group <- lapply(split(x, groups), function(data) analyze_dataset(data, m = multiplier, m2 = method,zt = z_threshold,mzt = modified_z_threshold))
  }

  # Generate plots
  if (plot) {
    results$plots <- list()

    # Overall plots
    results$plots$overall <- list()

    # Boxplot
    results$plots$overall$boxplot <- function() {
      if (!is.null(groups)) {
        boxplot(split(x, groups), main = "Boxplot by Group",
                ylab = "Value", col = "lightblue",
                outline = TRUE)
      } else {
        boxplot(x, main = "Boxplot with Outliers",
                ylab = "Value", col = "lightblue",
                outline = TRUE)
        points(rep(1, length(x)), x, col = "red", pch = 20)
      }
    }

    # Density plot
    results$plots$overall$density <- function() {
      if (!is.null(groups)) {
        # Setup plot
        plot(NULL, xlim = range(x, na.rm = TRUE),
             ylim = c(0, max(sapply(split(x, groups), function(g) {
               max(density(g, na.rm = TRUE)$y)
             }))),
             main = "Density Plot by Group",
             xlab = "Value", ylab = "Density")

        # Plot each group with different colors
        colors <- rainbow(length(unique(groups)))
        for (i in seq_along(unique(groups))) {
          group_data <- x[groups == unique(groups)[i]]
          d <- density(group_data, na.rm = TRUE)
          lines(d, col = colors[i])
          rug(group_data, col = colors[i])
        }
        legend("topright", legend = unique(groups),
               col = colors, lty = 1)
      } else {
        d <- density(x, na.rm = TRUE)
        plot(d, main = "Density Plot of Data",
             xlab = "Value", ylab = "Density")
        polygon(d, col = "lightblue", border = "blue")
        rug(x, col = "red")
      }
    }

    # Method comparison plot
    if (method == "all") {
      results$plots$overall$comparison <- function() {
        par(mfrow = c(3, 1), mar = c(4, 4, 2, 2))
        methods <- c("IQR", "Z-score", "Modified Z-score")

        if (!is.null(groups)) {
          # Plot for each method with groups
          for(i in 1:3) {
            plot(NULL, xlim = range(x, na.rm = TRUE), ylim = c(0, 1),
                 main = paste(methods[i], "Method"),
                 xlab = "Value", ylab = "Group")

            unique_groups <- unique(groups)
            for(j in seq_along(unique_groups)) {
              group_data <- results$by_group[[unique_groups[j]]]
              outliers <- switch(i,
                                 group_data$iqr$is_outlier,
                                 group_data$zscore$is_outlier,
                                 group_data$modified_zscore$is_outlier)

              points(x[groups == unique_groups[j]],
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
            results$overall$iqr$is_outlier,
            results$overall$zscore$is_outlier,
            results$overall$modified_zscore$is_outlier
          )

          for(i in 1:3) {
            plot(x[!is.na(x)], rep(0, sum(!is.na(x))),
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
      results$plots$by_group <- list()
      unique_groups <- unique(groups)

      for (group in unique_groups) {
        group_data <- x[groups == group]
        results$plots$by_group[[group]] <- list()

        # Boxplot for group
        results$plots$by_group[[group]]$boxplot <- function() {
          boxplot(group_data, main = paste("Boxplot -", group),
                  ylab = "Value", col = "lightblue",
                  outline = TRUE)
          points(rep(1, length(group_data)), group_data,
                 col = "red", pch = 20)
        }

        # Density plot for group
        results$plots$by_group[[group]]$density <- function() {
          d <- density(group_data, na.rm = TRUE)
          plot(d, main = paste("Density Plot -", group),
               xlab = "Value", ylab = "Density")
          polygon(d, col = "lightblue", border = "blue")
          rug(group_data, col = "red")
        }
      }
    }
  }

  class(results) <- "outlier_analysis"
  return(results)
}


#' @rdname outlierdetect1
#' @export
iqr_outlier <- function(x, multiplier) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - multiplier * iqr
  upper_bound <- q3 + multiplier * iqr

  list(
    outliers = x[x < lower_bound | x > upper_bound],
    indices = which(x < lower_bound | x > upper_bound),
    bounds = c(lower = lower_bound, upper = upper_bound),
    is_outlier = x < lower_bound | x > upper_bound
  )
}
#' @rdname outlierdetect1
#' @export
zscore_outlier <- function(x, z_threshold) {
  z_scores <- scale(x)
  list(
    outliers = x[abs(z_scores) > z_threshold],
    indices = which(abs(z_scores) > z_threshold),
    z_scores = z_scores,
    is_outlier = abs(z_scores) > z_threshold
  )
}
#' @rdname outlierdetect1
#' @export
zscore_outlier2 <- function(x, z_threshold) {
  median_x <- median(x, na.rm = TRUE)
  mad_x <- mad(x, na.rm = TRUE)
  modified_z_scores <- 0.6745 * (x - median_x) / mad_x

  list(
    outliers = x[abs(modified_z_scores) > z_threshold],
    indices = which(abs(modified_z_scores) > z_threshold),
    modified_z_scores = modified_z_scores,
    is_outlier = abs(modified_z_scores) > z_threshold
  )
}

# Function to compute summary statistics
compute_summary <- function(x) {
  list(
    n = length(x),
    na_count = sum(is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    skewness = if(length(x) > 2) {
      sum((x - mean(x, na.rm = TRUE))^3, na.rm = TRUE) /
        (length(x[!is.na(x)]) * sd(x, na.rm = TRUE)^3)
    } else NA,
    kurtosis = if(length(x) > 2) {
      sum((x - mean(x, na.rm = TRUE))^4, na.rm = TRUE) /
        (length(x[!is.na(x)]) * sd(x, na.rm = TRUE)^4) - 3
    } else NA
  )
}

# Function to analyze a single dataset
analyze_dataset <- function(data, m,zt,mzt,m2) {
  data_clean <- data[!is.na(data)]
  result <- list()
  result$summary <- compute_summary(data)

  # Apply selected m(s)
  if (m2 %in% c("iqr", "all")) {
    result$iqr <- iqr_outlier(data_clean, m)
  }
  if (m2 %in% c("zscore", "all")) {
    result$zscore <- zscore_outlier(data_clean, zt)
  }
  if (m2 %in% c("modified_zscore", "all")) {
    result$modified_zscore <- zscore_outlier2(data_clean, mzt)
  }

  # Create comparison if multiple ms used
  if (m2 == "all") {
    result$comparison <- data.frame(
      value = data_clean,
      iqr_outlier = result$iqr$is_outlier,
      zscore_outlier = result$zscore$is_outlier,
      modified_zscore_outlier = result$modified_zscore$is_outlier,
      methods_agreeing = rowSums(cbind(
        result$iqr$is_outlier,
        result$zscore$is_outlier,
        result$modified_zscore$is_outlier
      ))
    )
  }

  return(result)
}

