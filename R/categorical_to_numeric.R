#' Convert Categorical Values to Numeric Improvement to seq_along()
#'
#' @description
#' Converts categorical values into numeric by sorting unique values
#' alphabetically/numerically and assigning sequential numbers starting from 1
#'
#' @param x A vector containing categorical values (character or factor)
#' @param decreasing logical. Should the sort be increasing or decreasing?
#'
#' @return A numeric vector where each category is mapped to a number based on its
#' sorted position. NA values in the input remain NA in the output.
#'
#' @examples
#' # Example 1: Basic character vector
#' fruits <- c("apple", "banana", "apple", "cherry", "banana")
#' cat_to_num(fruits, decreasing = FALSE)  # Returns: 1, 2, 1, 3, 2
#'
#' # Example 2: Vector with NAs
#' grades <- c("A", NA, "C", "B", "A", NA)
#' cat_to_num(grades)  # Returns: 1, NA, 3, 2, 1, NA
#'
#' # Example 3: Factor input
#' animals <- factor(c("dog", "cat", "bird", "cat", "dog"))
#' cat_to_num(animals)  # Returns: 3, 2, 1, 2, 3
#'
#' # Example 4: Numeric categories
#' sizes <- c("XL", "S", "M", "XS", "L")
#' cat_to_num(sizes)  # Returns: 5, 2, 3, 1, 4
#'
#' # Example 5: Single category
#' status <- c("active", "active", "active")
#' cat_to_num(status)  # Returns: 1, 1, 1
#'
#' @export
#' @rdname categorical_to_numeric
cat_to_num <- function(x, decreasing = FALSE) {
  # Remove NA values and get unique sorted values
  unique_vals <- sort(unique(stats::na.omit(x)), decreasing = decreasing)

  # Create a named vector mapping categories to numbers
  val_map <- seq_along(unique_vals)
  names(val_map) <- unique_vals

  # Convert to numeric while preserving NA
  val_map[x]
}

#' @export
#' @examples
#' # Examples for cat_to_num2
#'
#' # Assign numeric values to a set of categorical values
#' # Call the value assignment to assign
#' # a numeric value to a new categorical value
#'
#' cat_to_num2(letters[1:7])("b")
#' cat_to_num2(letters[1:7], decreasing = FALSE)("d")
#' # the above assigns numeric values to letters a-g
#' # and then calls the number for "a"
#'
#' cat_vals_5 <- c("apple","orange","guava")
#' num_equiv <- cat_to_num2(cat_vals_5)
#'
#' applenum = num_equiv("apple")
#'
#'
#' @rdname categorical_to_numeric
cat_to_num2 <- function(x, decreasing = FALSE){
  # assign numeric values
  getassignment <- cat_to_num(x, decreasing = decreasing)

  # set function for subsequent calls
  function(val){
    stopifnot(val %in% x)
    as.numeric(getassignment[val])
  }
}
