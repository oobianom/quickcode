# Future release

##' Check Multicollinearity Between Two Covariate Vectors
##'
##' This function calculates the Variance Inflation Factor (VIF) to assess multicollinearity
##' between two covariate vectors. A VIF value greater than 10 indicates high multicollinearity.
##'
##' @ param x A numeric vector representing the first covariate (e.g., AGE).
##' @ param y A numeric vector representing the second covariate (e.g., WEIGHT).
##' @ return A list containing the VIF value and a message about the level of multicollinearity.
##' @ importFrom car vif
##' @ export
##'
##' @ examples
##' # Example 1
##' age <- c(25, 30, 35, 40, 45)
##' weight <- c(60, 70, 80, 90, 100)
##' check_collinear(age, weight)
##'
##' # Example 2
##' age2 <- c(20, 22, 24, 26, 28)
##' weight2 <- c(50, 55, 53, 58, 60)
##' check_collinear(age2, weight2)
##'
##' # Example 3
##' age3 <- c(30, 35, 40, 45, 50)
##' weight3 <- c(70, 75, 80, 85, 90)
##' check_collinear(age3, weight3)
##'
##' # Example 4
##' age4 <- c(60, 62, 65, 67, 70)
##' weight4 <- c(80, 82, 85, 90, 95)
##' check_collinear(age4, weight4)
##'
##' # Example 5
##' age5 <- c(10, 15, 20, 25, 30)
##' weight5 <- c(30, 35, 40, 45, 50)
##' check_collinear(age5, weight5)
#'
#' check_collinear <- function(x, y) {
#'   # Combine variables into a data frame
#'   data <- data.frame(x = x, y = y)
#'   # Fit a linear model to check for multicollinearity
#'   model <- stats::lm(y ~ x, data = data)
#'   # Calculate VIF
#'   vif_value <- car::vif(model)
#'   # Check VIF value
#'   if (vif_value > 10) {
#'     message <- "High multicollinearity detected."
#'   } else {
#'     message <- "Multicollinearity is not a concern."
#'   }
#'   return(list(VIF = vif_value, Message = message))
#' }
