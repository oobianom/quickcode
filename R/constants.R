#' Mathematical constants
#'
#' List of mathematical functions
#'
#' @note
#' \cr 1. Pi (π) - Approximately 3.14159
#' \cr 2. Euler's Number (e) - Approximately 2.71828
#' \cr 3. Golden Ratio (φ) - Approximately 1.61803
#' \cr 4. Euler-Mascheroni Constant (γ) - Approximately 0.57721
#' \cr 5. Feigenbaum Constant (δ) - Approximately 4.66920
#' \cr 6. Champernowne Constant (C10) - Approximately 0.123456789101112...
#' \cr 7. Apéry's Constant (ζ(3)) - Approximately 1.20205
#' \cr 8. Gelfond-Schneider Constant (eπ) - Approximately 22.45915
#' \cr 9. Khinchin's Constant (K) - Approximately 2.68545
#' \cr 10. Ramanujan-Soldner Constant (μ) - Approximately 1.45136
#'
#' @export

const <- list(

  pi = pi,

  e = exp(1),

  golden_ratio = (1 + sqrt(5)) / 2,

  euler_mascheroni = 0.5772156649,

  feigenbaum = 4.6692016091,

  champernowne = strsplit("0.123456789101112131415161718192021...", "")[[1]], # Champernowne Constant as a character vector

  apery = 1.2020569032,

  gelfond_schneider = exp(pi),

  khinchin = 2.685452001,

  ramanujan_soldner = 1.4513692348

)
