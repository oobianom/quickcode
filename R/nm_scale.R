#' Compute NONMEM S1 scaling from concentration, amount, and volume units
#'
#' @title Compute S1 scaling expression for NONMEM
#' @description
#' Given the units used in your dataset/model for the dependent variable (DV,
#' a concentration like "ng/mL"), the dosing amount (AMT, e.g. "mg") and the
#' model volume parameter V (e.g. "L" or "mL"), this function returns a robust
#' NONMEM-ready S1 expression so that \code{A(n)/S1} has the same units as DV.
#'
#' The function:
#' \itemize{
#'   \item parses common unit notations and synonyms (e.g. "ug", "mcg"),
#'   \item validates input and provides informative errors,
#'   \item computes the numeric factor such that \code{S1 = V * factor},
#'   \item returns a human-readable explanation and an optional numeric example.
#' }
#'
#' @param dv_unit Character scalar. DV concentration unit, e.g. "ng/mL", "mg/L", "ug per mL".
#' @param amt_unit Character scalar. AMT unit (mass), e.g. "mg", "ug", "ng".
#' @param v_unit Character scalar. Unit for model volume parameter V, e.g. "L", "mL".
#' @param sig_digits Integer scalar. Significant digits for formatted output (default = 8).
#' @param example Logical scalar. If TRUE, include a numeric example (default = FALSE).
#' @param example_amt Numeric scalar. Example AMT value (default = 1).
#' @param example_v Numeric scalar. Example V value (default = 1).
#'
#' @return A list with elements:
#' \describe{
#'   \item{factor}{Numeric. The multiplicative factor such that S1 = V * factor.}
#'   \item{S1_expression}{Character. NONMEM-ready expression (e.g. "S1 = V/1000").}
#'   \item{explanation}{Character. Human-readable explanation of the derivation.}
#'   \item{example}{Named list (present only when \code{example = TRUE}) with numeric example.}
#' }
#'
#' @examples
#' # Typical PK: DV in ng/mL, AMT in mg, V in L -> S1 = V * 1e+06 / 1000 = V * 1000
#' nonmem_scaling("ng/mL", "mg", "L")
#'
#' # If V is stored in mL:
#' nonmem_scaling("ng/mL", "mg", "mL")
#'
#' # With numeric example
#' nonmem_scaling("ng/mL", "mg", "L", example = TRUE, example_amt = 100, example_v = 40)
#'
#' # Microgram dosing
#' nonmem_scaling("pg/mL", "ug", "L")
#'
#' @export
nonmem_scaling <- function(dv_unit,
                          amt_unit,
                          v_unit,
                          sig_digits = 8L,
                          example = FALSE,
                          example_amt = 1,
                          example_v = 1) {


  # ===========================================================================

  # Input validation

  # ===========================================================================

  .assert_scalar_char <- function(x, arg_name) {
    if (missing(x) || is.null(x) || length(x) != 1L || !is.character(x) || is.na(x) || nchar(trimws(x)) == 0L) {
      stop(sprintf("'%s' must be a non-empty character scalar.", arg_name), call. = FALSE)
    }
  }
  .assert_scalar_char(dv_unit, "dv_unit")
  .assert_scalar_char(amt_unit, "amt_unit")
  .assert_scalar_char(v_unit, "v_unit")


  if (!is.numeric(sig_digits) || length(sig_digits) != 1L || sig_digits < 1L) {
    stop("'sig_digits' must be a positive integer.", call. = FALSE)
  }
  sig_digits <- as.integer(sig_digits)


  if (!is.logical(example) || length(example) != 1L || is.na(example)) {
    stop("'example' must be TRUE or FALSE.", call. = FALSE)
  }
  if (isTRUE(example)) {
    if (!is.numeric(example_amt) || length(example_amt) != 1L || !is.finite(example_amt) || example_amt <= 0) {
      stop("'example_amt' must be a positive finite number.", call. = FALSE)
    }
    if (!is.numeric(example_v) || length(example_v) != 1L || !is.finite(example_v) || example_v <= 0) {
      stop("'example_v' must be a positive finite number.", call. = FALSE)
    }
  }


  # ===========================================================================

  # Normalization helper
  # ===========================================================================
  .normalize_unit <- function(x) {
    x <- tolower(trimws(as.character(x)))
    # Remove all whitespace
    x <- gsub("\\s+", "", x, perl = TRUE)
    # Normalize unicode micro signs to 'u'
    x <- gsub("\u00B5", "u", x, fixed = TRUE)
    x <- gsub("\u03BC", "u", x, fixed = TRUE)
    # Normalize 'mcg' -> 'ug'
    x <- sub("^mcg", "ug", x)
    # Remove trailing dots
    x <- gsub("\\.$", "", x)
    x
  }

  dv_unit_norm  <- .normalize_unit(dv_unit)
  amt_unit_norm <- .normalize_unit(amt_unit)
  v_unit_norm   <- .normalize_unit(v_unit)

  # ===========================================================================
  # Parse DV into mass/volume components
  # ===========================================================================
  .split_concentration <- function(x, original) {
    # Try separators in priority order: "/", " per ", "per"
    sep <- NULL
    if (grepl("/", x, fixed = TRUE)) {
      sep <- "/"
    } else if (grepl("per", x, fixed = TRUE)) {
      sep <- "per"
    }
    if (is.null(sep)) {
      stop(sprintf(
        "Cannot parse DV unit '%s' as a concentration. Expected format: 'mass/volume' (e.g. 'ng/mL', 'mg per L').",
        original
      ), call. = FALSE)
    }
    parts <- strsplit(x, sep, fixed = TRUE)[[1L]]
    parts <- trimws(parts)
    parts <- parts[nchar(parts) > 0L]
    if (length(parts) != 2L) {
      stop(sprintf(
        "Cannot parse DV unit '%s' into exactly two components (mass and volume). Got: [%s].",
        original, paste(parts, collapse = ", ")
      ), call. = FALSE)
    }
    list(mass = parts[1L], volume = parts[2L])
  }

  dv_parsed <- .split_concentration(dv_unit_norm, dv_unit)
  dv_mass_str <- dv_parsed$mass
  dv_vol_str  <- dv_parsed$volume

  # ===========================================================================
  # Canonical unit maps
  # Mass: value = grams per 1 unit
  # Volume: value = milliliters per 1 unit
  # ===========================================================================
  mass_map <- c(
    kg = 1e3,
    g  = 1,
    mg = 1e-3,
    ug = 1e-6,
    ng = 1e-9,
    pg = 1e-12,
    fg = 1e-15

  )

  vol_map <- c(
    kl = 1e6,
    l  = 1e3,
    dl = 1e2,
    cl = 10,
    ml = 1,
    ul = 1e-3,
    nl = 1e-6,
    cc = 1        # 1 cc = 1 mL
  )

  # ===========================================================================

  # Resolve unit strings to canonical keys
  # ===========================================================================
  .resolve_mass <- function(x, original_input) {
    # Direct match
    if (x %in% names(mass_map)) return(x)
    # Common synonyms / long forms
    x2 <- x
    x2 <- sub("^micrograms?$", "ug", x2)
    x2 <- sub("^milligrams?$", "mg", x2)
    x2 <- sub("^nanograms?$", "ng", x2)
    x2 <- sub("^picograms?$", "pg", x2)
    x2 <- sub("^femtograms?$", "fg", x2)
    x2 <- sub("^kilograms?$", "kg", x2)
    x2 <- sub("^grams?$", "g", x2)
    if (x2 %in% names(mass_map)) return(x2)
    # Strip trailing 's' for plurals
    x3 <- sub("s$", "", x2)
    if (x3 %in% names(mass_map)) return(x3)
    stop(sprintf(
      "Unknown mass unit: '%s' (from input '%s'). Supported: %s.",
      x, original_input, paste(names(mass_map), collapse = ", ")
    ), call. = FALSE)
  }

  .resolve_volume <- function(x, original_input) {
    # Direct match
    if (x %in% names(vol_map)) return(x)
    # Common synonyms / long forms
    x2 <- x
    x2 <- sub("^lit(er|re)s?$", "l", x2)
    x2 <- sub("^millilit(er|re)s?$", "ml", x2)
    x2 <- sub("^microlit(er|re)s?$", "ul", x2)
    x2 <- sub("^decilit(er|re)s?$", "dl", x2)
    x2 <- sub("^centilit(er|re)s?$", "cl", x2)
    x2 <- sub("^nanolit(er|re)s?$", "nl", x2)
    x2 <- sub("^kilolit(er|re)s?$", "kl", x2)
    if (x2 %in% names(vol_map)) return(x2)
    # Strip trailing 's'
    x3 <- sub("s$", "", x2)
    if (x3 %in% names(vol_map)) return(x3)
    stop(sprintf(
      "Unknown volume unit: '%s' (from input '%s'). Supported: %s.",
      x, original_input, paste(names(vol_map), collapse = ", ")
    ), call. = FALSE)
  }

  amt_key        <- .resolve_mass(amt_unit_norm, amt_unit)
  dv_mass_key    <- .resolve_mass(dv_mass_str, dv_unit)
  v_key          <- .resolve_volume(v_unit_norm, v_unit)
  dv_vol_key     <- .resolve_volume(dv_vol_str, dv_unit)

  # ===========================================================================
  # Compute scaling factor
  # ---------------------------------------------------------------------------
  # In NONMEM, A(n) is in AMT units. We need:
  #   A(n) / S1 = concentration in DV units
  #   => S1 = A(n) / C_dv
  #
  # If A = 1 amt_unit in compartment with volume V (in v_units):

  #   True concentration = (1 amt_unit) / (V v_units)
  #   Convert to DV units:
  #     mass conversion: 1 amt_unit = (g_per_amt / g_per_dv_mass) dv_mass_units
  #     vol conversion:  1 v_unit   = (mL_per_v / mL_per_dv_vol) dv_vol_units
  #
  #   C_dv = f_mass / (V * f_vol)   ... per unit AMT
  #   => S1 = V * f_vol / f_mass
  # ===========================================================================
  g_per_amt     <- mass_map[[amt_key]]
  g_per_dv_mass <- mass_map[[dv_mass_key]]
  mL_per_v      <- vol_map[[v_key]]
  mL_per_dv_vol <- vol_map[[dv_vol_key]]

  # f_mass: how many DV-mass-units per 1 AMT-unit
  f_mass <- g_per_amt / g_per_dv_mass
  # f_vol: how many DV-volume-units per 1 V-unit
  f_vol  <- mL_per_v / mL_per_dv_vol

  factor <- f_vol / f_mass

  if (!is.finite(factor) || factor <= 0) {
    stop("Computed scaling factor is non-positive or non-finite. Please verify unit inputs.", call. = FALSE)
  }

  # ===========================================================================
  # Format the S1 expression for NONMEM
  # ===========================================================================
  .is_power_of_10 <- function(x, tol = 1e-12) {
    if (x <= 0) return(list(yes = FALSE))
    log_val <- log10(x)
    if (abs(log_val - round(log_val)) < tol) {
      return(list(yes = TRUE, exponent = as.integer(round(log_val))))
    }
    list(yes = FALSE)
  }

  .format_expression <- function(fac) {
    # Check if factor is exactly 1
    if (abs(fac - 1) < .Machine$double.eps^0.5) {
      return("S1 = V")
    }

    p10 <- .is_power_of_10(fac)
    if (p10$yes) {
      exp_val <- p10$exponent
      if (exp_val == 0L) return("S1 = V")
      if (exp_val > 0L) {
        # e.g. factor = 1000 -> "S1 = V * 1000"
        return(sprintf("S1 = V * %s", format(10^exp_val, scientific = FALSE)))
      } else {
        # e.g. factor = 0.001 -> "S1 = V / 1000"
        return(sprintf("S1 = V / %s", format(10^(-exp_val), scientific = FALSE)))
      }
    }

    # Check if 1/factor is a nice integer (for division form)
    inv <- 1 / fac
    p10_inv <- .is_power_of_10(inv)
    if (p10_inv$yes && p10_inv$exponent > 0L) {
      return(sprintf("S1 = V / %s", format(10^(p10_inv$exponent), scientific = FALSE)))
    }

    # Check if factor itself is a nice integer
    if (abs(fac - round(fac)) < 1e-9 && fac < 1e12) {
      return(sprintf("S1 = V * %s", format(round(fac), scientific = FALSE)))
    }

    # General case: use signif
    fstr <- formatC(fac, digits = sig_digits, format = "g")
    sprintf("S1 = V * %s", fstr)
  }

  expr_str <- .format_expression(factor)

  # ===========================================================================
  # Build explanation
  # ===========================================================================
  explanation <- paste0(
    "Goal: A(n)/S1 yields concentration in [", dv_mass_str, "/", dv_vol_str, "].\n",
    "\n",
    "Unit conversions:\n",
    "  - 1 ", amt_unit, " = ", signif(f_mass, sig_digits), " ", dv_mass_str, "\n",
    "  - 1 ", v_unit,   " = ", signif(f_vol, sig_digits),  " ", dv_vol_str, "\n",
    "\n",
    "Derivation:\n",
    "  C [", dv_mass_str, "/", dv_vol_str, "] = A [", amt_unit, "] * f_mass / (V [", v_unit, "] * f_vol)\n",
    "  => S1 = V * f_vol / f_mass = V * ", signif(f_vol, sig_digits), " / ", signif(f_mass, sig_digits), "\n",
    "  => S1 = V * ", signif(factor, sig_digits), "\n",
    "\n",
    "NONMEM code:\n",
    "  ", expr_str, "\n"
  )

  # ===========================================================================
  # Optional numeric example
  # ===========================================================================
  example_out <- NULL
  if (isTRUE(example)) {
    # C_dv = AMT * f_mass / (V * f_vol)
    conc <- (example_amt * f_mass) / (example_v * f_vol)
    # Verify via S1: A/S1 = AMT / (V * factor) should equal conc
    conc_via_s1 <- example_amt / (example_v * factor)
    example_out <- list(
      AMT            = example_amt,
      AMT_unit       = amt_unit,
      V              = example_v,
      V_unit         = v_unit,
      S1             = example_v * factor,
      concentration  = conc,
      conc_via_S1    = conc_via_s1,
      DV_unit        = paste0(dv_mass_str, "/", dv_vol_str),
      note           = sprintf(
        "A dose of %s %s distributed in V = %s %s gives C = %s %s/%s.",
        signif(example_amt, sig_digits), amt_unit,
        signif(example_v, sig_digits), v_unit,
        signif(conc, sig_digits), dv_mass_str, dv_vol_str
      )
    )
  }

  # ===========================================================================
  # Return
  # ===========================================================================
  structure(
    list(
      factor        = factor,
      S1_expression = expr_str,
      explanation   = explanation,
      example       = example_out
    ),
    class = "nonmem_scaling"
  )
}


#' Print method for s1_scaling objects
#' @param x An object of class \code{s1_scaling}.
#' @param ... Additional arguments (ignored).
print.nonmem_scaling <- function(x, ...) {
  cat("--- NONMEM S1 Scaling ---\n\n")
  cat(x$explanation)
  if (!is.null(x$example)) {
    cat("\nExample:\n")
    cat("  ", x$example$note, "\n")
  }
  cat("\n")
  invisible(x)
}

