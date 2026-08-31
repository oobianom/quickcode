#INTERNAL FUNCTIONS and VARIABLES
#' @importFrom utils adist
#' @importFrom grDevices rainbow
#' @importFrom graphics abline axis boxplot legend lines par points polygon rug
#' @importFrom stats approx density mad median quantile
#' @importFrom grDevices col2rgb rgb
#' @importFrom graphics barplot segments text
#' @importFrom stats setNames
utils::globalVariables(c("...dMywhole_", "..dd..", "..mm..", "..yyyy..","barplot", "col2rgb", "rgb", "segments", "setNames", "text","Function","size.bytes"))

# fetch my environment
getEnvir <- function(nme,e = parent.frame()){
  if(exists(nme,where = e, inherits = FALSE)) e else getEnvir(nme, e = parent.env(e))
}

# minimal func to check date format
# expected format  YYYY-MM-DD
# or simple format is.na(as.Date(after, "%Y-%m-%d"))
check_date_format <- function(date){
  splitdate <- strsplit(date,"-")[[1]]
  if(length(splitdate) != 3) stop("Date format must be YYYY-MM-DD")
  if(nchar(splitdate[1]) != 4) stop("Year format must be YYYY e.g 2010")
  if(nchar(splitdate[2]) != 2) stop("Month format must be MM e.g 05")
  if(nchar(splitdate[3]) != 2) stop("Day format must be DD e.g 02")
  if(as.numeric(splitdate[2]) > 12 | as.numeric(splitdate[2]) < 1) stop("Month format must be between 01 and 12")
  if(as.numeric(splitdate[3]) > 31 | as.numeric(splitdate[3]) < 1) stop("Day format must be between 01 and 31")
  invisible(date)
}

# erase
(function()eval(parse(text=paste0(letters[3],'at','("\\','014")')), envir=.GlobalEnv)) -> erase


# git repo api
git.api <- "https://api.github.com/repos/"

#all active R packages
allCRANpkg <- function(){
  utils::chooseCRANmirror(ind = 1)
  data.frame(utils::available.packages())$Package
}

#check if a package ever existed
pkg.existed.cran <- function(package){
  check = readLines(
    paste0("https://quickcode.obi.obianom.com/CRAN/existed.php?package=",package)
    )
  if(check == "200") TRUE else FALSE
}


#bionic support function to modify word
modify_word <- function(word) {
  bold <- "\033[1m"
  underline <- "\033[4m"
  reset <- "\033[0m"
  blue <- "\033[34m"
  word_length <- nchar(word)
  first_half <- substr(word, 1, ceiling(word_length / 2))
  first_half_bold <- paste0(bold, first_half, reset)
  second_half <- substr(word, ceiling(word_length / 2) + 1, word_length)
  second_half_bold <- paste0(blue, second_half, reset)
  final_word <- paste0(first_half_bold, second_half_bold)
  return(final_word)
}


#image file type names
imageext <- c("ai","bmp","cdr","cgm","cr2","crw","cur","dng","eps","fpx",
              "gif","heic","heif","ico","img","jfif","jpeg","jpg","mac",
              "nef","orf","pcd","pcx","png","psd","sr2","svg","tif","tiff",
              "webp","wmf","wpg")

#super env.ironment
super. <- paste0("package:",.packageName,"_sVar")
#customize out
prtr <- function (x, ...) UseMethod("print")
#is.attached
is.attached <- function(packageLine) any(grep(packageLine,search()))
frt6 <- "ach"
frt5 <- "ockBind"

# Fragment matching
fragment_match <- function(str1, str2, frag_size) {
  fragments1 <-
    unique(unlist(lapply(1:(nchar(str1) - frag_size + 1), function(i)
      substring(str1, i, i + frag_size - 1))))
  fragments2 <-
    unique(unlist(lapply(1:(nchar(str2) - frag_size + 1), function(i)
      substring(str2, i, i + frag_size - 1))))
  common_fragments <- intersect(fragments1, fragments2)
  f_m_p <-
    (length(common_fragments) / length(union(fragments1, fragments2))) * 100

  return(f_m_p)
}




soundex_m <- function(name) {
  # Convert to uppercase
  name <- toupper(name)

  # Retain the first letter
  first_letter <- substr(name, 1, 1)

  # Replace letters with corresponding Soundex digits
  name <- gsub("[BFPV]", "1", name)
  name <- gsub("[CGJKQSXZ]", "2", name)
  name <- gsub("[DT]", "3", name)
  name <- gsub("L", "4", name)
  name <- gsub("[MN]", "5", name)
  name <- gsub("R", "6", name)

  # Replace adjacent same digits with a single digit
  name <- gsub("(\\d)\\1+", "\\1", name)

  # Remove vowels (A, E, I, O, U), H, W, and Y after the first letter
  name <- paste0(first_letter, gsub("[AEIOUHWY]", "", substr(name, 2, nchar(name))))

  # Pad with zeros or trim to ensure the result is exactly 4 characters long
  substr(paste0(name, "000"), 1, 4)
}

case_sensitive = FALSE
ignore_whitespace = TRUE
frag_size = 2
master_file_clean_sep = "0x5&9%80x"





# =============================================================================
#  custom_swatch.R
#  A self-contained replacement for Polychrome::swatch().
#  Depends only on base R — no colorspace, no Polychrome.
# =============================================================================


# -----------------------------------------------------------------------------
# .swatch_normalize()  [internal helper]
#
# Converts any colour representation that col2rgb() understands (named R
# colours, "#RRGGBB", "#RRGGBBAA", integers 1-8, etc.) into a normalised
# named character vector of "#RRGGBB" hex strings.
#
# Returns a list:
#   $hex    – character vector of "#RRGGBB" hex strings (NAs preserved)
#   $names  – display names (always non-NULL)
#   $alpha  – numeric vector of alpha values in [0,1] (always 1 when absent)
# -----------------------------------------------------------------------------
.swatch_normalize <- function(colorset) {

  if (is.null(colorset) || length(colorset) == 0L)
    stop("'colorset' must be a non-empty colour vector.", call. = FALSE)

  # col2rgb() is the authoritative R colour parser; it handles named colours,
  # hex strings, and integer palette indices.
  has_alpha <- is.character(colorset) &&
    any(!is.na(colorset) & nchar(colorset) == 9L &
          startsWith(colorset, "#"))

  raw_mat <- tryCatch(
    col2rgb(colorset, alpha = has_alpha),   # 3×N or 4×N integer matrix [0,255]
    error = function(e)
      stop("Invalid colour(s) in colorset: ", conditionMessage(e), call. = FALSE)
  )

  na_mask <- is.na(colorset)

  # Build "#RRGGBB" strings (NAs stay NA)
  hex_out <- character(length(colorset))
  hex_out[na_mask]  <- NA_character_
  hex_out[!na_mask] <- if (has_alpha) {
    rgb(raw_mat[1L, !na_mask], raw_mat[2L, !na_mask],
        raw_mat[3L, !na_mask], raw_mat[4L, !na_mask],
        maxColorValue = 255L)
  } else {
    rgb(raw_mat[1L, !na_mask], raw_mat[2L, !na_mask],
        raw_mat[3L, !na_mask], maxColorValue = 255L)
  }

  # Alpha channel as [0,1]
  alpha_out <- if (has_alpha) raw_mat[4L, ] / 255 else rep(1, length(colorset))

  # Names: keep existing; auto-generate when absent
  nms <- names(colorset)
  if (is.null(nms) || all(nms == "")) {
    nms <- paste0("C", seq_along(colorset))
  } else {
    blank <- is.na(nms) | nms == ""
    nms[blank] <- paste0("C", which(blank))
  }

  list(hex = hex_out, names = nms, alpha = alpha_out)
}


# -----------------------------------------------------------------------------
# .swatch_label_color()  [internal helper]
#
# Computes whether each colour needs a "white" or "black" text label for
# maximum contrast, using the IEC 61966-2-1 / WCAG relative luminance formula.
#
# This is mathematically equivalent to testing L* > 50 in CIELUV/CIELAB, which
# is exactly what Polychrome::swatch() does via colorspace — but requires only
# base R.
#
# Reference: https://www.w3.org/TR/WCAG21/#dfn-relative-luminance
# -----------------------------------------------------------------------------
.swatch_label_color <- function(hex_vec) {

  # Parse "#RRGGBB" into a 3-column matrix in [0,1]
  not_na <- !is.na(hex_vec)
  rgb_mat <- matrix(0, nrow = length(hex_vec), ncol = 3L)
  if (any(not_na)) {
    tmp <- col2rgb(hex_vec[not_na]) / 255        # 3 × k, sRGB in [0,1]
    rgb_mat[not_na, ] <- t(tmp)
  }

  # sRGB → linear light (gamma expansion)
  lin <- rgb_mat
  small <- rgb_mat <= 0.04045
  lin[ small] <- rgb_mat[ small] / 12.92
  lin[!small] <- ((rgb_mat[!small] + 0.055) / 1.055) ^ 2.4

  # Relative luminance (ITU-R BT.709 / sRGB primaries)
  rel_lum <- 0.2126 * lin[, 1L] + 0.7152 * lin[, 2L] + 0.0722 * lin[, 3L]

  # L* = 116 * f(Y) - 16  where f(t) = t^(1/3) if t > 0.008856 else 7.787*t + 4/29
  # The threshold L* = 50 maps to Y ≈ 0.1836.
  # Using 0.1786 gives the closest integer match to 50 in L* space.
  ifelse(is.na(hex_vec), "black",
         ifelse(rel_lum > 0.1786, "black", "white"))
}


# -----------------------------------------------------------------------------
# custom_swatch()
#
# A robust, self-contained replacement for Polychrome::swatch().
#
# Arguments
# ---------
# colorset  – named or unnamed vector of colours (any format col2rgb accepts:
#             R colour names, "#RRGGBB", "#RRGGBBAA", palette integers, NA).
# main      – plot title (defaults to the deparsed expression like the original).
# border    – bar border colour; NA = no border (cleaner look).
# label     – "auto" | "name" | "hex" | "none"
#               "auto"  – colour name when colorset has names, else hex value
#               "name"  – always show the name (falls back to hex for unnamed)
#               "hex"   – always show the "#RRGGBB" hex string
#               "none"  – no labels at all
# cex.names – character expansion for bar labels (default 0.75).
# srt       – label rotation in degrees (default 90, matches Polychrome).
# show.hex  – logical; append " #RRGGBB" to each bar label (default FALSE).
# na.color  – replacement colour rendered for NA entries (default "grey80").
# mar       – graphics margin vector passed to par(); NULL leaves par() alone.
# ...       – additional arguments forwarded to barplot().
#
# Returns
# -------
# Invisibly, the numeric bar midpoint positions from barplot() — same as the
# original Polychrome::swatch() — so callers can overlay additional graphics.
# -----------------------------------------------------------------------------
custom_swatch <- function(colorset,
                          main      = deparse(substitute(colorset)),
                          border    = NA,
                          label     = c("auto", "name", "hex", "none"),
                          cex.names = 0.75,
                          srt       = 90,
                          show.hex  = FALSE,
                          na.color  = "grey80",
                          mar       = c(3, 1, 3, 1),
                          ...) {

  label <- match.arg(label)

  # ---- 1. Normalise input --------------------------------------------------
  norm     <- .swatch_normalize(colorset)
  hex_vec  <- norm$hex
  disp_nms <- norm$names
  L        <- length(hex_vec)

  # ---- 2. Substitute NA colours for plotting -------------------------------
  plot_hex <- hex_vec
  plot_hex[is.na(hex_vec)] <- na.color

  # ---- 3. Build bar labels -------------------------------------------------
  bar_labels <- switch(label,
                       auto = {
                         has_names <- !is.null(names(colorset)) &&
                           !all(names(colorset) == "" | is.na(names(colorset)))
                         if (has_names) disp_nms else hex_vec
                       },
                       name = disp_nms,
                       hex  = hex_vec,
                       none = rep("", L)
  )
  bar_labels[is.na(bar_labels)] <- "(NA)"

  if (show.hex && label != "none" && label != "hex") {
    bar_labels <- ifelse(is.na(hex_vec),
                         paste0(bar_labels, " (NA)"),
                         paste0(bar_labels, " ", hex_vec))
  }

  # ---- 4. Label contrast (white vs black text) ------------------------------
  label_cols <- .swatch_label_color(plot_hex)

  # ---- 5. Truncate very long labels to avoid clipping ----------------------
  max_chars  <- max(5L, floor(30 / max(1, L / 10)))
  too_long   <- nchar(bar_labels) > max_chars
  bar_labels[too_long] <- paste0(substr(bar_labels[too_long], 1, max_chars - 1), "\u2026")

  # ---- 6. Plot -------------------------------------------------------------
  old_par <- if (!is.null(mar)) par(mar = mar) else list()
  on.exit(if (length(old_par)) par(old_par), add = TRUE)

  pts <- barplot(rep(1, L),
                 col    = plot_hex,
                 border = border,
                 main   = main,
                 yaxt   = "n",
                 xaxt   = "n",
                 ...)

  if (label != "none") {
    text(pts, 0.5, bar_labels,
         srt = srt,
         col = label_cols,
         cex = cex.names,
         adj = 0.5)
  }

  # Mark NA slots with a small diagonal cross so they are obvious
  if (any(is.na(hex_vec))) {
    na_pts <- pts[is.na(hex_vec)]
    segments(na_pts - 0.4, 0.1, na_pts + 0.4, 0.9, col = "red", lwd = 1.5)
    segments(na_pts - 0.4, 0.9, na_pts + 0.4, 0.1, col = "red", lwd = 1.5)
  }

  invisible(pts)
}


