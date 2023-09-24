# Internal functions and miscellaneous
clean()
# random string
randString <- function(n, length) {
  .combo <- c(0:9, letters, 0:9, LETTERS)
  .all <- matrix(sample(.combo, n * length, replace = TRUE), ncol = n)
  unlist(lapply(1:ncol(.all), function(i)paste(.all[, i], collapse = "")))
}

