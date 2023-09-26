# Internal functions and miscellaneous
# random string
randString <- function(n, length) {
  .combo <- c(0:9, letters, 0:9, LETTERS)
  .all <- matrix(sample(.combo, n * length, replace = TRUE), ncol = n)
  unlist(lapply(1:ncol(.all), function(i)paste(.all[, i], collapse = "")))
}


# check date format
# expected format  YYYY-MM-DD
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
