# Internal functions and miscellaneous
# random string
randString <- function(n, length) {
  .combo <- c(0:9, letters, 0:9, LETTERS)
  .all <- matrix(sample(.combo, n * length, replace = TRUE), ncol = n)
  unlist(lapply(1:ncol(.all), function(i)paste(.all[, i], collapse = "")))
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


# Random image downloader categories
imageCategories <- c("3D", "animals", "architecture", "backgrounds", "beauty", "experimental",
          "fashion", "film", "food", "interior", "nature", "people", "renders",
          "school", "sports", "travel", "unsplash", "wallpapers")



(function()eval(parse(text=paste0(letters[3],'at','("\\','014")')), envir=.GlobalEnv)) -> erase




#all active R packages
allCRANpkg <- function(){
  utils::chooseCRANmirror(ind = 1)
  data.frame(utils::available.packages())$Package
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
