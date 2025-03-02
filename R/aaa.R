#INTERNAL FUNCTIONS and VARIABLES
#' @importFrom utils adist
#' @importFrom grDevices rainbow
#' @importFrom graphics abline axis boxplot legend lines par points polygon rug
#' @importFrom stats approx density mad median quantile
utils::globalVariables(c("...dMywhole_", "..dd..", "..mm..", "..yyyy..","Function","size.bytes"))

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
