#' Function chaining
#'
#' Chain multiple function to a call
#'
#' @examples
#' 1:3%.%.unique.length
#' sample(1:1000,10,replace=TRUE) %.%.unique.length
#' @export

`%.%` <- function(left, right){
  .pF <- as.character(substitute(right))
  .pF2 <- strsplit(.pF,"\\.")[[1]][-1]
  lapply(.pF2, function(l) eval(parse(text = paste0("left <<-left %>% ",l))))
  left
}



