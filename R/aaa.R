# Internal functions and miscellaneous

# random string
randString <- function(n,length){
  .ret <- c()
  .combo <- c(0:9,letters,0:9,LETTERS)
  .all <- matrix(sample(.combo,n * length, replace = TRUE), ncol = n)
  for(i in 1:ncol(.all)) vector_push(.ret,paste(.all[,i], collapse = ""))
  .ret
}


