#' Clear environment, clear console, set work directory and load files
#'
#' Shorthand to quickly clear console, clear environment, set working directory, load files
#'
#' @param setwd OPTIONAL. set working directory
#' @param source OPTIONAL. source in file(s)
#' @param load OPTIONAL. load in Rdata file(s)
#' @param clearPkgs Clear previous loaded packages, TRUE or FALSE
#' @return cleared environment and set directory
#'
#' @examples
#' \donttest{
#' quickcode::clean()
#' quickcode::clean(clearPkgs = TRUE) #clear all previously loaded packages
#' quickcode::clean(setwd = "/home/") #clear env and set working directory
#' quickcode::clean(source = c("/home/file1.R","file2"))
#' quickcode::clean(setwd = "/home/",source = c("file1","file2))
#' quickcode::clean(setwd = "/home/",source="file1.R",load="obi.RData")
#' }
#'
#' @export
#'



clean <- function(setwd = NULL, source = c(), load = c(), clearPkgs = FALSE) {
  # clear console, clean garbage and shut devices
  erase() #clear console
  rm(list = setdiff(ls(envir = parent.frame()), c("setwd", "source", "load", "clearPkgs")), envir = parent.frame())
  graphics.off() #graphics off
  closeAllConnections() #close any option connections
  gc() #garbage cleanup


  # set directory if it exists
  #prevwd <- getwd()
  #on.exit(setwd(prevwd))

  if (not.null(setwd)) {
    if (dir.exists(setwd)) {
      setwd(setwd)
    }
  }

  # remove previous loaded packages
  if (clearPkgs) {
    deftPkg <- c("base", "quickcode", getOption("defaultPackages"))
    for (i in grep("package:", search(), value = TRUE)) {
      curr <- strsplit(i, ":")[[1]][2]
      if (curr %nin% deftPkg){
        tryCatch({
          detach(name = i, character.only = TRUE, force = TRUE)
        }, warning = function(w) {},
        error = function(e) {},
        finally = {})

      }
    }
  }

  # load quickcode if not loaded
  if ("quickcode" %nin% (.packages()))
    library(quickcode, quietly = TRUE)

  # source in any required files
  if (length(source)) {
    for (sourced in source) {
      if (file.exists(sourced)) source(sourced)
    }
  }

  # load in any required data
  if (length(load)) {
    for (loaded in load) {
      if (file.exists(loaded)) load(loaded, envir = parent.frame())
    }
  }
}


#' Not in vector or array
#'
#' Check if entry is in vector
#'
#' @param x vector entry
#' @param table table of items to check
#' @return a boolean value to indicate if entry is present
#' @examples
#' 5 %nin% c(1:10) #FALSE
#' 5 %nin% c(11:20) #TRUE
#'
#' x = "a"
#' if(x %nin% letters) x
#' @export

`%nin%` <- function(x, table) {
  !(x %in% table)
}


#' Not numeric
#'
#' Check if entry is not numeric
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is numeric
#' @examples
#' not.numeric("45") # TRUE
#' not.numeric(45) # FALSE
#' if(not.numeric(45)) message("yes") # yes
#'
#' @export

not.numeric <- function(x) !is.numeric(x)


#' Not NULL
#'
#' Check if entry is not NULL
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is NULL
#' @examples
#' not.null("") # TRUE
#' not.null(NULL) # FALSE
#' if(not.null(45)) message("something") # yes
#'
#' @export

not.null <- function(x) !is.null(x)


#' Not empty
#'
#' Check if entry is not empty
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is empty
#' @examples
#' not.empty("empty") # TRUE
#' not.empty('') # FALSE
#' not.empty(NULL) # logical(0)
#' if(not.empty('')) message("yes") # NULL
#' @export

not.empty <- function(x) not.null(x) & (x != '')


#' Not a vector
#'
#' Check if entry is not vector
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is vector
#' @examples
#' vect1 = list(r=1,t=3:10)
#' vect2 = LETTERS
#' not.vector(vect1) # FALSE
#' not.vector(vect2) # FALSE
#' if(not.vector(vect1)) message("yes") # NULL
#'
#' @export

not.vector <- function(x) !is.vector(x)


#' Not an integer
#'
#' Check if entry is not an integer
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is an integer
#' @examples
#' not.integer(23.43) # TRUE
#' not.integer(45L) # FALSE
#' if(not.integer(4L)) message("yes") # NULL
#'
#' @export

not.integer <- function(x) !is.integer(x)


#' Not an environment
#'
#' Check if entry is not an environment object
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is an environment
#' @examples
#' test.env <- new.env()
#' test.notenv <- list(t=1)
#' not.environment(test.env) # FALSE
#' not.environment(test.notenv) # TRUE
#' if(not.environment(test.notenv)) message("yes") # yes
#'
#' @export

not.environment <- function(x) !is.environment(x)


#' Not a data
#'
#' Check if entry is not a data object
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is a data table
#' @examples
#' test.dt <- data.frame(ID=1:200,Type="RPKG.net")
#' test.notenv <- list(t=1)
#' not.data(test.dt) # FALSE
#' not.data(test.notenv) # TRUE
#' if(not.data(test.dt)) message("yes") # NULL
#'
#' @export

not.data <- function(x) !is.data.frame(x)



#' Not logical
#'
#' Check if entry is a logical object
#'
#' @param x vector entry
#' @return a boolean value to indicate if entry is logical
#' @examples
#' test.env <- TRUE
#' test.notenv <- 0
#' not.logical(test.env) # FALSE
#' not.logical(test.notenv) # TRUE
#' if(not.logical(test.notenv)) message("yes") # yes
#'
#' @export

not.logical <- function(x) !is.logical(x)



#' Load specific R libraries and clear environment
#'
#' Only include libraries, don't install if library doesn't exist
#'
#' @param ... multiple library names
#' @param lib.loc OPTIONAL. library store location
#' @param quietly OPTIONAL. attach library quietly
#' @param clear OPTIONAL. clear environment after attach
#' @return loaded libraries and clear environment
#' @examples
#' \donttest{
#' libraryAll() # show installed libraries
#' libraryAll(r2symbols,dplyr,ggplot2,shinyStorePlus)
#' libraryAll("r2ymbols")
#' }
#' @export

libraryAll <- function(..., lib.loc = NULL, quietly = FALSE, clear = TRUE) {
  # load quickcode if not loaded
  if("quickcode" %nin% (.packages())) library(quickcode, quietly = TRUE)
  # load user requested libraries
  lib.names <- as.list(substitute(args(...))[-1L])
  lapply(lib.names, function(lib) do.call("library", list(package = lib, lib.loc = lib.loc, quietly = quietly)))

  if(!length(lib.names)){
    library()
  }
  if(clear)erase()
}

#' Calculate geometric mean and round
#'
#' Calculate the geometric mean
#'
#' @param num vector of numbers
#' @param na.rm remove NAs from the vector
#' @param round round result to decimal place
#' @return the geometric mean of a set of numbers
#' @examples
#' num1 <- sample(300:3000,10)
#' g.mean(num1)
#'
#' @export

g.mean <- function(num, na.rm = TRUE, round = 2) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  return(round(exp(base::sum(log(num[num > 0]), na.rm = na.rm) / length(num)),round))
}



#' Calculate geometric standard deviation and round
#'
#' Calculate the geometric standard deviation
#'
#' @param num vector of numbers
#' @param na.rm remove NAs from the vector
#' @param round round result to decimal place
#' @return the geometric standard deviation of a set of numbers
#' @examples
#' num1 <- sample(330:400,10)
#' sd.gm(num1,na.rm=FALSE)
#'
#' @export

sd.gm <- function(num, na.rm = TRUE, round = 2) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  return(round(exp(stats::sd(log(num[num > 0]), na.rm = na.rm)),round))
}



#' Calculate geometric coefficient of variation and round
#'
#' Calculate the coefficient of variation and round
#'
#' @param num vector of numbers
#' @param na.rm remove NAs from the vector
#' @param pct TRUE or FALSE. should result be in percent
#' @param round round result to decimal place
#' @return the geometric cv of a set of numbers
#' @examples
#' num1 <- sample(330:400,15)
#' cv.gm(num1,round = 3)
#'
#' @export

cv.gm <- function(num, na.rm = TRUE, pct = TRUE, round = 2) {
  if(not.numeric(num)) stop("The vector must have numbers only")
  res <- sqrt(exp(sd(log(num[num > 0]), na.rm = na.rm)^2) - 1)
  if (pct) res <- res * 100
  round(res,round)
}


#' Add elements to a vector like array_push in php
#'
#' Shorthand to add elements to a vector and save as the same name
#'
#' @param . first list
#' @param add vector to add
#' @return vector combining fist and second vector, but have name set to the first
#' @examples
#' num1 <- sample(330:400,10)
#' num2 <-"rpkg.net"
#' vector_push(num1, add= num2)
#' @export
#'
vector_push <- function(., add) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  assign(as.character(..), c(get(as.character(..), envir = parent.frame()), add), envir = parent.frame())
}


#' Add elements to a list like array_push in php
#'
#' Shorthand to add elements to a vector and save as the same name
#'
#' @param . first list
#' @param add list to add
#' @return vector combining fist and second vector, but have name set to the first
#' @examples
#' num1 <- list(sample(330:400,10))
#' num2 <-list("rpkg.net")
#' list_push(num1, add= num2)
#' @export
#'
list_push <- function(., add) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  assign(as.character(..), list(get(as.character(..), envir = parent.frame()), add), envir = parent.frame())
}


#' Increment vector by value
#'
#' Increment the content of a vector and resave as the vector
#'
#' @param . vector of number(s)
#' @param add number to add
#' @return vector combining fist and second vector, but have name set to the first
#' @examples
#' num1 <- sample(330:400,10)
#' inc(num1)
#' inc(num1, add= 5)
#' @export
#'
inc <- function(., add = 1) {
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  assign(as.character(..), (get(as.character(..), envir = parent.frame()) + add), envir = parent.frame())
}



#' Calculate data to another data like array_push in php
#'
#' Shorthand to add data to a dataset and save as the same name
#'
#' @param . first data set
#' @param add data set to add
#' @param which where to append the new data e.g. rows or cols
#' @return the combined dataset store to a variable with the name of the first
#' @examples
#' p1 <- data.frame(PK=1:10,ID2=1:10)
#' p2 <- data.frame(PK=11:20,ID2=21:30)
#' data_push(p1,p2,"rows")
#' @export
#'
data_push <- function(., add, which = c("rows", "cols")) {
  which <- match.arg(which)
  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  data <- as.data.frame(get(as.character(..), envir = parent.frame()))
  add <- as.data.frame(add)
    switch(which,
           "rows" = {
             data <- rbind(data,add)
           },
           "cols" = {
             data <- cbind(data,add)
           }
    )
  assign(as.character(..), data, envir = parent.frame())
}




#' Shuffle a vector just like shuffle in php
#'
#' Shorthand to shuffle a vector and save
#'
#' @param . vector to shuffle
#' @param replace replace selected value
#' @param prob probability of occurrence
#' @param seed apply seed if indicated for reproducibility
#' @return shuffled vector of items store to the vector name
#'
#' @examples
#' v1<-c(3,45,23,3,2,4,1)
#'
#'
#' #demonstrate vector_shuffle
#' vector_shuffle(v1)
#' v1 # show outputs
#'
#' #demonstrate reproducibility in shuffle with seed
#' v0<-v1
#' vector_shuffle(v0)
#' v0 #first output
#'
#' v0<-v1
#' vector_shuffle(v0)
#' v0 # different output from first output top
#'
#' v0<-v1
#' vector_shuffle(v0,seed = 232L)
#' v0 #second output
#'
#' v0<-v1
#' vector_shuffle(v0,seed = 232L)
#' v0 #the same output as second output top
#' @export
#'

vector_shuffle <- function(., replace = FALSE, prob = NULL, seed = NULL) {
  if(not.vector(.)) stop("The first element must be a vector")
  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  val <- get(as.character(..), envir = parent.frame())

  if(not.null(seed))set.seed(seed)
  assign(as.character(..), sample(val, length(val), replace = replace, prob = prob), envir = parent.frame())
}



#' Shuffle a data frame just like shuffle in php
#'
#' Shorthand to shuffle a data frame and save
#'
#' @param . data to shuffle as data frame
#' @param which what to shuffle, rows or columns
#' @param seed apply seed if indicated for reproducibility
#' @return shuffled data frame of items store to the data frame name
#'
#' @examples
#' df1<-data.frame(ID=46:55,PK=c(rep("Treatment",5),rep("Placebo",5)))
#' data_shuffle(df1)
#' data_shuffle(df1,seed = 1003)
#' @export
#'

data_shuffle <- function(., which = c("rows", "cols"), seed = NULL) {
  which <- match.arg(which)

  .. <- substitute(.)

  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))

  data <- as.data.frame(get(as.character(..), envir = parent.frame()))

  if(not.null(seed))set.seed(seed)

    switch(which,
      "rows" = {
        data <- data[sample(nrow(data)), ]
      },
      "cols" = {
        data <- data[, sample(ncol(data))]
      }
    )
  assign(as.character(..), data, envir = parent.frame())
}





#' Clear environment, clear console, set work directory and load files
#'
#' Shorthand to quickly clear console, clear environment, set working directory, load files
#'
#' @param setwd OPTIONAL. set working directory
#' @param source OPTIONAL. source in file(s)
#' @param load OPTIONAL. load in Rdata file(s)
#' @return cleared environment and set directory
#' @examples
#' \donttest{
#' refresh()
#' refresh(setwd = "home")
#' refresh(setwd = "home",source = c("home/file1.R","file2"))
#' refresh(setwd = "home/",source = c("file1","file2))
#' refresh(setwd = "home/",source="file1.R",load="obi.RData")
#' }
#'
#' @export
#'
refresh <- clean



#' Shiny app function to insert string to current file in RStudio
#'
#' Shorthand to insert content to opened file
#'
#' @param string what to insert
#' @return Inserts into current position on opened file
#'
#' @examples
#' if(interactive()){
#' insertInText('hello rpkg.net')
#' insertInText('hello world')
#' }
#' @export
#'

insertInText <- function(string) {
  adc <- rstudioapi::getActiveDocumentContext()
  rstudioapi::insertText(location = adc$selection[[1]]$range$start, string)
}



#' Snippet function to add header to a current opened file
#'
#' Shorthand to add header
#'
#' @return Inserts header content for file
#'
#' @examples
#' if(interactive())
#' add.header()
#' @export
#'

add.header <- function() {
  insertInText(paste0("
############################################################################
#  Document Path: ", rstudioapi::getActiveDocumentContext()$path, "
#
#  Author:
#
#  Date: ", Sys.Date(), "
#
#  Title:
#
#  Description:
#
#  Required Files:
#
#  Exported Files:
#
#  R Version: ", version$version.string, "
#
#############################################################################

  "))
}

#' Snippet R function to clear console and set directory
#'
#' Shorthand to add clear console code to current file
#'
#' @return Inserts code to clear console
#'
#' @examples
#' if(interactive())
#' add.snippet.clear()
#' @export
#'

add.snippet.clear <- function() {
  insertInText(paste0("
# script header
quickcode::libraryAll(
  ...
)
quickcode::clean(
  setwd = '...'
)

# script body


# session information
  sessionInfo()
"))
}



#' Snippet function to add header to a current Rmd opened file
#'
#' Shorthand to add Rmd header
#'
#' @return Inserts header content for Rmd file
#'
#' @examples
#' if(interactive())
#' header.rmd()
#' @export
#'

header.rmd <- function() {
  insertInText(paste0("
<!---

Document Path: ", rstudioapi::getActiveDocumentContext()$path, "

Author:

Date: ", Sys.Date(), "

Title:

Description:

Required Files:

Exported Files:

R Version: ", version$version.string, "


--->"))
}


#' Re-sample a dataset by column and return number of entry needed
#'
#' Shorthand to return a re-sample number of rows in a data frame by unique column
#'
#' @param .dt data frame to re-sample
#' @param col column to uniquely re-sample
#' @param n number of rows to return
#' @param seed unique numeric value for reproducibility
#' @return data frame containing re-sampled rows from an original data frame
#'
#' @examples
#' data1 <- data.frame(ID=1:10,MOT=11:20)
#' sample_by_column(data1,MOT,3)
#' sample_by_column(data1,ID,7)
#' @export
#'

sample_by_column <- function(.dt, col, n, seed = 354354) {
  set.seed(seed)
  if(not.data(.dt)) stop("First element must be a data frame.")
  .dt[.dt[, as.character(substitute(col))] %in% sample(unique(.dt[, as.character(substitute(col))]), n),]
}


#' Add index  keys to a vector
#'
#' Index a vector and convert to a list of objects
#'
#' @param vector vector to transform
#' @return a transformed list containing keys along with vector values
#' @examples
#' #ex1 simple conversion of a vector
#' rti2 <- c("rpkg","obinna", "obianom")
#' add_key(rti2)
#' rti2
#'
#' #ex2 add keys to a vector content for use in downstream processes
#' ver1 <- c("Test 1","Test 2","Test 3")
#' add_key(ver1)
#'
#' for(i in ver1){
#'   message(sprintf("%s is the key for this %s", i$key, i$value))
#' }
#'
#' @export
#'
add_key <- function(vector){
  . = list()
  iky = 1
  for(i in vector){
    .[[length(.)+1]] <- list(key = iky, value = i)
    inc(iky)
  }
  #resave to vector name
  .. <- substitute(vector)
  assign(as.character(..), ., envir = parent.frame())
}


#' Duplicate a file and global replace
#'
#' Shorthand to return a re-sample number of rows in a data frame by unique column
#'
#' @param file data frame to re-sample
#' @param new.name column to uniquely re-sample
#' @param pattern number of rows to return
#' @param replacement unique numeric value for reproducibility
#' @param open description
#' @return data frame containing re-sampled rows from an original data frame
#'
#' @examples
#' \donttest{
#' duplicate('file.R')
#' }
#' @export
#'
duplicate <- function(file, new.name,pattern, replacement,open = TRUE){
  #get initial file
  .file.1 <- readLines(file)

  #substitute text
  if(not.null(pattern) & not.null(replacement)){
      for(.i in 1:length(pattern)){
        .file.1 <- gsub(pattern[.i],replacement[.i],.file.1)
      }
  }

  #write to new file
  writeLines(.file.1,new.name)
  rstudioapi::navigateToFile(new.name)
}



#' Generate a random number
#'
#' Shorthand code to generate a random number
#'
#' @param n how many numbers to generate
#' @param max.digits maximum number of digits in each number
#' @return random numbers between 1 and 1 billion
#'
#' @examples
#' number(1)
#' number(10)
#' paste0(number(2),LETTERS)
#' @export
#'
number <- function(n,max.digits=10){
  substr(sample(1L:1000000000L, n),0,max.digits)
}


#' Initialize new variables and objects
#'
#' Shorthand to initialize one or more objects
#'
#' @param ... variable names to initialize
#' @param value value to initialize them to
#' @return initialized objects set to the value specified
#'
#' @examples
#' init(t,u,v)
#' message(t) # t = NULL
#' message(u) # u = NULL
#' message(v) # v = NULL
#' init(j,k,m,value = 7)
#' message(j) # j = 7
#' message(k) # k = 7
#' message(m) # m = 7
#'
#' @export
#'
init <- function(...,value = NULL){
  .v <- as.list(substitute(args(...))[-1L])
  for(i in .v)
    assign(as.character(i), value , envir = parent.frame())
}



#' @export
#'
ai.duplicate <- function(file = NULL, new.name = NULL , open = TRUE) {

  pattern = NULL
  replacement = NULL

  if (is.null(file)) {
    file <- readline(prompt = "What file are you trying to duplicate?")
  }

  if (is.null(new.name)) {
    new.name <- readline(prompt = "What is the new file name?")
  }

  #strings to replace
  repeat{
    vector_push(pattern,readline(prompt = "What string would you like to replace?"))
    vector_push(replacement,readline(prompt = "What will you like to replace with?"))

    #check if more replacements are needed
    if(!as.logical(toupper(readline(prompt = "Want to replace more (T = Yes, F = No) ?"))))
      break
  }
  #duplicate file with entered parameters
  duplicate(file, new.name, pattern, replacement, open = TRUE)
  invisible(file)
}






##Next version to-do list

#ggplot

#Duplicate a file and open for editing
#Duplicate a file and change a string within


#Duplicate a file using the console prompt


##
##sample_by_column <- function(.dt, col, n, replace = FALSE) {
##  .dt[.dt[, as.character(substitute(col))] %in% sample(unique(.dt[, as.character(substitute(col))]), n, replace = replace), ]
##}
##sample_by_row <- function(.dt, col, n, replace = FALSE) {
## .dt[.dt[, as.character(substitute(col))] %in% sample(unique(.dt[, as.character(substitute(col))]), n, replace = replace), ]
##}
##
##
## Next version
##
## has <- function(., var, col, row) {
##   stop("Not completed")
##
##   object <- .
##
##   if (is.data.frame(object)) {
##     # check if var exists in data.frame
##
##     # check if column exists in data.frame
##
##     # check if row exists in data.frame
##   }
`%nin%` -> `%!in%`
(function()eval(parse(text=paste0(letters[3],'at','("\\','014")')), envir=.GlobalEnv)) -> erase
##   if (is.vector(object)) {
##     # check if var exists in vector
##   }
##
##   if (is.character(object)) {
##     # check if var exists in character
##   }
##
##   if (is.numeric(object)) {
##     # check if var exists in number
##   }
## }


