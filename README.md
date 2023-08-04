# quickcode: NOT functions and a compilation of some simple quick plus often used R functions

## Install

```
remotes::install_github("oobianom/quickcode")

#or from CRAN

install.packages("quickcode") 

```

## Some Quick Examples

```
#initialize one or more variables

print(g) # Error: object 'g' not found

init(g,h,i,o)
print(g) # g = NULL
print(h) # h = NULL

init(r,y,u,b,value = 5)
print(r) # r = 5
print(b) # b = 5
print(z) # Error: object 'z' not found



#add keys to a vector content for use in downstream processes

ver1 <- c("Test 1","Test 2","Test 3")
add_key(ver1)

for(i in ver1){
message(sprintf("%s is the key for this %s", i$key, i$value))
}



#check if the entry is not integer

not.integer(45) #returns TRUE
not.integer(45.) #returns TRUE
not.integer(45L) #returns FALSE

not.null(45L) #returns TRUE
not.null(h<-NULL) #returns FALSE

#clear R environment, set directory and load data
#note: the code below also automatically loads the quickcode library so that all other functions within package can be used easily


quickcode::refresh()
quickcode::clean()

#or combine with setwd and source and load

quickcode::clean(
  setwd = "/wd/",
  source = c(
  "file.R",
  "file2.R"
  ),
  load = c(
  "data.RData",
  "data2.RData"
  )
)

#shorthand for not in vector

p1 <- 4
p2 <- c(1:10)

p1 %nin% p2



#add to a vector in one code

p1 <- c(6,7,8)
p2 <- c(1,2,3)

vector_push(p1,p2)

print(p1)

#add to a data frame in one code

p1 <- data.frame(ID=1:10,ID2=1:10)
p2 <- data.frame(ID=11:20,ID2=21:30)

data_push(p1,p2,"rows")

print(p1)



#load libraries

quickcode::libraryAll(
  dplyr,
  r2resize,
  ggplot2
)
```


### And many more useful functions...
