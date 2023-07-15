# quickcode: A compilation of some my quick and often used R codes

## Install

```
devtools::install_github("oobianom/quickcode")

install.packages("quickcode") # when published

```

## Examples

```

#check if a number is not integer

not.integer(45) #returns TRUE
not.integer(45.) #returns TRUE
not.integer(45L) #returns FALSE

#clear R environment, set directory and load data

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

p1 %!in% p2



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
