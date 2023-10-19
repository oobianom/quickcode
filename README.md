# quickcode: NOT functions and a compilation of some simple quick plus often used R functions

<img src="https://quickcode.obi.obianom.com/CRAN/rockybilly.regular_qc.webp" width="350" align="right">

## Official website: https://quickcode.obi.obianom.com

***
***


## Install

[![Typing SVG](https://readme-typing-svg.demolab.com?font=courier&weight=800&size=18&pause=1000&color=F71111&multiline=true&random=false&width=500&height=250&lines=Install+directly+from+CRAN+using+;install.packages(%22quickcode%22)+;OR+install+from+github+using+%7Bremotes%7D;remotes%3A%3Ainstall_github(%22oobianom%2Fquickcode%22);All+set!+Load+the+package+and+start+using...;library(quickcode)+or+quickcode%3A%3Aclean())](https://git.io/typing-svg)
# 30+ great R functions to add to your scripts!

## Featured function
### Add one-line code in your R script to clear environment, clear console, set working directory and load files
![](https://quickcode.obi.obianom.com/quickcode.png)

## Some Quick R Examples

***

```
#simple conversion between boolean types
#input type is "vector"
baba <- c(TRUE,"y","n","YES","yes",FALSE,"f","F","T","t")
as.boolean(baba,1) # return vector as Yes/No
as.boolean(baba,2) # return vector as TRUE/FALSE
as.boolean(baba,3) # return vector as 1/0

```
***

```
#apply the yesNoBool to convert between boolean
#input type is "data.frame"
usedata <- data.frame(ID = number(32))
usedata #view the dataset

usedata$yess = rep(c("yes","n","no","YES","No","NO","yES","Y"),4) #create a new column
usedata #view the modified dataset

#set all yess field as standardize boolean
yesNoBool(usedata,yess, type="bin") #set all as binary 1/0
yesNoBool(usedata,yess, type="log") #set all as logical TRUE/FALSE

```


***

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

```

***

```
#add keys to a vector content for use in downstream processes

ver1 <- c("Test 1","Test 2","Test 3")
add_key(ver1)

for(i in ver1){
message(sprintf("%s is the key for this %s", i$key, i$value))
}

```

***

```

#check if the entry is not integer

not.integer(45) #returns TRUE
not.integer(45.) #returns TRUE
not.integer(45L) #returns FALSE

not.null(45L) #returns TRUE
not.null(h<-NULL) #returns FALSE


```

***

```

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


```

***

```

#shorthand for not in vector

p1 <- 4
p2 <- c(1:10)

p1 %nin% p2



```

***

```

#add to a vector in one code

p1 <- c(6,7,8)
p2 <- c(1,2,3)

vector_push(p1,p2)

print(p1)


```

***

```

#add to a data frame in one code

p1 <- data.frame(ID=1:10,ID2=1:10)
p2 <- data.frame(ID=11:20,ID2=21:30)

data_push(p1,p2,"rows")

print(p1)

```

***

```

#remove from a vector in one code

p1 <- c(6,7,8,1,2,3)

vector_pop(p1)

print(p1)


```

***

```



#remove from a data frame in one code

p1 <- data.frame(ID=1:10,ID2=1:10,CD=11:20,BD=21:30)

data_pop(p1) #remove last row

print(p1)

data_pop(p1,5) #remove last 5 rows

print(p1)

```

***

```


#remove columns from a data frame in one code

p1 <- data.frame(ID=1:10,ID2=1:10,ID4=1:10,CD=11:20,BD=21:30)

data_pop(p1,which = "cols") #remove last column

print(p1)

data_pop(p1,2,which = "cols") #remove last 2 columns

print(p1)

data_pop(p1,1,which = "cols") #remove last 1 column and vectorise

print(p1)

```

***

```
#load libraries

quickcode::libraryAll(
  dplyr,
  r2resize,
  ggplot2
)


```

***

```
```


### And many more useful functions...
