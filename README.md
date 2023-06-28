# quickcode: A compilation of some my quick and often used R codes

### An experiment in progress...

## Examples

```
#clear R environment, set directory and load data

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


#load libraries

quickcode::libraryAll(
  dplyr,
  r2resize,
  ggplot2
)


```
