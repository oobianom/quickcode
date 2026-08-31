# print all the environment object and sizes and connections to each other
#' Get all the environment objects and their sizes
#'
#' Retrieve the size contribution of all the available objects in the environment
#'
#' @param envir the environment to retrieve objects from
#' @return a dataframe of all the variables within the environment
#' @examples
#' # Get a data frame of all environment objects and their size
#' summarize.envobj()
#' @export
summarize.envobj <- function(envir = parent.frame()){
  envs = ls(all.names = TRUE, envir = envir)
  envs.size = c()
  for(b in  envs) vector_push(envs.size, eval(parse(text = paste0("object.size(",b,")"))))
  within(data.frame(objects = c(envs,"TOTAL"), size.bytes = c(envs.size,sum(envs.size))),{
    size.kbytes = size.bytes/1000
    size.mbytes = size.kbytes/1000
  })
}


# Future updates: version 2
# within(data.frame(size.bytes = unlist(
#   sapply(ls(all.names = TRUE, envir = envir), function(n) {
#     object.size(get(n))
#   }, simplify = FALSE)
# )),{
#   size.kbytes = size.bytes/1000
# })
