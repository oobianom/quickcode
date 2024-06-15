#' Mutate a subset of dataset, and reattach it to the dataset
#'
#' @export
#'
mutate_filter <- function(.,filter,...){
  .dt <- .
  .dt$refonumval__1 <- 1:nrow(.dt)
  .dt1 <- .dt
  data_pop_filter(.dt1,remove = filter)
  .mut <- as.character(list(substitute(...)))
  .dt2 <- with(subset(.dt,filter),{
    eval(parse(text = .mut))
  })

}
