#' Set a variable once
#'
#' Only set a variable once, where multiple calls are ignored
#' @export

setOnce <- function(., val = 1L) {
  if(!inherits(.,"once")){
  .. <- substitute(.)
  if (typeof(..) != "symbol") stop(paste0(.., " must be an object."))
  res <- val
  class(res) <- c('once','list')
  assign(as.character(..), res, envir = .GlobalEnv)
  }
}
