#' @export
hiper_glm <- function(outcome, design){
  #TODO: Maximize likelihood
  hglm <-  list()
  class(hglm) <- "hglm"
  return(hglm)
}
