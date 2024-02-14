#' @export
hiper_glm <- function(outcome, design){
  mle = find_mle_BFGS(design, outcome)
  #To Do: get standard errors for MLE
  hglm <-  list()
  class(hglm) <- "hglm"
  warning("function not implemented yet")
}
