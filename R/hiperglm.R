#' @export
hiper_glm <- function(outcome, design, model = "linear", option = list()){
  mle = list()

  if(!(model %in% c("linear", "logit"))){
    stop("argument 'model' must be 'linear' or 'logit'")
  }

  mle = find_mle(design = design, outcome = outcome, model = model, option)
  hglm_out <-  list(coef = mle$coef, info_mat = mle$info_mat)

  class(hglm_out) <- "hglm"
  return(hglm_out)
}
