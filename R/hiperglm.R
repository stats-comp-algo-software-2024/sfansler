#' @export
hiper_glm <- function(outcome, design, model, solver){
  if(!(model %in% c("linear", "logit"))){
    stop("argument 'model' must be 'linear' or 'logit'")
  }

  if(model == 'linear'){
    if(solver == 'pseudoinverse'){
      mle = find_mle_pseudo_inv(design = design, outcome = outcome)
    } else if(solver == 'BFGS'){
      mle = find_mle_optim_linear(design = design, outcome = outcome)
    } else {
      stop("for linear model, solver must be either 'pseudoinverse' or 'BFGS'")
    }
  } else if(model == 'logit'){
    if(solver == 'newton'){
      mle = find_mle_newton(design = design, outcome = outcome)
    } else if(solver == 'BFGS'){
      mle = find_mle_optim_logistic(design = design, outcome = outcome)
    } else {
      stop("for logit model, solver must be either 'newton' or 'BFGS'")
    }
  }

  hglm_out <-  list(coef = mle$coef, info_mat = mle$info_mat)

  class(hglm_out) <- "hglm"
  return(hglm_out)
}
