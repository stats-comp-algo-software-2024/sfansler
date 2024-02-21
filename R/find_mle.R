#' Finds MLE using pseudo_inverse
find_mle_pseudo_inv = function(design, outcome){
mle = list()
mle$coef = solve(t(design) %*% design, t(design) %*% outcome)
noise_variance <- mean((outcome - design %*% mle$coef)^2)
n <- nrow(design); p <- ncol(design)
noise_variance <- noise_variance / (1 - p / n)
mle$info_mat = (t(design) %*% design) / noise_variance
return(mle)
}

#' Finds MLE using BFGS for a linear model (via log-likelihood and gradient)
find_mle_optim_linear = function(design, outcome) {
  p = ncol(design)
  init = rep(0, p)
  mle = list()
  mle$coef = stats::optim(par = init, fn = log_likelihood_linear, gr = gradient_linear, control = list(fnscale = -1), design = design, outcome = outcome, method = "BFGS")$par
  mle$info_mat = -hessian(beta = mle$coef, design = design, outcome = outcome)
  return(mle)
}

#' Finds MLE using BFGS for a logistic model (via log-likelihood and gradient)
find_mle_optim_logistic = function(design, outcome) {
  p = ncol(design)
  init = rep(0, p)
  mle = list()
  mle$coef = stats::optim(par = init, fn = log_likelihood_logistic, gr = gradient_logistic, control = list(fnscale = -1), design = design, outcome = outcome, method = "BFGS")$par
  mle$info_mat = -hessian(beta = mle$coef, design = design, outcome = outcome)
  return(mle)
}

#' Finds MLE using Newton's method for a logistic model
find_mle_newton = function(design, outcome) {
  p = ncol(design)
  beta = rep(0, p)
  max_iter = 1e5
  iter = 0
  abs_diff = rel_diff = 1 #Initialize differences so loop will run

  while(any(c(abs_diff, rel_diff) > p * 1e-3)){
    if(iter < max_iter){
          likelihood_prev = log_likelihood_logistic(design = design, outcome = outcome, beta = beta)
          grad = gradient_logistic(design = design, outcome = outcome, beta = beta)
          info = -hessian(design = design, outcome = outcome, beta = beta)
          beta = beta + solve(info, grad)
          likelihood_new = log_likelihood_logistic(design = design, outcome = outcome, beta = beta)
          abs_diff = likelihood_new - likelihood_prev
          rel_diff = abs_diff / pmax(abs(likelihood_new), abs(likelihood_prev))
          iter = iter + 1
    } else {
      stop("Convergence not reached in maximum iterations")
    }
  }
  mle = list(coef = beta, info_mat = info)
  return(mle)
}
