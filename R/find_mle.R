#' Finds MLE using pseudo_inverse
find_mle_pseudo_inv = function(design, outcome){
mle = solve(t(design) %*% design, t(design) %*% outcome)
return(mle)
}

#' Calculates the log-likelihood used to find MLE using BFGS
log_likelihood = function(beta, outcome, design, noise_variance = 1){
  n = length(outcome)
  mu = design %*% beta
  ll = -1/2 * n * log(2*pi*noise_variance) -1/2 * sum((outcome - mu)^2 / noise_variance)
  return(ll)
}

#' Calculates the gradient used to find MLE using BFGS
gradient = function(outcome, design, beta, noise_variance = 1){
  n = length(outcome)
  mu = design %*% beta
  grad = t(design) %*% (outcome - mu) / noise_variance
  return(grad)
}

#'Find MLE using BFGS (via log-likelihood and gradient)
find_mle_BFGS = function(design, outcome) {
  p = ncol(design)
  init = rep(0, p)
  mle = stats::optim(par = init, fn = log_likelihood, gr = gradient, control = list(fnscale = -1), design = design, outcome = outcome, method = "BFGS")
  return(mle)
}
