#' Calculates the log-likelihood used to find MLE using BFGS for a linear model
log_likelihood_linear = function(beta, outcome, design, noise_variance = 1){
  mu = design %*% beta
  ll = -1/2 * sum((outcome - mu)^2 / noise_variance)
  return(ll)
}

#' Calculates the gradient used to find MLE using BFGS for a linear model
gradient_linear = function(beta, outcome, design, noise_variance = 1){
  mu = design %*% beta
  grad = t(design) %*% (outcome - mu) / noise_variance
  return(grad)
}

#' Calculates the log-likelihood used to find MLE using BFGS for a logistic model
log_likelihood_logistic = function(beta, outcome, design) {
  pi = 1 / (1 + exp(-(design %*% beta)))
  ll = outcome %*% log(pi) + (1-outcome) %*% log(1 - pi)
  return(ll)
}

#' Calculates the gradient used to find MLE using BFGS for a logistic model
gradient_logistic = function(beta, outcome, design) {
  pi = exp(design %*% beta) / (1 + exp(design %*% beta))
  grad = t(design) %*% (outcome - pi)
  return(grad)
}

#' Calculates the hessian for a logistic model
hessian = function(beta, outcome, design) {
  pi = exp(design %*% beta) / (1 + exp(design %*% beta))
  W = diag(as.vector(pi * (1 - pi)))
  hess = -t(design) %*% W %*% design
}
