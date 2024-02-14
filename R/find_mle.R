#Find MLE using pseudo-inverse
find_mle_pseudo_inv = function(X, y){
mle = solve(t(X)%*%X, t(X)%*%y)
return(mle)
}

log_likelihood = function(beta, y, X, noise_variance = 1){
  n = length(y)
  mu = X %*% beta
  ll = -1/2 * n *log(2*pi*noise_variance) -1/2*sum((y - mu)^2/noise_variance)
  return(ll)
}

gradient = function(y, X, beta, noise_variance = 1){
  n = length(y)
  mu = X %*% beta
  grad = t(X) %*% (y - mu)/noise_variance
  return(grad)
}

#Find MLE using BFGS
find_mle_BFGS = function(X, y, p) {

  init = rep(0, p)
  mle = stats::optim(par = init, fn = log_likelihood, gr = gradient, control = list(fnscale = -1), X = X, y = y, method = "BFGS")
  return(mle)
}
