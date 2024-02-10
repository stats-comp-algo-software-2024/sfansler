#Find MLE using pseudo-inverse
find_mle_pseudo_inv = function(X, y){
mle = solve(t(X)%*%X, t(X)%*%y)
return(mle)
}


#Find MLE using BFGS
find_mle_BFGS = function(X, y, p) {

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

  approx_grad <- function(func, beta, ..., dx = .Machine$double.eps^(1/3)) {
    numerical_grad <- rep(0, length(beta))

    fn = function(beta) func(beta, ...)

    for(i in 1:length(beta)){
      e = rep(0, times = length(beta))
      e[i] = 1
      numerical_grad[i] = (fn(beta + dx*e) - fn(beta - dx*e))/(2*dx)
    }
    return(numerical_grad)
  }

  init = rep(0, p)

  if(are_all_close(gradient(y = y, X = X, beta = init), approx_grad(log_likelihood, beta = init, y = y, X = X)) == F){
    stop("Calculated Gradient and Numerical Gradient do not match")
  }

  mle = stats::optim(par = init, fn = log_likelihood, gr = gradient, control = list(fnscale = -1), X = X, y = y, method = "BFGS")
  return(mle)
}
