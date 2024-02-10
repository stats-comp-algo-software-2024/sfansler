test_that("observed gradient is close to numerical gradient", {
  df = simulate_data(100, 5)
  X = df$design
  y = df$outcome
  p = ncol(X)

  init = rep(0, p)
  grad = gradient(beta = init, X = X, y = y)
  gradient_num = approx_grad(log_likelihood, X = X, y = y, beta = init)
  result = are_all_close(grad, gradient_num)
  expect_true(result)
})



test_that("linalg and optim least-sq coincide", {
  df = simulate_data(100, 5)
  X = df$design
  y = df$outcome

  p = ncol(X)

  linalg_result = find_mle_pseudo_inv(X, y)
  BFGS_result = find_mle_BFGS(X, y, p)$par

  result = are_all_close(linalg_result, BFGS_result)
  expect_true(result)
})
