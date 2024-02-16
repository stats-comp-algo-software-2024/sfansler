test_that("observed gradient is close to numerical gradient", {
  df = simulate_data(100, 5)
  design = df$design
  outcome = df$outcome
  p = ncol(design)

  init = rep(0, p)
  grad = gradient(beta = init, design = design, outcome = outcome)
  gradient_num = approx_grad(log_likelihood, design = design, outcome = outcome, beta = init)
  expect_true(are_all_close(grad, gradient_num))
})


test_that("linalg and optim least-sq coincide", {
  df = simulate_data(100, 5)
  design = df$design
  outcome = df$outcome

  linalg_result = find_mle_pseudo_inv(design, outcome)
  BFGS_result = find_mle_BFGS(design, outcome)$par

  expect_true(are_all_close(linalg_result, BFGS_result, abs_tol = 1e-3, rel_tol = 1e-3))
})


test_that("newton and bfgs outputs coincide on logit model", {
  n_obs <- 32; n_pred <- 4
  data <- simulate_data(n_obs, n_pred, model = 'logit', seed = 1918)
  design <- data$design; outcome <- data$outcome
  via_newton_out <- hiper_glm(design, outcome, model = 'logit')
  via_bfgs_out <- hiper_glm(
    design, outcome, model = 'logit', option = list(mle_solver = 'BFGS')
  )
  expect_true(are_all_close(
    coef(via_newton_out), coef(via_bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
  ))
})
