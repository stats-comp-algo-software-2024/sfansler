test_that("TRUE when <abs_tol, <rel_tol", {
  w = c(6 + 1e-7, 5 + 1e-7, 4 + 1e-7)
  v = c(6, 5, 4)
  abs_diff <- abs(v - w)
  all(abs_diff < 1e-6) #TRUE
  all(abs_diff < rel_tol * pmax(abs(v), abs(w))) #TRUE
  result <- are_all_close(v, w)
  expect_true(result)
})


test_that("FALSE when <abs_tol, >rel_tol", {
  w = rep(0, 3)
  v = c(1e-7, 1e-8, 1e-9)
  abs_diff <- abs(v - w)
  all(abs_diff < 1e-6) #TRUE
  all(abs_diff < rel_tol * pmax(abs(v), abs(w))) #FALSE
  result <- are_all_close(v, w)
  expect_false(result)
  })

test_that("FALSE when >abs_tol, <rel_tol", {
  w = c(6 + 1e-6, 5 + 1e-7, 4 + 1e-6)
  v = c(6, 5, 4)
  abs_diff <- abs(v - w)
  all(abs_diff < 1e-6) #FALSE
  all(abs_diff < rel_tol * pmax(abs(v), abs(w))) #TRUE
  results <- are_all_close(v, w)
  expect_false(results)
})
