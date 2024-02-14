test_that("TRUE when <abs_tol, <rel_tol", {
  w = c(6 + 1e-7, 5 + 1e-7, 4 + 1e-7)
  v = c(6, 5, 4)
  expect_true(are_all_close(v, w))
})


test_that("FALSE when <abs_tol, >rel_tol", {
  w = rep(0, 3)
  v = c(1e-7, 1e-8, 1e-9)
  expect_false(are_all_close(v, w))
  })

test_that("FALSE when >abs_tol, <rel_tol", {
  w = c(6 + 1e-6, 5 + 1e-7, 4 + 1e-6)
  v = c(6, 5, 4)
  expect_false(are_all_close(v, w))
})
