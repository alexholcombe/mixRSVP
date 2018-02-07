context("test-fit_model.R")

#Try a dataset that can give grad.default pesky fitting error like
#Error in grad.default(ufn, ans$par, ...) : function returns NA at 1.4434927002511e-050.000135401581392880.000100001 distance from x.,

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
