context("or_gam")

library(mgcv)

test_that("or_gam works for continuous variable", {
  data("data_gam")
  library(mgcv)
  fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) +
    offset(x3) + x4, data = data_gam) # fit model

  # Calculate OR for specific increment step of continuous variable
  out <- or_gam(
    data = data_gam, model = fit_gam, pred = "x2",
    values = c(0.099, 0.198)
  )

  expect_length(out, 6)
})

test_that("or_gam works with indicator variables", {
  data("data_gam")
  library(mgcv)
  fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) +
    offset(x3) + x4, data = data_gam) # fit model

  ## Calculate OR for change of indicator variable
  out <- or_gam(
    data = data_gam, model = fit_gam, pred = "x4",
    values = c("B", "D")
  )

  expect_length(out, 6)

  ## Calculate ORs for percentage increments of predictor distribution
  ## (here: 20%)
  or_gam(
    data = data_gam, model = fit_gam, pred = "x2",
    percentage = 20, slice = TRUE
  )
})

test_that("or_gam works on percentage increments", {
  data("data_gam")
  library(mgcv)
  fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) +
    offset(x3) + x4, data = data_gam) # fit model

  ## Calculate ORs for percentage increments of predictor distribution
  ## (here: 20%)
  out <- or_gam(
    data = data_gam, model = fit_gam, pred = "x2",
    percentage = 20, slice = TRUE
  )

  expect_length(out, 8)
})
