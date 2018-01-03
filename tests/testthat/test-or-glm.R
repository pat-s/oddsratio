context("or_glm")

pacman::p_load(mgcv, MASS)

test_that("correct level count of indicator variable for glm", {
  data("data_glm")
  fit_glm <- glm(admit ~ rank, data = data_glm, family = "binomial") # fit model

  out <- or_glm(data = data_glm, model = fit_glm)

  expect_length(out$predictor, length(levels(data_glm$rank)) - 1)

})

test_that("or_glm works with glmmPQL", {

  data(bacteria)
  fit_glmmpql <- glmmPQL(y ~ trt + week, random = ~1 | ID,
                        family = binomial, data = bacteria,
                        verbose = FALSE)
  # Apply function
  out <- or_glm(data = bacteria, model = fit_glmmpql, incr = list(week = 5))

  expect_length(out, 5)

})
