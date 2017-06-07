context("or_glm")

test_that("correct level count of indicator variable", {
  data("data_glm")
  fit_glm <- glm(admit ~ rank, data = data_glm, family = "binomial") # fit model
  
  out <-or_glm(data = data_glm, model = fit_glm)
  
  expect_equal(length(out$predictor), length(levels(data_glm$rank)) - 1 )
  
})