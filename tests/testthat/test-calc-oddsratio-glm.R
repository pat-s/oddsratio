context("calc.oddsratio.glm")

test_that("correct level count of indicator variable", {
  data("data.glm")
  fit.glm <- glm(admit ~ rank, data = data.glm, family = "binomial") # fit model
  
  out <- calc.oddsratio.glm(data = data.glm, model = fit.glm)
  
  expect_equal(length(out$predictor), length(levels(data.glm$rank)) - 1 )
  
})