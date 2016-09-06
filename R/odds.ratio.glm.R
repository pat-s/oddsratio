calc.oddsratio.glm <- function(model, data, incr, quietly = FALSE) {
  
  if (class(model)[1] == "glm") {
    
    # get pred names and coefficients without intercept
    preds <- names(coefficients(model))[2:length(coefficients(model))]
    coef <- coefficients(model)[2:length(coefficients(model))]
  }
  
  if (class(model)[1] == "glmmPQL") {
    # get pred names and coefficients without intercept
    preds <- names(model$coefficients$fixed)[2:length(model$coefficients$fixed)]
    coef <- model$coefficients$fixed[2:length(model$coefficients$fixed)]
  }
  
  odds.ratios <- list()
  for (i in preds) {
    # check if predictor is numeric or integer
    if (is.numeric(data[[i]]) | is.integer(data[[i]])) {
      odds.ratios[[i]] <- round(exp(as.numeric(coef[[i]])* as.numeric(incr[[i]])), 3)
      incr1 <- as.numeric(incr[[i]])
      or <- odds.ratios[[i]]
    }
    # if pred is factor -> perform direct conversion to odds ratio
    else {
      odds.ratios[[i]] <- round(exp(as.numeric(coef[[i]])), 3)
      incr1 <- "Non numeric predictor. Refer to basis factor level!"
      or <- odds.ratios[[i]]
    }
    
    if (!quietly) {
      cat("Variable:   '", i, "'\nIncrement:  '", 
          incr1, "'\nOdds ratio: ", or, "\n\n", sep = "")
    }
  }
}
