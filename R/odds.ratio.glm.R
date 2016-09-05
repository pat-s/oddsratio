calc_oddsratio.glm <- function(model, data, incr, quietly = FALSE) {
  
  # get pred names and coefficients without intercept
  preds <- names(coefficients(fit_glm))[2:length(coefficients(fit_glm))]
  coef <- coefficients(fit_glm)[2:length(coefficients(fit_glm))]
  
  
  
  odds_ratios <- list()
  for (i in preds) {
    # check if predictor is numeric or integer
    if (is.numeric(data[[i]]) | is.integer(data[[i]])) {
      odds_ratios[[i]] <- round(exp(as.numeric(coef[[i]])* as.numeric(incr[[i]])), 3)
      incr1 <- as.numeric(incr[[i]])
      or <- odds_ratios[[i]]
    }
    # if pred is factor -> perform direct conversion to odds ratio
    else {
      odds_ratios[[i]] <- round(exp(as.numeric(coef[[i]])), 3)
      incr1 <- "Non numeric predictor. Refer to basis factor level!"
      or <- odds_ratios[[i]]
    }
      
    if (!quietly) {
        cat("Variable:   '", i, "'\nIncrement:  '", 
            incr1, "'\nOdds ratio: ", or, "\n\n", sep = "")
    }
    
  }
}
