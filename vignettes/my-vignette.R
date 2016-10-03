## ---- echo=FALSE---------------------------------------------------------
calc.oddsratio.gam <- function(data, model, pred, values, percentage, 
                               slice = FALSE, quietly = FALSE) {
  
  names.pred <- colnames(data)
  
  if (slice) {
    
    # get range of pred distribution and slice in x tiles
    range <- max(data[, pred]) - min(data[, pred])
    step <- range / (100/percentage)
    
    range.v <- c()
    
    # create vector of percentage
    for (i in 0:(100/percentage)) {
      range.v <- c(range.v, min(data[, pred]) + step * i)
    }
    
    result <- data.frame(odds.ratio = numeric(length = 100/percentage),
                         from = numeric(length = 100/percentage),
                         to = numeric(length = 100/percentage),
                         perc_from = character(length = 100/percentage),
                         perc_to = character(length = 100/percentage),
                         stringsAsFactors = FALSE)
    
    if (!quietly)
      # print variable information
      cat("Predictor: '", pred, "'\nSteps:     ", 100/percentage,
          " (", percentage, "%)\n", sep = "")
    
    # apply OR calc for vector
    for (x in 1:(100/percentage)) {
      
      # set all preds to their mean if they are numeric
      for (i in names.pred) {
        if (is.numeric(data[[i]])) 
           data[[i]] <- mean(data[[i]])
      }
      
      # reduce to 1 row
      data <- data[1, ]
      # subset DF to preds only
      data <- data[, names.pred]
      
      # set values[1] of pred
      data[, pred] <- range.v[x]
      # calc log odds for value 1
      pred.gam1 <- as.numeric(predict(model, data, type = "link"))
      # set values[2] of pred
      data[, pred] <- range.v[x + 1]
      # calc log odds for value 2
      pred.gam2 <- as.numeric(predict(model, data, type = "link"))
      odds.ratio <- as.numeric(predict(model, data, type = "link"))
      
      # combine results in DF
      result$odds.ratio[x] <- as.numeric(exp(pred.gam2 - pred.gam1), 2)
      result$from[x] <- round(range.v[x], 3)
      result$to[x] <-  round(range.v[x + 1], 3)
      result$perc_from[x] <- as.character((percentage*x - percentage))
      result$perc_to[x] <- as.character((percentage)*x)

      if (!quietly) {
        cat("\nOdds ratio from ", round(range.v[x], 3), "(", 
            (percentage*x - percentage), "%)", " to ", 
            round(range.v[x + 1], 3), 
            "(", (percentage)*x, "%): ", 
            as.numeric(exp(pred.gam2 - pred.gam1), 2), sep = "")
      }
    }
    return(invisible(result))
  }
  
  # set all preds to their mean if they are numeric
  for (i in names.pred) {
    if (is.numeric(data[[i]]))
       data[[i]] <- mean(data[[i]])
  }
  
  # reduce to 1 row
  data <- data[1, ]
  # subset DF to preds only
  data <- data[, names.pred]
  
  # set values[1] of pred
  data[, pred] <- values[1]
  # calc log odds for value 1
  pred.gam1 <- as.numeric(predict(model, data, type = "link"))
  # set values[2] of pred
  data[, pred] <- values[2]
  # calc log odds for value 2
  pred.gam2 <- as.numeric(predict(model, data, type = "link"))
  
  odds.ratio <- as.numeric(exp(pred.gam2 - pred.gam1), 2)
  value.info <- list(paste0("OR (", values[1], " to ", values[2], ")"))
  
  if (!quietly & !slice)
    # print variable information
    cat("Predictor: '", pred, "'\n", sep = "")
  
  if (!quietly & !slice) {
    cat("\nOdds ratio from '", values[1], "' to '",
        values[2], "': ", 
        as.numeric(exp(pred.gam2 - pred.gam1), 2), sep = "" ) 
  }
}


## ------------------------------------------------------------------------
suppressPackageStartupMessages(library(mgcv))
n <- 200
sig <- 2
dat <- suppressMessages(gamSim(1, n = n,scale = sig))
dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))

fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4, data = dat)

## ------------------------------------------------------------------------
calc.oddsratio.gam(model = fit_gam, data = dat, pred = "x2",
                   values = c(0.099, 0.198))

## ------------------------------------------------------------------------
calc.oddsratio.gam(model = fit_gam, data = dat, pred = "x4",
                   values = c("A", "B"))

calc.oddsratio.gam(model = fit_gam, data = dat, pred = "x4",
                   values = c("B", "D"))

## ------------------------------------------------------------------------
calc.oddsratio.gam(fit_gam, pred = "x2", percentage = 20,
                   slice = TRUE, data = dat, quietly = F)

## ---- echo=FALSE---------------------------------------------------------
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

## ------------------------------------------------------------------------
dat <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
dat$rank <- factor(dat$rank)
fit_glm <- glm(admit ~ gre + gpa + rank, data = dat, family = "binomial")

## ------------------------------------------------------------------------
calc.oddsratio.glm(fit_glm, dat, list(gre = 380, gpa = 5))

## ------------------------------------------------------------------------
library(nlme)
library(MASS)
data(bacteria)
fit_glmmPQL <- MASS::glmmPQL(y ~ trt + week, random = ~1 | ID,
                             family = binomial, data = bacteria, verbose = FALSE)

## ------------------------------------------------------------------------
calc.oddsratio.glm(fit_glmmPQL, bacteria, list(week = 5))

