calc_oddsratio.gam <- function(data, model, pred, values, 
                          steps, slice = FALSE, quietly = FALSE) {
  

  names_pred <- colnames(data)
  
  if (slice) {
    
    # get range of pred distribution and slice in x tiles
    range <- max(data[, pred]) - min(data[, pred])
    step <- range / steps
    
    range_v <- c()
    # create vector of steps
    for (i in 0:steps) {
      range_v <- c(range_v, min(data[, pred]) + step * i)
    }
    
    result <- data.frame(odds_ratio = numeric(length = steps),
                         from = numeric(length = steps),
                         to = numeric(length = steps),
                         perc_from = character(length = steps),
                         perc_to = character(length = steps),
                         stringsAsFactors=FALSE)
    
    
    if(!quietly)
      # print variable information
      cat("Predictor: '", pred, "'\nSteps: ", steps, " (", 100/steps, "%)\n", sep="")
    
    # apply OR calc for vector
    for (x in 1:(steps)) {
      # set all preds to their mean
      for (i in names_pred) {
        data[[i]] <- mean(data[[i]])
      }
      # reduce to 1 row
      data <- data[1, ]
      # subset DF to preds only
      data <- data[, names_pred]
      
      # set values[1] of pred
      data[, pred] <- range_v[x]
      # calc log odds for value 1
      pred_gam1 <- as.numeric(predict(model, data, type = "link"))
      # set values[2] of pred
      data[, pred] <- range_v[x+1]
      # calc log odds for value 2
      pred_gam2 <- as.numeric(predict(model, data, type = "link"))
      odds_ratio <- as.numeric(predict(model, data, type = "link"))
      
      
      result$odds_ratio[x] <- as.numeric(exp(pred_gam2 - pred_gam1), 2)
      result$from[x] <- round(range_v[x], 3)
      result$to[x] <-  round(range_v[x+1], 3)
      result$perc_from[x] <- as.character((100/steps*x - 100/steps))
      result$perc_to[x] <- as.character((100/steps)*x)

      
      if (!quietly) {
        cat("\nOdds ratio from ", round(range_v[x], 3), "(", 
            (100/steps*x - 100/steps), "%)", " to ", 
            round(range_v[x+1], 3), 
            "(", (100/steps)*x, "%): ", 
            as.numeric(exp(pred_gam2 - pred_gam1), 2), sep = "")
      }
    }
    return(result)
  }
  
  # set all preds to their mean
  for (i in names_pred) {
    data[[i]] <- mean(data[[i]])
  }
  
  # reduce to 1 row
  data <- data[1, ]
  # subset DF to preds only
  data <- data[, names_pred]
  
  
  # set values[1] of pred
  data[, pred] <- values[1]
  # calc log odds for value 1
  pred_gam1 <- as.numeric(predict(model, data, type = "link"))
  # set values[2] of pred
  data[, pred] <- values[2]
  # calc log odds for value 2
  pred_gam2 <- as.numeric(predict(model, data, type = "link"))
  
  odds_ratio <- as.numeric(exp(pred_gam2 - pred_gam1), 2)
  value_info <- list(paste0("OR (", values[1], " to ", values[2], ")"))
  
  if (!quietly & !slice)
    # print variable information
    cat("Predictor: '", pred, "'\n", sep = "")
  
  # return named array
  array(odds_ratio, dimnames = value_info)
}
