calc_oddsratio.gam <- function(data, model, pred, values, 
                          percentage, slice = FALSE, quietly = FALSE) {
  

  names_pred <- colnames(data)
  
  if (slice) {
    
    # get range of pred distribution and slice in x tiles
    range <- max(data[, pred]) - min(data[, pred])
    step <- range / (100/percentage)
    
    range_v <- c()
    
    # create vector of percentage
    for (i in 0:(100/percentage)) {
      range_v <- c(range_v, min(data[, pred]) + step * i)
    }
    
    result <- data.frame(odds_ratio = numeric(length = 100/percentage),
                         from = numeric(length = 100/percentage),
                         to = numeric(length = 100/percentage),
                         perc_from = character(length = 100/percentage),
                         perc_to = character(length = 100/percentage),
                         stringsAsFactors=FALSE)
    
    
    if(!quietly)
      # print variable information
      cat("Predictor: '", pred, "'\nSteps: ", 100/percentage, " (", percentage, "%)\n", sep="")
    
    # apply OR calc for vector
    for (x in 1:(100/percentage)) {
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
      result$perc_from[x] <- as.character((percentage*x - percentage))
      result$perc_to[x] <- as.character((percentage)*x)

      
      if (!quietly) {
        cat("\nOdds ratio from ", round(range_v[x], 3), "(", 
            (percentage*x - percentage), "%)", " to ", 
            round(range_v[x+1], 3), 
            "(", (percentage)*x, "%): ", 
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
