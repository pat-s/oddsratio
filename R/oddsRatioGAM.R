oddsratioGAM <- function(model, data, steps,
                         predictors = NULL, pred, slice = FALSE,
                         values) {
  
  # get names of preds
  if (!is.null(predictors)) {
    names_pred <- predictors
  }
  else names_pred <- colnames(data)
  
  if (slice) {
    
    # get range of pred distribution and slice in x tiles
    range <- max(data[, pred]) - min(data[, pred])
    step <- range / steps
    
    range_v <- c()
    # create vector of steps
    for (i in 0:steps) {
      range_v <- c(range_v, min(data[, pred]) + step * i)
    }
    
    result <- data.frame(nrow = 1)
    # apply OR calc for vector
    for (x in 1:(steps)) {
      # set all preds to their mean
      for (i in names_pred) {
        data[[i]] <- mean(data[[i]])
      }
      # reduce to 1 row
      data <- dplyr::slice(data, 1:1)
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
      
      odds_ratio <- as.numeric(exp(pred_gam2 - pred_gam1), 2)
      value_info <- list(paste0("OR (", round(range_v[x], 3), " to ", round(range_v[x+1], 3), ")"))
      
      tmp <- data.frame(odds_ratio)
      names(tmp) <- value_info
      
      result <- cbind(result, tmp)
    }
    result[, 1] <- NULL
    return(result)
  }
  
  # set all preds to their mean
  for (i in names_pred) {
    data[[i]] <- mean(data[[i]])
  }
  
  # reduce to 1 row
  data <- dplyr::slice(data, 1:1)
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
  
  # return named array
  array(odds_ratio, dimnames = value_info)
}
