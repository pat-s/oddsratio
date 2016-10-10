#' @name calc.oddsratio.gam
#' @title Calculate odds ratio of GAM(M)
#' 
#' @importFrom stats coefficients
#' 
#' @description This function to calculates odds ratio(s) for specific increment 
#'     steps of a GAM. 
#'     Odds ratios can also be calculated for continuous percentage increment steps 
#'     across the whole predictor distribution using \code{slice = TRUE}.
#' 
#' @param data The data used for model fitting.
#' @param model A fitted GAM(M).
#' @param pred Character of length one. The name of the predictor to calculate 
#' the odds ratio for.
#' @param values Numeric vector of length two.
#' Predictor values to estimate odds ratio from. Function is coded to use the first 
#' number given as the "lower" one, i.e. calculating the odds ratio 
#' 'from value1 to value2'. 
#' Only used if \code{slice = FALSE}
#' @param percentage Numeric of length one. Percentage number to split the 
#' predictor distribution into. 
#' A value of 10 would split the predictor distribution by 10\% intervals. 
#' Only needed if \code{slice = TRUE}.
#' @param slice Logical. \code{Default = FALSE}. Whether to calculate 
#' odds ratio for fixed increment steps over the whole predictor distribution. 
#' See `steps` for setting the increment values.
#' @param quietly Logical. \code{Default = FALSE}. Whether to output 
#' information to the console.
#' 
#' 
#' @details Currently supported functions: \code{\link[mgcv]{mgcv::gam}}, 
#' \code{\link[mgcv]{mgcv::gamm}}, \code{\link[gam]{gam::gam}}. 
#' For \code{\link[mgcv]{mgcv::gamm}}, the \code{model} input of \code{\link[oddsratio]{calc.oddsratio.gam}} 
#' needs to be the \code{gam} output (e.g. \code{fit.gam$gam}).
#' 
#' @seealso \code{\link[oddsratio]{odds.ratio.glm}}
#' 
#' @examples 
#' # load data (Source: package 'mgcv')
#' library(mgcv)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n,scale = sig)
#' dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))
#' fit.gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + 
#'                      offset(x3) + x4, data = dat) # fit model
#' 
#' # Calculate OR for specific increment step of continuous variable
#' calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
#'                    values = c(0.099, 0.198))
#' 
#' ## Calculate OR for change of indicator variable
#' calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x4", 
#'                    values = c("B", "D"))
#' 
#' ## Calculate ORs for percentage increments of predictor distribution (here: 20%)
#' calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
#'                    percentage = 20, slice = TRUE)
#' 
#' @export
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
      pred.gam1 <- as.numeric(stats::predict(model, data, type = "link"))
      # set values[2] of pred
      data[, pred] <- range.v[x + 1]
      # calc log odds for value 2
      pred.gam2 <- as.numeric(stats::predict(model, data, type = "link"))
      odds.ratio <- as.numeric(stats::predict(model, data, type = "link"))
      
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
  pred.gam1 <- as.numeric(stats::predict(model, data, type = "link"))
  # set values[2] of pred
  data[, pred] <- values[2]
  # calc log odds for value 2
  pred.gam2 <- as.numeric(stats::predict(model, data, type = "link"))
  
  odds.ratio <- as.numeric(exp(pred.gam2 - pred.gam1), 2)
  
  if (!quietly & !slice)
    # print variable information
    cat("Predictor: '", pred, "'\n", sep = "")
  
  if (!quietly & !slice) {
    cat("\nOdds ratio from '", values[1], "' to '",
        values[2], "': ", 
        as.numeric(exp(pred.gam2 - pred.gam1), 2), sep = "" ) 
  }
}
