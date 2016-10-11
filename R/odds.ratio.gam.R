#' @name calc.oddsratio.gam
#' @title Calculate Odds Ratio of Generalized Additive (Mixed) Models
#' 
#' @importFrom stats coefficients
#' 
#' @description This function to calculates odds ratio(s) for specific increment 
#'     steps of a GAM(M)s. 
#' @description Odds ratios can also be calculated for continuous percentage increment steps 
#'     across the whole predictor distribution using \code{slice = TRUE}.
#' 
#' @param data The data used for model fitting.
#' @param model A fitted GAM(M).
#' @param pred Character of length one. The name of the predictor to calculate 
#' the odds ratio for.
#' @param values Numeric vector of length two.
#' Predictor values to estimate odds ratio from. Function is written to use the 
#' first provided value as the "lower" one, i.e. calculating the odds ratio 
#' 'from value1 to value2'. Only used if \code{slice = FALSE}.
#' @param percentage Numeric of length one. Percentage number to split the 
#' predictor distribution into. 
#' A value of 10 would split the predictor distribution by 10\% intervals. 
#' Only needed if \code{slice = TRUE}.
#' @param slice Logical. \code{Default = FALSE}. Whether to calculate 
#' odds ratios for fixed increment steps over the whole predictor distribution. 
#' See \code{percentage} for setting the increment values.
#' 
#' 
#' @details Currently supported functions: \code{\link[mgcv]{gam}}(mgcv), 
#' \code{\link[mgcv]{gamm}}, \code{\link[gam]{gam}}(gam). 
#' @details For \code{\link[mgcv]{gamm}}, the \code{model} input of 
#' \code{\link[oddsratio]{calc.oddsratio.gam}} needs to be the \code{gam} 
#' output (e.g. \code{fit.gam$gam}).
#' 
#' 
#' @return A data frame with (up to) six columns. \code{perc1} and \code{perc1}
#' are only returned if \code{slice = TRUE}:
#' \item{predictor}{Predictor name}
#' \item{value1}{First value of odds ratio calculation}
#' \item{value2}{Second value of odds ratio calculation}
#' \item{perc1}{Percentage value of \code{value1}}
#' \item{perc2}{Percentage value of \code{value2}}
#' \item{oddsratio}{Calculated odds ratio(s)}
#' 
#' @seealso \code{\link[oddsratio]{calc.oddsratio.glm}}
#' 
#' @examples 
#' # load data (Source: ?mgcv::gam)
#' library(mgcv)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n,scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))
#' fit.gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + 
#'                offset(x3) + x4, data = dat) # fit model
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
                               slice = FALSE) {
  
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
    
    result <- data.frame(predictor = length(100/percentage),
                         value1 = numeric(length = 100/percentage),
                         value2 = numeric(length = 100/percentage),
                         perc1 = character(length = 100/percentage),
                         perc2 = character(length = 100/percentage),
                         oddsratio = numeric(length = 100/percentage),
                         stringsAsFactors = FALSE)
    
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
      
      result$predictor = pred
      result$oddsratio[x] <- as.numeric(exp(pred.gam2 - pred.gam1), 2)
      result$value1[x] <- round(range.v[x], 3)
      result$value2[x] <- round(range.v[x + 1], 3)
      result$perc1[x] <- percentage*x - percentage
      result$perc2[x] <- percentage*x
    }
    return(result)
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
  
  result <- data.frame(predictor = pred,
                       value1 = values[1],
                       value2 = values[2],
                       oddsratio = odds.ratio,
                       stringsAsFactors = FALSE)
  return(result)
} 
