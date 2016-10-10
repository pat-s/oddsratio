#' @name calc.oddsratio.glm
#' @title Calculate odds ratio of GLM(M)
#' 
#' @importFrom stats coefficients
#' 
#' @description This function calculates odds ratio for specific 
#'     increment steps of GLMs. 
#' 
#' @param data The data used for model fitting.
#' @param model A fitted GLM(M).
#' @param incr List. Increment values of each predictor.
#' @param quietly Logical. Default = FALSE. Whether to output information to 
#' the console.
#' 
#' 
#' @examples 
#' ## Example with stats::glm()
#' # load data (Data source: http://www.ats.ucla.edu/stat/r/dae/logit.htm)
#' dat <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
#' dat$rank <- factor(dat$rank)
#' fit.glm <- glm(admit ~ gre + gpa + rank, data = dat, family = "binomial") # fit model
#' 
#' # Calculate OR for specific increment step of continuous variable
#' calc.oddsratio.glm(data = dat, model = fit.glm, incr = list(gre = 380, gpa = 5))
#' 
#' ## Example with MASS:glmmPQL()
#' # load data
#' library(MASS)
#' data(bacteria)
#' fit.glmmPQL <- MASS::glmmPQL(y ~ trt + week, random = ~1 | ID,
#'                              family = binomial, data = bacteria, verbose = FALSE)
#' 
#' # Apply function
#' calc.oddsratio.glm(data = bacteria, model = fit.glmmPQL, incr = list(week = 5))
#' 
#' @details Currently supported functions: \code{\link[stats]{glm}}, 
#' \code{\link[MASS]{glmmPQL}}
#'
#' @seealso \code{\link[oddsratio]{calc.oddsratio.gam}}
#' 
#' @export
calc.oddsratio.glm <- function(data, model, incr, quietly = FALSE) {
  
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
      odds.ratios[[i]] <- round(exp(as.numeric(coef[[i]]) * as.numeric(incr[[i]])), 3)
      incr1 <- as.numeric(incr[[i]])
      or <- odds.ratios[[i]]
    }
    # if pred is factor -> perform direct conversion to odds ratio
    else {
      odds.ratios[[i]] <- round(exp(as.numeric(coef[[i]])), 3)
      incr1 <- "Non-numeric predictor. Refer to base factor level!"
      or <- odds.ratios[[i]]
    }
    
    if (!quietly) {
      cat("Variable:   '", i, "'\nIncrement:  '", 
          incr1, "'\nOdds ratio: ", or, "\n\n", sep = "")
    }
  }
}
