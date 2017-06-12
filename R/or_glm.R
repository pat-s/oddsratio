#' @name or_glm
#' @title Calculate odds ratios of Generalized Linear (Mixed) Models
#'
#' @importFrom stats coefficients
#' @importFrom stats confint
#' @import mgcv
#' @import MASS
#'
#' @description This function calculates odds ratio(s) for specific
#'     increment steps of GLMs.
#'
#' @param data The data used for model fitting.
#' @param model A fitted GLM(M).
#' @param incr List. Increment values of each predictor.
#' @param CI numeric. Which confident interval to calculate.
#' Must be between 0 and 1. Default to 0.95
#'
#' @return A data frame with five columns:
#' \item{predictor}{Predictor name(s)}
#' \item{oddsratio}{Calculated odds ratio(s)}
#' \item{CI_low}{Lower confident interval of odds ratio}
#' \item{CI_high}{Higher confident interval of odds ratio}
#' \item{increment}{Increment of the predictor(s)}
#'
#' @details `CI_low` and `CI_high` are only calculated for GLM models because
#' [glmmPQL] does not return confident intervals due to its penalizing
#' behaviour.
#'
#' @author Patrick Schratz <patrick.schratz@gmail.com>
#'
#' @examples
#' ## Example with glm()
#' # load data (source: http://www.ats.ucla.edu/stat/r/dae/logit.htm) and
#' # fit model
#' fit_glm <- glm(admit ~ gre + gpa + rank, data = data_glm,
#'                family = "binomial") # fit model
#'
#' # Calculate OR for specific increment step of continuous variable
#' or_glm(data = data_glm, model = fit_glm, incr = list(gre = 380, gpa = 5))
#'
#' # Calculate OR and change the confidence interval level
#' or_glm(data = data_glm, model = fit_glm,
#'        incr = list(gre = 380, gpa = 5), CI = .70)
#'
#' ## Example with MASS:glmmPQL()
#' # load data
#' library(MASS)
#' data(bacteria)
#' fit_glmmPQL <- glmmPQL(y ~ trt + week, random = ~1 | ID,
#'                        family = binomial, data = bacteria,
#'                        verbose = FALSE)
#'
#' # Apply function
#' or_glm(data = bacteria, model = fit_glmmPQL, incr = list(week = 5))
#'
#' @details Currently supported functions: [glm],
#' [glmmPQL]
#'
#' @seealso [or_gam]
#'
#' @export
or_glm <- function(data, model, incr, CI = 0.95) {

  if (class(model)[1] == "glm") {
    # get pred names and coefficients without intercept
    preds <- names(coefficients(model))[2:length(coefficients(model))]
    coef <- coefficients(model)[2:length(coefficients(model))]
  }

  if (class(model)[1] == "glmmPQL") {
    # get pred names and coefficients without intercept
    preds <- names(model$coefficients$fixed)[2:length(model$coefficients$fixed)]
    coef <- model$coefficients$fixed[2:length(model$coefficients$fixed)]
    cat("Warning: No confident interval calculation possible
        for 'glmmPQL' models\n\n")
  }

  increments <- list()
  odds_ratios <- list()
  CI_low <- list()
  CI_high <- list()
  for (i in preds) {

    # CI calculation
    if (class(model)[1] == "glm") {
      CI_list <- as.data.frame(suppressMessages(confint(model,
                                                        level = CI))) [-1, ]
    }

    # check if predictor is numeric or integer
    if (is.numeric(data[[i]]) | is.integer(data[[i]])) {
      odds_ratios[[i]] <- round(exp(as.numeric(coef[[i]]) * as.numeric(incr[[i]])), 3)
      if (!class(model)[1] == "glmmPQL") {
        CI_low[[i]] <- round(exp(CI_list[i, 1] * as.numeric(incr[[i]])), 3) # nocov
        CI_high[[i]] <- round(exp(CI_list[i, 2] * as.numeric(incr[[i]])), 3) # nocov
      }
      increments[[i]] <- as.numeric(incr[[i]])
      or <- odds_ratios[[i]]
    }
    # if pred is factor -> perform direct conversion to odds ratio
    else {
      odds_ratios[[i]] <- round(exp(as.numeric(coef[[i]])), 3)

      if (!class(model)[1] == "glmmPQL") {
        CI_low[[i]] <- round(exp(CI_list[i, 1]), 3)
        CI_high[[i]] <- round(exp(CI_list[i, 2]), 3)
      }

      increments[[i]] <- "Indicator variable"
      or <- odds_ratios[[i]]
    }
  }

  # set CIs NA if model is of type glmmPQL
  if (class(model)[1] == "glmmPQL") {
    CI_low <- c(rep(NA, length(preds)))
    CI_high <- c(rep(NA, length(preds)))
  }

  # create data frame to return
  result <- data.frame(predictor = as.character(names(odds_ratios)),
                       oddsratio = unlist(odds_ratios, use.names = FALSE),
                       CI_low = unlist(CI_low, use.names = FALSE),
                       CI_high = unlist(CI_high, use.names = FALSE),
                       increment = as.character(unlist(increments,
                                                       use.names = FALSE)),
                       stringsAsFactors = FALSE)

  # set CI column names
  if (class(model)[1] == "glm") {
    colnames(result)[3] <- paste0("CI_low (", names(CI_list) [1], ")")
    colnames(result)[4] <- paste0("CI_high (", names(CI_list) [2], ")")
  }

  return(result)
}
