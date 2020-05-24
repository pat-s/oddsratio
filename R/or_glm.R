#' @name or_glm
#' @title Calculate Odds Ratios of Generalized Linear (Mixed) Models
#'
#' @importFrom stats coefficients
#' @importFrom stats confint
#' @import mgcv
#'
#' @description This function calculates odds ratio(s) for specific increment
#'   steps of GLMs.
#'
#' @param data The data used for model fitting.
#' @param model A fitted GLM(M).
#' @param incr Increment values of each predictor given in a named list.
#' @param ci Which confidence interval to calculate. Must be between 0
#'   and 1. Default to 0.95
#'
#' @return A data frame with five columns:
#'   \item{predictor}{Predictor name(s)}
#'   \item{oddsratio}{Calculated odds ratio(s)}
#'   \item{ci_low}{Lower confident interval of odds ratio}
#'   \item{ci_high}{Higher confident interval of odds ratio}
#'   \item{increment}{Increment of the predictor(s)}
#'
#' @details `ci_low` and `ci_high` are only calculated for GLM models because
#'   [MASS::glmmPQL()] does not return confident intervals due to its penalizing
#'   behavior.
#'
#'   Currently supported functions: [stats::glm],[MASS::glmmPQL]
#'
#' @examples
#' ## Example with glm()
#' library(oddsratio)
#' # load data (source: http://www.ats.ucla.edu/stat/r/dae/logit.htm) and
#' # fit model
#' fit_glm <- glm(admit ~ gre + gpa + rank,
#'   data = data_glm,
#'   family = "binomial"
#' ) # fit model
#'
#' # Calculate OR for specific increment step of continuous variable
#' or_glm(data = data_glm, model = fit_glm, incr = list(gre = 380, gpa = 5))
#'
#' # Calculate OR and change the confidence interval level
#' or_glm(
#'   data = data_glm, model = fit_glm,
#'   incr = list(gre = 380, gpa = 5), ci = .70
#' )
#'
#' ## Example with MASS:glmmPQL()
#' # load data
#' library(MASS)
#' data(bacteria)
#' fit_glmmPQL <- glmmPQL(y ~ trt + week,
#'   random = ~ 1 | ID,
#'   family = binomial, data = bacteria,
#'   verbose = FALSE
#' )
#'
#' # Apply function
#' or_glm(data = bacteria, model = fit_glmmPQL, incr = list(week = 5))
#' @seealso [or_gam()]
#' @export
or_glm <- function(data,
                   model,
                   incr,
                   ci = 0.95) {

  if (class(model)[1] == "glm") {
    # get pred names and coefficients without intercept
    preds <- names(coefficients(model))[2:length(coefficients(model))]
    coef <- coefficients(model)[2:length(coefficients(model))]
  }

  if (class(model)[1] == "glmmPQL") {
    # get pred names and coefficients without intercept
    preds <- names(model$coefficients$fixed)[2:length(model$coefficients$fixed)]
    coef <- model$coefficients$fixed[2:length(model$coefficients$fixed)]
    warning("No confident interval calculation possible for 'glmmPQL' models.", call. = FALSE) # nolint
  }

  increments <- list()
  odds_ratios <- list()
  ci_low <- list()
  ci_high <- list()

  for (i in preds) {

    # ci calculation
    if (class(model)[1] == "glm") {
      ci_list <- data.frame(suppressMessages(confint(model,
        level = ci
      ))) [-1, ]
    }

    # check if predictor is numeric or integer
    if (is.numeric(data[[i]]) | is.integer(data[[i]])) {
      odds_ratios[[i]] <- round(exp(as.numeric(coef[[i]]) *
        as.numeric(incr[[i]])), 3)
      if (!class(model)[1] == "glmmPQL") {
        ci_low[[i]] <- round(exp(ci_list[i, 1] * # nocov start
          as.numeric(incr[[i]])), 3)
        ci_high[[i]] <- round(exp(ci_list[i, 2] *
          as.numeric(incr[[i]])), 3) # nocov end
      }
      increments[[i]] <- as.numeric(incr[[i]])
      or <- odds_ratios[[i]] # nolint
    }
    # if pred is factor -> perform direct conversion to odds ratio
    else {
      odds_ratios[[i]] <- round(exp(as.numeric(coef[[i]])), 3)

      if (!class(model)[1] == "glmmPQL") {
        ci_low[[i]] <- round(exp(ci_list[i, 1]), 3)
        ci_high[[i]] <- round(exp(ci_list[i, 2]), 3)
      }

      increments[[i]] <- "Indicator variable"
      or <- odds_ratios[[i]]
    }
  }

  # set cis NA if model is of type glmmPQL
  if (class(model)[1] == "glmmPQL") {
    ci_low <- c(rep(NA, length(preds)))
    ci_high <- c(rep(NA, length(preds)))
  }

  # create data frame to return
  result <- data.frame(
    predictor = names(odds_ratios),
    oddsratio = unlist(odds_ratios, use.names = FALSE),
    ci_low = unlist(ci_low, use.names = FALSE),
    ci_high = unlist(ci_high, use.names = FALSE),
    increment = unlist(increments,
      use.names = FALSE
    )
  )

  # set ci column names
  if (class(model)[1] == "glm") {

    # Clean variable names

    col_names <- gsub("\\.\\.", replacement = "", names(ci_list))
    col_names <- gsub("X", replacement = "", col_names)

    colnames(result)[3] <- paste0("ci_low (", col_names[1], ")")
    colnames(result)[4] <- paste0("ci_high (", col_names[2], ")")
  }

  return(result)
}
