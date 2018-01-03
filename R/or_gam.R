#' @name or_gam
#' @title Calculate odds ratios of Generalized Additive (Mixed) Models
#'
#' @importFrom stats coefficients predict
#'
#' @description This function calculates odds ratio(s) for specific increment
#'     steps of a GAM(M)s.
#' @description Odds ratios can also be calculated for continuous percentage
#' increment steps across the whole predictor distribution using `slice = TRUE`.
#'
#' @param data The data used for model fitting.
#'
#' @param model A fitted GAM(M).
#'
#' @param pred Character. Predictor name for which to calculate
#' the odds ratio.
#'
#' @param values Numeric vector of length two.
#' Predictor values to estimate odds ratio from. Function is written to use the
#' first provided value as the "lower" one, i.e. calculating the odds ratio
#' 'from value1 to value2'. Only used if `slice = FALSE`.
#'
#' @param percentage Numeric. Percentage number to split the
#' predictor distribution into.
#' A value of 10 would split the predictor distribution by 10\% intervals.
#' Only needed if `slice = TRUE`.
#'
#' @param slice Logical. `Default = FALSE`. Whether to calculate
#' odds ratios for fixed increment steps over the whole predictor distribution.
#' See `percentage` for setting the increment values.
#'
#' @param CI Numeric. Currently fixed to 95\% confidence interval level
#' (2.5\% - 97.5\%).
#' It should not be changed in a function call!
#'
#' @details Currently supported functions: [mgcv::gam],
#' [mgcv::gamm], [gam::gam].
#'
#' @details For [mgcv::gamm], the `model` input of
#' [or_gam] needs to be the `gam` output (e.g. `fit_gam$gam`).
#'
#' @return A data frame with (up to) eight columns. `perc1` and `perc2`
#' are only returned if `slice = TRUE`:
#' \item{predictor}{Predictor name}
#' \item{value1}{First value of odds ratio calculation}
#' \item{value2}{Second value of odds ratio calculation}
#' \item{perc1}{Percentage value of `value1`}
#' \item{perc2}{Percentage value of `value2`}
#' \item{oddsratio}{Calculated odds ratio(s)}
#' \item{CI_low}{Lower \code{(2.5\%)} confident interval of odds ratio}
#' \item{CI_high}{Higher \code{(97.5\%)} confident interval of odds ratio}
#'
#' @seealso [or_glm]
#' @seealso [plot_gam]
#' @seealso [insert_or]
#'
#' @author Patrick Schratz <patrick.schratz@gmail.com>
#'
#' @examples
#' # load data (Source: ?mgcv::gam) and fit model
#' library(mgcv)
#' fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) +
#'                offset(x3) + x4, data = data_gam) # fit model
#'
#' # Calculate OR for specific increment step of continuous variable
#' or_gam(data = data_gam, model = fit_gam, pred = "x2",
#'        values = c(0.099, 0.198))
#'
#' ## Calculate OR for change of indicator variable
#' or_gam(data = data_gam, model = fit_gam, pred = "x4",
#'        values = c("B", "D"))
#'
#' ## Calculate ORs for percentage increments of predictor distribution
#' ## (here: 20%)
#' or_gam(data = data_gam, model = fit_gam, pred = "x2",
#'        percentage = 20, slice = TRUE)
#'
#' @export
or_gam <- function(data = NULL, model = NULL, pred = NULL, values = NULL,
                   percentage = NULL, slice = FALSE, CI = NULL) {

  names_pred <- colnames(data)

  if (slice == TRUE) {
    if (is.null(percentage)) {
      stop(paste0("Please specify 'percentage'.")) # nocov
    }

    # get range of pred distribution and slice in x tiles
    range <- max(data[, pred]) - min(data[, pred])
    step <- range / (100 / percentage)

    range_v <- c()

    # create vector of percentage
    for (i in 0:(100 / percentage)) {
      range_v <- c(range_v, min(data[, pred]) + step * i)
    }

    result <- data.frame(predictor = length(100 / percentage),
                         value1 = numeric(length = 100 / percentage),
                         value2 = numeric(length = 100 / percentage),
                         perc1 = character(length = 100 / percentage),
                         perc2 = character(length = 100 / percentage),
                         oddsratio = numeric(length = 100 / percentage),
                         CI_low = numeric(length = 100 / percentage),
                         CI_high = numeric(length = 100 / percentage),
                         stringsAsFactors = FALSE)

    # apply OR calc for vector
    for (x in 1:(100 / percentage)) {

      # set all preds to their mean if they are numeric
      for (i in names_pred) {
        if (is.numeric(data[[i]]))
          data[[i]] <- mean(data[[i]])
      }

      # reduce to 1 row
      data <- data[1, ]
      # subset DF to preds only
      data <- data[, names_pred]

      # set values[1] of pred
      data[, pred] <- range_v[x]
      # calc log odds for value 1
      pred_gam1 <- as.numeric(predict(model, data,
                                      type = "link", se.fit = TRUE))
      # calc 95% CI log odds (mean +- 2* stdev)
      pred_gam1_CI_low <- pred_gam1[1] - (2 * pred_gam1[2])
      pred_gam1_CI_high <- pred_gam1[1] + (2 * pred_gam1[2])

      # set values[2] of pred
      data[, pred] <- range_v[x + 1]
      # calc log odds for value 2
      pred_gam2 <- as.numeric(predict(model, data,
                                      type = "link", se.fit = TRUE))
      # calc 95% CI log odds (mean +- 2* stdev)
      pred_gam2_CI_low <- pred_gam2[1] - (2 * pred_gam2[2])
      pred_gam2_CI_high <- pred_gam2[1] + (2 * pred_gam2[2])

      result$predictor <- pred
      result$oddsratio[x] <- round(as.numeric(exp(pred_gam2[1] -
                                                    pred_gam1[1])), 2)
      result$value1[x] <- round(range_v[x], 3)
      result$value2[x] <- round(range_v[x + 1], 3)
      result$CI_high[x] <- round(as.numeric(exp(pred_gam2_CI_low -
                                                  pred_gam1_CI_low)), 2) # no mistake # nolint
      result$CI_low[x] <- round(as.numeric(exp(pred_gam2_CI_high -
                                                 pred_gam1_CI_high)), 2) # no mistake # nolint
      result$perc1[x] <- percentage * x - percentage
      result$perc2[x] <- percentage * x
    }

    # change col names
    colnames(result)[7] <- paste0("CI_low (2.5%)")
    colnames(result)[8] <- paste0("CI_high (97.5%)")

    return(result)
  }

  # set all preds to their mean if they are numeric
  for (i in names_pred) {
    if (is.numeric(data[[i]]))
      data[[i]] <- mean(data[[i]])
  }

  # reduce to 1 row
  data <- data[1, ]
  # subset DF to preds only
  data <- data[, names_pred]

  # set values[1] of pred
  data[, pred] <- values[1]
  # calc log odds for value 1
  pred_gam1 <- as.numeric(predict(model, data, type = "link", se.fit = TRUE))
  # calc 95% CI log odds (mean +- 2* stdev)
  pred_gam1_CI_low <- pred_gam1[1] - (2 * pred_gam1[2])
  pred_gam1_CI_high <- pred_gam1[1] + (2 * pred_gam1[2])

  # set values[2] of pred
  data[, pred] <- values[2]
  # calc log odds for value 2
  pred_gam2 <- as.numeric(predict(model, data, type = "link", se.fit = TRUE))
  # calc 95% CI log odds (mean +- 2* stdev)
  pred_gam2_CI_low <- pred_gam2[1] - (2 * pred_gam2[2])
  pred_gam2_CI_high <- pred_gam2[1] + (2 * pred_gam2[2])

  odds_ratio <- as.numeric(exp(pred_gam2[1] - pred_gam1[1]), 2)

  # low and high are somehow switched in the results
  odds_ratio_low <- as.numeric(exp(pred_gam2_CI_low - pred_gam1_CI_low), 2)
  odds_ratio_high <- as.numeric(exp(pred_gam2_CI_high - pred_gam1_CI_high), 2)

  result <- data.frame(predictor = pred,
                       value1 = values[1],
                       value2 = values[2],
                       oddsratio = odds_ratio,
                       CI_low = odds_ratio_high, # no mistake
                       CI_high = odds_ratio_low, # no mistake
                       stringsAsFactors = FALSE)

  # change col names
  colnames(result)[5] <- paste0("CI_low (2.5%)")
  colnames(result)[6] <- paste0("CI_high (97.5%)")

  return(result)
}
