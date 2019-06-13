#' @name plot_gam
#' @title Plot smoothing functions of GAM(M) models
#'
#' @description This function plots the smoothing function of selected GAM(M) models
#' using the `ggplot2` plotting system.
#'
#' @import ggplot2
#' @importFrom grDevices dev.off png
#' @importFrom graphics plot
#' @param model A fitted model of class `gam`.
#'
#' @param pred The predictor of the fitted model to plot the smooth function of.
#'
#' @param col_line Character. Sets color for smoothing function. Default to
#' `"blue"`.
#'
#' @param ci_line_col Character. Sets color for confident interval line of
#' smoothing function. Default to `"black"`.
#'
#' @param ci_line_type Character. Sets linetype of confident interval line
#' of smoothing function. Default to `"dashed"`.
#'
#' @param ci_fill Character. Fill color of area between smoothing function and
#' its confident interval lines.
#'
#' @param ci_alpha Numeric (range: 0-1). Opacity value of confidence interval shading.
#'
#' @param ci_line_size,sm_fun_size Line sizes.
#'
#' @param title Character. Plot title.
#'
#' @param xlab Character. X-axis title.
#'
#' @param ylab Character. Y-axis title.
#'
#' @param limits_y Numeric of length two. Sets y-axis limits.
#'
#' @param breaks_y Numeric of length three. Sets y-axis breaks.
#' See [seq].
#' Values need to be given in a `seq` call, e.g. `seq(-6, 6, 2)`.
#'
#' @examples
#' # load data (Source: ?mgcv::gam) and fit model
#' library(mgcv)
#' fit_gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4,
#'   data = data_gam
#' )
#'
#' library(oddsratio)
#' plot_gam(fit_gam, pred = "x2", title = "Predictor 'x2'")
#' @seealso [plot_gam]
#' @seealso [or_gam]
#' @seealso [insert_or]
#'
#' @author Patrick Schratz <patrick.schratz@gmail.com>
#' @export

plot_gam <- function(model = NULL, pred = NULL, col_line = "blue", # nocov start
                     ci_line_col = "black", ci_line_type = "dashed",
                     ci_fill = "grey", ci_alpha = 0.4,
                     ci_line_size = 0.8, sm_fun_size = 1.1, title = NULL,
                     xlab = NULL, ylab = NULL, limits_y = NULL,
                     breaks_y = NULL) {

  df <- gam_to_df(model, pred) # nolint

  if (is.null(xlab)) {
    xlab <- df[[pred]]$xlab
  }

  if (is.null(ylab)) {
    ylab <- df[[pred]]$ylab
  }

  plot_gam <- ggplot(df, aes_(~x, ~y)) +
    geom_line(colour = col_line, size = sm_fun_size) +
    geom_line(aes_(~x, ~se_upr),
      linetype = ci_line_type,
      colour = ci_line_col, size = ci_line_size
    ) +
    geom_line(aes_(~x, ~se_lwr),
      linetype = ci_line_type,
      colour = ci_line_col, size = ci_line_size
    ) +
    geom_ribbon(aes_(x = ~x, ymin = ~se_lwr, ymax = ~se_upr),
      fill = ci_fill, alpha = ci_alpha
    ) +
    ylab(ylab) +
    xlab(xlab)

  if (!is.null(limits_y) & !is.null(breaks_y)) {
    plot_gam <- plot_gam +
      scale_y_continuous(breaks = c(breaks_y), limits = c(limits_y))
  }
  else if (!is.null(limits_y) & is.null(breaks_y)) {
    plot_gam <- plot_gam +
      scale_y_continuous(limits = limits_y)
  }
  else if (is.null(limits_y) & !is.null(breaks_y)) {
    plot_gam <- plot_gam +
      scale_y_continuous(breaks = c(breaks_y))
  }

  # optional ggplot arguments
  if (!is.null(title)) {
    plot_gam <- plot_gam + ggtitle(title) # nolint
  }

  return(plot_gam)
} # nocov end
