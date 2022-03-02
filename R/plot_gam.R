#' @name plot_gam
#' @title Plot GAM(M) Smoothing Function
#'
#' @description Plots the smoothing function of GAM(M) predictors via ggplot2
#'
#' @import ggplot2
#' @importFrom grDevices dev.off png
#' @importFrom graphics plot
#'
#' @param model A fitted model of class `gam`.
#' @param pred Predictor name.
#' @param col_line Smoothing function line color.
#' @param ci_line_col Confident interval line color.
#' @param ci_line_type Linetype of confidence interval.
#' @param ci_fill Fill color of area between smoothing function and its
#'   confidence interval lines.
#' @param ci_alpha Opacity value of confidence interval.
#' @param ci_line_size,sm_fun_size Line sizes.
#' @param title Plot title.
#' @param xlab x-axis title.
#' @param ylab y-axis title.
#' @param limits_y y-axis limits.
#' @param breaks_y y-axis breaks. Values are handed over to a `seq` call, e.g.
#'   `seq(-6, 6, 2)`.
#'
#' @examples
#' library(oddsratio)
#' library(mgcv)
#' fit_gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4,
#'   data = data_gam
#' )
#'
#' plot_gam(fit_gam, pred = "x2", title = "Predictor 'x2'")
#' @seealso [or_gam()] [insert_or()]
#' @export
plot_gam <- function(model = NULL,
                     pred = NULL,
                     col_line = "blue",
                     ci_line_col = "black",
                     ci_line_type = "dashed",
                     ci_fill = "grey",
                     ci_alpha = 0.4,
                     ci_line_size = 0.8,
                     sm_fun_size = 1.1,
                     title = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     limits_y = NULL,
                     breaks_y = NULL) {
  df <- gam_to_df(model, pred)

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
  } else if (!is.null(limits_y) & is.null(breaks_y)) {
    plot_gam <- plot_gam +
      scale_y_continuous(limits = limits_y)
  } else if (is.null(limits_y) & !is.null(breaks_y)) {
    plot_gam <- plot_gam +
      scale_y_continuous(breaks = c(breaks_y))
  }

  # optional ggplot arguments
  if (!is.null(title)) {
    plot_gam <- plot_gam +
      ggtitle(title)
  }

  return(plot_gam)
}
