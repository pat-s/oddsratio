#' @name pl.smooth.gam
#' @title Plot smoothing functions of GAM(M) models
#' 
#' @description This function plots the smoothing function of selected GAM(M) models
#' using the \code{ggplot2} plotting system.
#' 
#' @import ggplot2 
#' @importFrom cowplot background_grid
#' @importFrom grDevices dev.off png
#' @importFrom graphics plot

#' @param model A fitted model of class \code{gam}.
#' @param pred The predictor of the fitted model to plot the smooth function of. 
#' @param col.line Character. Sets color for smoothing function. Default to \code{"blue"}.
#' @param ci.line.col Character. Sets color for confident interval line of smoothing function. Default to \code{"black"}
#' @param ci.line.type Character. Sets linetype of confident interval line of smoothing function. Default to \code{"dashed"}. 
#' @param ci.fill Character. Fill color of area between smoothing function and its confident interval lines.
#' @param ci.alpha Numeric [0,1]. Opacity value of confidence interval shading.
#' @param ci.line.size,sm.fun.size Line sizes.
#' @param title Character. Plot title.
#' @param xlab Character. X-axis title.
#' @param ylab Character. Y-axis title.
#' @param limits.y Numeric of length two. Sets y-axis limits.
#' @param breaks.y Numeric of length three. Sets y-axis breaks. See \code{\link[base]{seq}}. 
#' Values need to be given in a 'seq()' call, e.g. seq(-6,6,2). 
#' 
#' @examples 
#' # load data (Source: ?mgcv::gam) and fit model
#' library(mgcv)
#' fit.gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4, data = data.gam)
#' 
#' library(oddsratio)
#' pl.smooth.gam(fit.gam, pred = "x2", title = "Predictor 'x2'")
#' 
#' @seealso \code{\link[oddsratio]{pl.smooth.gam}}
#' @seealso \code{\link[oddsratio]{calc.oddsratio.gam}}
#' @seealso \code{\link[oddsratio]{add.oddsratio.into.plot}}
#' 
#' @author Patrick Schratz <patrick.schratz@gmail.com>
#' @export


pl.smooth.gam <- function(
  model, pred, 
  col.line = "blue",  ci.line.col = "black", ci.line.type = "dashed", 
  ci.fill = "grey", ci.alpha = 0.4, ci.line.size = 0.8, sm.fun.size = 1.1,
  title = NULL, xlab = NULL, ylab = NULL,
  limits.y = NULL, breaks.y = NULL) 
{
  
  df <- gam.to.df(model, pred)
  
  if (is.null(xlab)) {
    xlab <- df[[pred]]$xlab
  }
  
  if (is.null(ylab)) {
    ylab <- df[[pred]]$ylab
  }
  
  plot.gam <- ggplot(df, aes_(~x, ~y)) + 
    geom_line(colour = col.line, size = sm.fun.size) + 
    geom_line(aes_(~x, ~se.upr), linetype = ci.line.type, 
              colour = ci.line.col, size = ci.line.size) +
    geom_line(aes_(~x, ~se.lwr), linetype = ci.line.type, 
              colour = ci.line.col, size = ci.line.size) +
    geom_ribbon(aes_(x = ~x, ymin = ~se.lwr, ymax = ~se.upr),
                fill = ci.fill, alpha = ci.alpha) +
    ylab(ylab) + 
    xlab(xlab) +
    background_grid(major = "xy", minor = "none") 
  
  if (!is.null(limits.y) & !is.null(breaks.y)) {
    plot.gam <- plot.gam + 
      scale_y_continuous(breaks = c(breaks.y), limits = c(limits.y))
  }
  else if (!is.null(limits.y) & is.null(breaks.y)) {
    plot.gam <- plot.gam + 
      scale_y_continuous(limits = limits.y)
  }
  else if (is.null(limits.y) & !is.null(breaks.y)) {
    plot.gam <- plot.gam + 
      scale_y_continuous(breaks = c(breaks.y))
  }
  
  
  # optional ggplot arguments
  if (!is.null(title)) {
    plot.gam <- plot.gam + ggtitle(title)
  }
  
  return(plot.gam)
}
