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
#' @param col.line.ci Character. Sets color for confident interval line of smoothing function. Default to \code{"black"}
#' @param line.type.ci Character. Sets linetype of confident interval line of smoothing function. Default to \code{"dashed"}. 
#' @param fill.ci Character. Fill color of area between smoothing function and its confident interval lines.
#' @param title Character. Plot title.
#' @param xlab Character. X-axis title.
#' @param ylab Character. Y-axis title.
#' 
#' @examples 
#' suppressPackageStartupMessages(library(mgcv))
#' set.seed(1234)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n, scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))
#' fit.gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4, data = dat)
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


pl.smooth.gam <- function(model, pred, col.line = "blue",  col.line.ci = "black",
                            line.type.ci = "dashed", fill.ci = "grey",
                            title = NULL, xlab = NULL, ylab = NULL) {
  
  df <- gam.to.df(model, pred)
  
  if (is.null(xlab)) {
    xlab <- df[[pred]]$xlab
  }
  
  if (is.null(ylab)) {
    ylab <- df[[pred]]$ylab
  }
  
  plot.gam <- ggplot(df, aes_(~x, ~y)) + 
    geom_line(colour = col.line, size = 1.1) + 
    geom_line(aes_(~x, ~se.upr), linetype = line.type.ci, 
              colour = col.line.ci, size = 0.8) +
    geom_line(aes_(~x, ~se.lwr), linetype = line.type.ci, 
              colour = col.line.ci, size = 0.8) +
    geom_ribbon(aes_(x = ~x, ymin = ~se.lwr, ymax = ~se.upr),
                fill = fill.ci, alpha = 0.4) +
    scale_y_continuous(breaks = c(seq(-6, 6, 2)), limits = c(-6, 6)) + 
    ylab(ylab) + 
    xlab(xlab) +
    background_grid(major = "xy", minor = "none") 
  
  # optional ggplot arguments
  if (!is.null(title)) {
    plot.gam <- plot.gam + ggtitle(title)
  }
  
  return(plot.gam)
}
