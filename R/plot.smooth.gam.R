suppressPackageStartupMessages(library(mgcv))
set.seed(1234)
n <- 200
sig <- 2
dat <- gamSim(1, n = n,scale = sig, verbose = FALSE)
dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))
fit.gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4, data = dat)

tmp <- calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
                   percentage = 20, slice = TRUE)
tmp <- calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
                          values = c(0.099, 0.198))


library(ggplot2)
library(cowplot)
library(gridExtra)

#' @param model. A fitted model of class \code{gam}.
#' @param pred The predictor of the fitted model to plot the smooth function of. 
#' @param col.line String. Sets color for smoothing function. Default to \code{"blue"}
#' @param col.line.ci String. Sets color for confident interval line of smoothing function. Default to \code{"black"}
#' @line.type.ci String. Sets linetype of confident interval line of smoothing function. Default to \code{"dashed"}. 
#' @param fill.ci String. Fill color of area between smoothing function and its confident interval lines

plot.smooth.gam(fit.gam, pred = "x2")

plot.smooth.gam <- function(model, pred, col.line = "blue",  col.line.ci = "black",
                            line.type.ci = "dashed", fill.ci = "grey",
                            title = NULL, xlab = NULL, ylab = NULL) {
  
  df <- gam.to.df(model, pred)
  
  if (is.null(xlab)) {
    xlab <- plot.df[[set.pred]]$xlab
  }
  
  if (is.null(ylab)) {
    ylab <- plot.df[[set.pred]]$ylab
  }
  
  plot.gam <- ggplot(df, aes(x, y)) + 
    geom_line(colour = col.line, size = 1.1) + 
    geom_line(aes(x, se.upr), linetype = line.type.ci, 
              colour = col.line.ci, size = 0.8) +
    geom_line(aes(x, se.lwr), linetype = line.type.ci, 
              colour = col.line.ci, size = 0.8) +
    geom_ribbon(aes(x = x, ymin = se.lwr, ymax = se.upr),
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
