#' @name no.plot
#' @title Suppress plotting output of 'plot()' function
#' 
#' @keywords internal
#' 
#' @description This function suppresses plotting output of 'plot()' function
#' 
#' @importFrom grDevices dev.off png
#' @importFrom graphics plot
#' 
#' @inheritParams calc.oddsratio.gam 
#' 
#' @details To prevent unwanted plot printing of 'plot()' in a function call 
#' in which the only desire is to work with the returned information of 'plot()'.
#' Used in \code{\link[oddsratio]{pl.smooth.gam}}. 
#' 
#' @seealso \code{\link[oddsratio]{pl.smooth.gam}}
#' 
#' @author Patrick Schratz <patrick.schratz@gmail.com>
#' 
#' @examples 
#' # load data (Source: ?mgcv::gam)
#' library(mgcv)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n, scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))
#' fit.gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + 
#'                offset(x3) + x4, data = dat) # fit model
#'                
#' tmp <- plot(fit.gam, pages = 1) # plot output 
#' tmp <- no.plot(fit.gam) # no plot output 
#' @export

no.plot <- function(model) {
  png("temp.xyz")
  plot.df <- plot(model, pages = 1)
  dev.off()
  file.remove("temp.xyz")
  return(invisible(plot.df))
}

#' @name gam.to.df
#' @title Converts a fitted GAM model into a tidy data frame
#' 
#' @keywords internal
#' 
#' @description This function converts a fitted GAM model into a tidy data frame
#' 
#' @inheritParams calc.oddsratio.gam 
#' 
#' @details To be able to plot the smoothing function of a GAM using ggplot2, some preprocessing
#' is needed coming from the raw fitted GAM model output.
#' 
#' Used in \code{\link[oddsratio]{pl.smooth.gam}}. 
#' 
#' @seealso \code{\link[oddsratio]{pl.smooth.gam}}
#' 
#' @author Patrick Schratz
#' 
#' @examples 
#' # load data (Source: ?mgcv::gam)
#' library(mgcv)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n, scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))
#' fit.gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + 
#'                offset(x3) + x4, data = dat) # fit model
#' 
#' tmp <- gam.to.df(fit.gam, "x2") 
#' @export

gam.to.df <- function(model, pred) {
  
  plot.df <- no.plot(model) 
  
  # get list index of spec. predictor
  set.pred <- which(grepl(pred, plot.df))
  
  df <- data.frame(x = plot.df[[set.pred]]$x,
                   se.upr = plot.df[[set.pred]]$fit + plot.df[[set.pred]]$se,
                   se.lwr = plot.df[[set.pred]]$fit - plot.df[[set.pred]]$se,
                   y = plot.df[[set.pred]]$fit)
}

