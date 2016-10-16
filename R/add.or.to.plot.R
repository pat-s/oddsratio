#' @name add.or.to.plot
#' @title Insert odds ratios of GAM(M)s into smoothing function
#' 
#' @description This function inserts calculated odds ratios of GAM(M)s into a plot of a
#' GAM(M) smoothing function.
#' 
#' @import ggplot2 
#' @importFrom cowplot background_grid
#' 
#' @param plot.object A `ggplot` object from \code{\link[oddsratio]{plot_smooth.gam}}
#' @param or.object A returned data.frame from \code{\link[oddsratio]{calc.oddsratio.gam}}
#' @param col.line String. Color of the vertical line showing the predictor values
#' @param col.text String. Color of the inserted odds ratio information
#' @param values Logical. Whether to print predictor value information nearby 
#' the inserted vertical lines. Default to \code{TRUE}.
#' @param height.or Numeric. Specifies y-location of inserted odds ratio information.
#' Relative to plotted y-axis range. A positive/negative value will place the 
#' odds ratio information higher/lower.  
#' @param height.val Numeric. Specifies y-location of inserted predictor information.
#' Relative to plotted y-axis range. A positive/negative value will place the 
#' predictor information higher/lower.  
#' @param x.shift Numeric. Placement of predictor information relative to its vertical line.
#' Relative to x-axis range. Default to \code{0.02}. 
#' Play around to find the perfect position for you.
#' 
#' @details The logic behind this function is to add calculated odds ratio of 
#' fitted GAM models (\code{\link[oddsratio]{calc.oddsratio.gam}}) into a plot 
#' showing the smooth function (\code{\link[oddsratio]{plot_smooth.gam}}) of the chosen 
#' predictor for which the odds ratio was calculated for. Multiple insertions can 
#' be made by iteratively calling the function (see examples).
#' 
#' @return Returns a \code{ggplot} plotting object
#' 
#' @seealso \code{\link[oddsratio]{plot_smooth.gam}}
#' @seealso \code{\link[oddsratio]{calc.oddsratio.gam}}
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
#' # create input objects (plot + odds ratios)   
#' library(oddsratio)            
#' plot.object <- plot_smooth.gam(fit.gam, pred = "x2", title = "Predictor 'x2'")
#' or.object1 <- calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
#'                                values = c(0.099, 0.198))
#'                                
#' # insert first odds ratios to plot
#' plot.object <- add.or.to.plot(plot.object, or.object1, height.or = 5,
#'                               x.shift = 0.03)
#'
#' # calculate second odds ratio
#' or.object2 <- calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
#'                                   values = c(0.4, 0.6))
#'                                   
#' # add second or into plot                                  
#' add.or.to.plot(plot.object, or.object2, height.or = 3, col.line = "green4",
#'                col.text = "green4")          
#' @export                       

add.or.to.plot <- function(plot.object, or.object, col.line = "red",
                           col.text = "red", values = TRUE,
                           height.or = 0, height.val = 0,
                           x.shift = NULL) {
  
  plot.object <- plot.object + 
    geom_vline(xintercept = or.object$value1, color = col.line) + 
    geom_vline(xintercept = or.object$value2, color = col.line) + 
    annotate("text", x = mean(c(or.object$value2, or.object$value1)),
             y = min(plot.object$data$se.lwr) + height.or, 
             label = paste0("OR: \n", round(or.object$oddsratio, 2)),
             color = col.text)
  
  if (values) {
    if (is.null(x.shift)) {
      # calc x range for x-shift 
      x.shift <- 0.02
      x.shift <- (max(plot.object$data$x) - min(plot.object$data$x)) * x.shift
    }
    plot.object <- plot.object + 
      annotate("text", x = or.object$value1 - x.shift,
               y = min(plot.object$data$se.lwr) + height.val, 
               label = paste0("-->\n", or.object$value1),
               color = col.text) +
      annotate("text", x = or.object$value2 + x.shift,
               y = min(plot.object$data$se.lwr) + height.val, 
               label = paste0("<--\n", or.object$value2),
               color = col.text)
    
  }
  return(plot.object)
}