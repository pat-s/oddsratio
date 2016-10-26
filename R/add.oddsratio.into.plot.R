#' @name add.oddsratio.into.plot
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
#' @param col.line Character. Color of the vertical line showing the predictor values
#' @param col.text Character. Color of the inserted odds ratio information
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
#' @details Right now the function does only accept results of 
#' \code{\link[oddsratio]{calc.oddsratio.gam}} with \code{slice = FALSE}. 
#' If you want to insert multiple odds ratio you have to do it iteratively. 
#' 
#' @return Returns a \code{ggplot} plotting object
#' 
#' @seealso \code{\link[oddsratio]{plot_smooth.gam}}
#' @seealso \code{\link[oddsratio]{calc.oddsratio.gam}}
#' 
#' @author Patrick Schratz <patrick.schratz@gmail.com>
#' 
#' @examples 
#' # load data (Source: ?mgcv::gam)
#' library(mgcv); library(cowplot)
#' set.seed(1234)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n, scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))
#' fit.gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + 
#'                offset(x3) + x4, data = dat) # fit model
#'                
#' # create input objects (plot + odds ratios)   
#' library(oddsratio)            
#' plot.object <- pl.smooth.gam(fit.gam, pred = "x2", title = "Predictor 'x2'")
#' or.object1 <- calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
#'                                values = c(0.099, 0.198))
#'                                
#' # insert first odds ratios to plot
#' plot.object <- add.oddsratio.into.plot(plot.object, or.object1, or.height = 3,
#'                                        x.shift = 0.04, line.size = 0.5, 
#'                                        line.type = "dotdash", text.size = 6,
#'                                        values.height = 0.5)
#'
#' # calculate second odds ratio
#' or.object2 <- calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
#'                                  values = c(0.4, 0.6))
#'                                   
#' # add or.object2 into plot                                  
#' add.oddsratio.into.plot(plot.object, or.object2, or.height = 0, 
#'                         line.col = "green4", text.col = "green4", 
#'                         line.alpha = 0.5, line.type = "dashed",
#'                         arrow.xloc.r = 0.01, arrow.xloc.l = -0.01,
#'                         arrow.length = 0.01)          
#' @export                       

add.oddsratio.into.plot <- function(plot.object, or.object, line.col = "red",
                                    line.size = 1.5, line.type = "solid", 
                                    line.alpha = 1, text.alpha = 1, 
                                    text.size = 4, arrow.length = NULL, 
                                    arrow.height = NULL,
                                    text.col = "red", values = TRUE,
                                    or.height = 0, values.height = 0,
                                    x.shift = NULL, arrow = TRUE,
                                    arrow.xloc.r = 0, arrow.xloc.l = 0) 
  {
  
  plot.object <- plot.object + 
    geom_vline(xintercept = or.object$value1, color = line.col, 
               size = line.size, linetype = line.type, alpha = line.alpha) + 
    geom_vline(xintercept = or.object$value2, color = line.col, 
               size = line.size, linetype = line.type, alpha = line.alpha) + 
    annotate("text", x = mean(c(or.object$value2, or.object$value1)),
             y = min(plot.object$data$se.lwr) + or.height, 
             label = paste0("OR: \n", round(or.object$oddsratio, 2)),
             color = text.col, size = text.size)
  
  if (values) {
    if (is.null(x.shift)) {
      # calc x range for x-shift 
      x.shift <- 0.02
      x.shift <- (max(plot.object$data$x) - min(plot.object$data$x)) * x.shift
    }
    
    if(is.null(arrow.length)) {
      # calc arrow length from x axis range
      arrow.length <- (max(plot.object$data$x) - min(plot.object$data$x)) * 0.02
    }
    
    if(is.null(arrow.height)) {
      # calc arrow height from y axis range
      arrow.height <- (max(plot.object$data$y) - min(plot.object$data$y)) * 0.05
    }
    plot.object <- plot.object + 

      annotate("text", x = or.object$value1 - x.shift,
               y = min(plot.object$data$se.lwr) + values.height, 
               label = or.object$value1,
               color = text.col, alpha = text.alpha, size = text.size) +
      annotate("text", x = or.object$value2 + x.shift,
               y = min(plot.object$data$se.lwr) + values.height, 
               label = or.object$value2,
               color = text.col, alpha = text.alpha, size = text.size)
    
    if (arrow) {
      plot.object <- plot.object + 
        
        # left arrow
        geom_segment(x = or.object$value1 - x.shift + arrow.xloc.l,
                     xend = or.object$value1 - x.shift + arrow.length,
                     y = min(plot.object$data$se.lwr) + values.height + arrow.height,
                     yend = min(plot.object$data$se.lwr) + values.height + arrow.height,
                     color = text.col,
                     arrow = arrow(length = unit(0.2, "cm"))) + 
        # right arrow
        geom_segment(x = or.object$value2 + x.shift + arrow.xloc.r,
                     xend = or.object$value2 + x.shift - arrow.length,
                     y = min(plot.object$data$se.lwr) + values.height + arrow.height,
                     yend = min(plot.object$data$se.lwr) + values.height + arrow.height,
                     color = text.col, 
                     arrow = arrow(length = unit(0.2, "cm"))) 
    }
    
  }
  return(plot.object)
}