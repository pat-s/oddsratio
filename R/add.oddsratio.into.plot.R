#' @name add.oddsratio.into.plot
#' @title Insert odds ratios of GAM(M)s into smoothing function
#' 
#' @description This function inserts calculated odds ratios of GAM(M)s into a plot of a
#' GAM(M) smoothing function.
#' 
#' @import ggplot2 
#' @importFrom cowplot background_grid
#' 
#' @param plot.object A `ggplot` object from \code{\link[oddsratio]{pl.smooth.gam}}
#' @param or.object A returned data.frame from \code{\link[oddsratio]{calc.oddsratio.gam}}
#' @param values Logical. Whether to print predictor value information nearby 
#' the inserted vertical lines. Default to \code{TRUE}.
#' @param rect Logical. Whether to print a shaded rectangle between the vertical lines.
#' @param arrow Logical. Wheter to print arrows above the inserted values. Default to \code{TRUE}.
#' @param line.col,line.alpha,line.type,line.size Aesthetics of vertical lines. 
#' @param text.col,text.alpha,text.size Aesthetics of inserted values.
#' @param rect.col,rect.alpha Aesthetics of shaded rectangle.
#' @param or.yloc,values.yloc Numeric. Specifies y-location of inserted odds ratio / values.
#' Relative to plotted y-axis range. A positive/negative value will place the 
#' the text higher/lower.  
#' @param values.xloc Numeric. X-axis location/shift of values relative to their vertical line.
#' Default to 2\% of x-axis range. 
#' @param arrow.xloc.r,arrow.xloc.l,arrow.yloc,arrow.length,arrow.col Numeric. Axis placement options of inserted arrows.
#' Relative to respective axis ranges.
#' 
#' @details The idea behind this function is to add calculated odds ratio of 
#' fitted GAM models (\code{\link[oddsratio]{calc.oddsratio.gam}}) into a plot 
#' showing the smooth function (\code{\link[oddsratio]{pl.smooth.gam}}) of the chosen 
#' predictor for which the odds ratio was calculated for. Multiple insertions can 
#' be made by iteratively calling the function (see examples).
#' 
#' @details Right now the function does only accept results of 
#' \code{\link[oddsratio]{calc.oddsratio.gam}} with \code{slice = FALSE}. 
#' If you want to insert multiple odds ratio you have to do it iteratively. 
#' 
#' @return Returns a \code{ggplot} plotting object
#' 
#' @seealso \code{\link[oddsratio]{pl.smooth.gam}}
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
#'                                  values = c(0.099, 0.198))
#'                                
#' # insert first odds ratios to plot
#' plot.object <- add.oddsratio.into.plot(plot.object, or.object1, or.yloc = 3,
#'                                        values.xloc = 0.04, line.size = 0.5, 
#'                                        line.type = "dotdash", text.size = 6,
#'                                        values.yloc = 0.5, arrow.col = "red")
#'
#' # calculate second odds ratio
#' or.object2 <- calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
#'                                  values = c(0.4, 0.6))
#'                                   
#' # add or.object2 into plot                                  
#' add.oddsratio.into.plot(plot.object, or.object2, or.yloc = 2.1, values.yloc = 2,
#'                         line.col = "green4", text.col = "black",
#'                         rect.col = "green4", rect.alpha = 0.2,
#'                         line.alpha = 1, line.type = "dashed",
#'                         arrow.xloc.r = 0.01, arrow.xloc.l = -0.01,
#'                         arrow.length = 0.01, rect = TRUE)          
#' @export                       

add.oddsratio.into.plot <- function(
  plot.object, or.object, 
  line.col = "red", line.size = 1.2, line.type = "solid", line.alpha = 1, 
  text.alpha = 1, text.size = 4, text.col = "black",
  rect.alpha = 0.5, rect.col = NULL, 
  rect = FALSE, arrow = TRUE, values = TRUE,
  values.yloc = 0, values.xloc = NULL, or.yloc = 0,
  arrow.length = NULL, arrow.yloc = NULL, arrow.col = NULL,
  arrow.xloc.r = NULL, arrow.xloc.l = NULL) 
{
  
  plot.object <- plot.object + 
    geom_vline(xintercept = or.object$value1, color = line.col, 
               size = line.size, linetype = line.type, alpha = line.alpha) + 
    geom_vline(xintercept = or.object$value2, color = line.col, 
               size = line.size, linetype = line.type, alpha = line.alpha) + 
    annotate("text", x = mean(c(or.object$value2, or.object$value1)),
             y = min(plot.object$data$se.lwr) + or.yloc, 
             label = paste0("OR: \n", round(or.object$oddsratio, 2)),
             color = text.col, size = text.size)
  
  
  
  if (rect) {
    if (is.null(rect.col)) {
      rect.col = text.col
    }

    #set drawing order to place rect behind smoothing fun
    plot.object$layers <- c(geom_rect(data = plot.object$data[1,], # avoids multiple rect drawings
                                      ymin = ggplot_build(plot.object)$panel$ranges[[1]]$y.range[1],
                                      ymax = ggplot_build(plot.object)$panel$ranges[[1]]$y.range[2],
                                      xmin = or.object$value1,
                                      xmax = or.object$value2,
                                      alpha = rect.alpha, fill = rect.col),
                            plot.object$layers)
  }

  if (values) {
    if (is.null(values.xloc)) {
      # calc x range for x-shift 
      values.xloc <- 0.03
      values.xloc <- (max(plot.object$data$x) - min(plot.object$data$x)) * values.xloc
    }
    
    if (is.null(arrow.length)) {
      # calc arrow length from x axis range
      arrow.length <- (max(plot.object$data$x) - min(plot.object$data$x)) * 0.01
    }
    
    if (is.null(arrow.yloc)) {
      # calc arrow height from y axis range
      arrow.yloc <- (max(plot.object$data$y) - min(plot.object$data$y)) * 0.05
    }
    
    if (is.null(arrow.xloc.l)) {
      # calc arrow shift from x axis range
      arrow.xloc.l <- -(max(plot.object$data$y) - min(plot.object$data$y)) * 0.002
    }
    
    if (is.null(arrow.xloc.r)) {
      # calc arrow shift from x axis range
      arrow.xloc.r <- (max(plot.object$data$y) - min(plot.object$data$y)) * 0.002
    }
    
    
    plot.object <- plot.object + 

      annotate("text", x = or.object$value1 - values.xloc,
               y = min(plot.object$data$se.lwr) + values.yloc, 
               label = or.object$value1,
               color = text.col, alpha = text.alpha, size = text.size) +
      annotate("text", x = or.object$value2 + values.xloc,
               y = min(plot.object$data$se.lwr) + values.yloc, 
               label = or.object$value2,
               color = text.col, alpha = text.alpha, size = text.size)
    
    
    if (arrow) {
      
      if (is.null(arrow.col)) {
        # calc arrow shift from x axis range
        arrow.col <- text.col
      }
      
      plot.object <- plot.object + 

        # left arrow
        geom_segment(x = or.object$value1 - values.xloc + arrow.xloc.l,
                     xend = or.object$value1 - values.xloc + arrow.length,
                     y = min(plot.object$data$se.lwr) + values.yloc + arrow.yloc,
                     yend = min(plot.object$data$se.lwr) + values.yloc + arrow.yloc,
                     color = arrow.col,
                     arrow = arrow(length = unit(0.2, "cm"), type = "closed")) + 
        # right arrow
        geom_segment(x = or.object$value2 + values.xloc + arrow.xloc.r,
                     xend = or.object$value2 + values.xloc - arrow.length,
                     y = min(plot.object$data$se.lwr) + values.yloc + arrow.yloc,
                     yend = min(plot.object$data$se.lwr) + values.yloc + arrow.yloc,
                     color = arrow.col, 
                     arrow = arrow(length = unit(0.2, "cm"), type = "closed")) 
    }
    
  }
  return(plot.object)
}