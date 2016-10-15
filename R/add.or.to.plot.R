#' 
#'
#'
#'
#'
#'
#' @examples 
#' #' # load data (Source: ?mgcv::gam)
#' library(mgcv)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n,scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))
#' fit.gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + 
#'                offset(x3) + x4, data = dat) # fit model
#' @name add.or.to.plot
#' @title Inserts calculated odds ratios into plot
#' 
#' @description This function inserts odds ratio information into a plot created
#' by \code{\link[oddsratio]{plot.smooth.gam}}
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 annotate
#' 
#' 
#' # create input objects (plot + odds ratios)               
#' plot.object <- plot.smooth.gam(fit.gam, pred = "x2", title = "Predictor 'x2'")
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