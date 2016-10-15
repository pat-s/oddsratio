# suppress plotting output of 'plot()' function
no.plot <- function(model) {
  png("temp.xyz")
  plot.df <- plot(model, pages = 1)
  dev.off()
  file.remove("temp.xyz")
  return(invisible(plot.df))
}

# create DF out of fitted gam model to use for plotting
gam.to.df <- function(model, pred) {
  
  plot.df <- no.plot(model) 
  
  # get list index of spec. predictor
  set.pred <- which(grepl(pred, plot.df))
  
  df <- data.frame(x = plot.df[[set.pred]]$x,
                   se.upr = plot.df[[set.pred]]$fit + plot.df[[set.pred]]$se,
                   se.lwr = plot.df[[set.pred]]$fit - plot.df[[set.pred]]$se,
                   y = plot.df[[set.pred]]$fit)
}

