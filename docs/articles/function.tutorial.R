## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  # fig.path = "figures/README-",
  fig.align = "center",
  fig.height = 4,
  fig.width = 6,
  collapse = TRUE,
  comment = "#>"
)
library(ggplot2)
library(cowplot)

## ---- results='hide'-----------------------------------------------------
library(oddsratio)

fit.gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4, data = data.gam)

## ------------------------------------------------------------------------
calc.oddsratio.gam(data = data.gam, model = fit.gam, pred = "x2", 
                   values = c(0.099, 0.198))

## ------------------------------------------------------------------------
calc.oddsratio.gam(data = data.gam, model = fit.gam, 
                   pred = "x4", values = c("A", "B"))

## ------------------------------------------------------------------------
calc.oddsratio.gam(data = data.gam, model = fit.gam, pred = "x2", 
                   percentage = 20, slice = TRUE)

## ------------------------------------------------------------------------
pl.smooth.gam(fit.gam, pred = "x2", title = "Predictor 'x2'")

## ------------------------------------------------------------------------
plot.object <- pl.smooth.gam(fit.gam, pred = "x2", title = "Predictor 'x2'")
or.object <- calc.oddsratio.gam(data = data.gam, model = fit.gam, 
                                pred = "x2", values = c(0.099, 0.198))

plot <- add.oddsratio.into.plot(plot.object, or.object, or.yloc = 3,
                                values.xloc = 0.05, arrow.length = 0.02, 
                                arrow.col = "red")
plot

## ------------------------------------------------------------------------
or.object2 <- calc.oddsratio.gam(data = data.gam, model = fit.gam, 
                                 pred = "x2", values = c(0.4, 0.6))

add.oddsratio.into.plot(plot, or.object2, or.yloc = 2.1, values.yloc = 2,
                        line.col = "green4", text.col = "black",
                        rect.col = "green4", rect.alpha = 0.2,
                        line.alpha = 1, line.type = "dashed",
                        arrow.xloc.r = 0.01, arrow.xloc.l = -0.01,
                        arrow.length = 0.02, rect = TRUE) 

## ------------------------------------------------------------------------
fit.glm <- glm(admit ~ gre + gpa + rank, data = data.glm, family = "binomial")

## ------------------------------------------------------------------------
calc.oddsratio.glm(data = data.glm, model = fit.glm, incr = list(gre = 380, gpa = 5))

## ------------------------------------------------------------------------
calc.oddsratio.glm(data = data.glm, model = fit.glm, 
                   incr = list(gre = 380, gpa = 5), CI = 0.70)

