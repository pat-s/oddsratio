## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  fig.align = "center",
  fig.height = 4,
  fig.width = 6,
  collapse = TRUE,
  comment = "#>"
)

## ---- results='hide'-----------------------------------------------------
library(oddsratio)
suppressPackageStartupMessages(library(mgcv))
set.seed(1234)
n <- 200
sig <- 2
dat <- suppressMessages(gamSim(1, n = n, scale = sig))
dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50), rep("D", 50)))

fit.gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4, data = dat)

## ------------------------------------------------------------------------
calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
                   values = c(0.099, 0.198))

## ------------------------------------------------------------------------
calc.oddsratio.gam(data = dat, model = fit.gam, 
                   pred = "x4", values = c("A", "B"))

## ------------------------------------------------------------------------
calc.oddsratio.gam(data = dat, model = fit.gam, pred = "x2", 
                   percentage = 20, slice = TRUE)

## ------------------------------------------------------------------------
plot_smooth.gam(fit.gam, pred = "x2", title = "Predictor 'x2'")

## ------------------------------------------------------------------------
plot.object <- plot_smooth.gam(fit.gam, pred = "x2", title = "Predictor 'x2'")
or.object <- calc.oddsratio.gam(data = dat, model = fit.gam, 
                   pred = "x2", values = c(0.099, 0.198))

plot <- add.oddsratio.into.plot(plot.object, or.object, height.or = 5, x.shift = 0.04)
plot

## ------------------------------------------------------------------------
or.object2 <- calc.oddsratio.gam(data = dat, model = fit.gam, 
                   pred = "x2", values = c(0.4, 0.6))

add.oddsratio.into.plot(plot, or.object2, height.or = 2.5, x.shift = 0.024, 
               col.line = "green4", col.text = "green4")

## ------------------------------------------------------------------------
dat <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
dat$rank <- factor(dat$rank)
fit.glm <- glm(admit ~ gre + gpa + rank, data = dat, family = "binomial")

## ------------------------------------------------------------------------
calc.oddsratio.glm(data = dat, model = fit.glm, incr = list(gre = 380, gpa = 5))

## ------------------------------------------------------------------------
calc.oddsratio.glm(data = dat, model = fit.glm, 
                   incr = list(gre = 380, gpa = 5), CI = 0.70)

