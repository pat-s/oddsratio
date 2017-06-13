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

fit_gam <- mgcv::gam(y ~ s(x0) + s(I(x1^2)) + s(x2) + offset(x3) + x4, 
                     data = data_gam)

## ------------------------------------------------------------------------
or_gam(data = data_gam, model = fit_gam, pred = "x2", 
       values = c(0.099, 0.198))

## ------------------------------------------------------------------------
or_gam(data = data_gam, model = fit_gam, 
       pred = "x4", values = c("A", "B"))

## ------------------------------------------------------------------------
or_gam(data = data_gam, model = fit_gam, pred = "x2", 
       percentage = 20, slice = TRUE)

## ------------------------------------------------------------------------
plot_gam(fit_gam, pred = "x2", title = "Predictor 'x2'")

## ------------------------------------------------------------------------
plot_object <- plot_gam(fit_gam, pred = "x2", title = "Predictor 'x2'")
or_object <- or_gam(data = data_gam, model = fit_gam, 
                    pred = "x2", values = c(0.099, 0.198))

plot <- insert_or(plot_object, or_object, or_yloc = 3,
                  values_xloc = 0.05, arrow_length = 0.02, 
                  arrow_col = "red")
plot

## ------------------------------------------------------------------------
or_object2 <- or_gam(data = data_gam, model = fit_gam, 
                     pred = "x2", values = c(0.4, 0.6))

insert_or(plot, or_object2, or_yloc = 2.1, values_yloc = 2,
          line_col = "green4", text_col = "black",
          rect_col = "green4", rect_alpha = 0.2,
          line_alpha = 1, line_type = "dashed",
          arrow_xloc_r = 0.01, arrow_xloc_l = -0.01,
          arrow_length = 0.02, rect = TRUE) 

## ------------------------------------------------------------------------
fit_glm <- glm(admit ~ gre + gpa + rank, data = data_glm, family = "binomial")

## ------------------------------------------------------------------------
or_glm(data = data_glm, model = fit_glm, incr = list(gre = 380, gpa = 5))

## ------------------------------------------------------------------------
or_glm(data = data_glm, model = fit_glm, 
       incr = list(gre = 380, gpa = 5), CI = 0.70)

