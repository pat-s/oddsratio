context("insert_or")

test_that("check bevhaviour of ggplot_build (changed in ggplot2 v2.2)", {
  data("data_gam")
  fit_gam <- gam(y ~ s(x0) + s(I(x1 ^ 2)) + s(x2) + offset(x3) + x4, 
                 data = data_gam)
  
  library(oddsratio)            
  plot_object <- plot_gam(fit_gam, pred = "x2", title = "Predictor 'x2'")
  
  # old way to do it (ggplot2 < v2.2)
  # ymin = ggplot_build(plot_object)$panel$ranges[[1]]$y.range[1]
  # ymax = ggplot_build(plot_object)$panel$ranges[[1]]$y.range[2]
  
  # new way to do it (ggplot2 > v2.2)
  ymin = ggplot_build(plot_object)$layout$panel_ranges[[1]]$y.range[1]
  ymax = ggplot_build(plot_object)$layout$panel_ranges[[1]]$y.range[2]
  
  expect_equal(class(ymin), "numeric")
  expect_equal(class(ymax), "numeric")
  expect_equal(length(ggplot_build(plot_object)$layout$panel_ranges[[1]]$
                        y.range), 2)
  
}) 