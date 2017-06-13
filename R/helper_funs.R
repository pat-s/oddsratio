#' @name no_plot
#' @title Suppress plotting output of [plot] function
#'
#' @keywords internal
#'
#' @description This function suppresses plotting output of [plot] function
#'
#' @importFrom grDevices dev.off png
#' @importFrom graphics plot
#'
#' @inheritParams or_gam
#'
#' @details To prevent unwanted plot printing of [plot] in a function call
#' in which the only desire is to work with the returned information of
#' [plot]. Used in [plot_gam].
#'
#' @seealso [plot_gam]
#'
#' @author Patrick Schratz <patrick.schratz@gmail.com>
#'
#' @examples
#' # load data (Source: ?mgcv::gam)
#' library(mgcv)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n, scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50),
#'                     rep("D", 50)))
#' fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) +
#'                offset(x3) + x4, data = dat) # fit model
#'
#' tmp <- plot(fit_gam, pages = 1) # plot output
#' tmp <- no_plot(fit_gam) # no plot output
#' @export

no_plot <- function(model = NULL) {
  png("temp.xyz")
  plot_df <- plot(model, pages = 1)
  dev.off()
  file.remove("temp.xyz")
  return(invisible(plot_df))
}

#' @name gam_to_df
#' @title Converts a fitted GAM model into a tidy data frame
#'
#' @keywords internal
#'
#' @description This function converts a fitted GAM model into a tidy data frame
#'
#' @inheritParams or_gam
#'
#' @details To be able to plot the smoothing function of a GAM using ggplot2,
#' some preprocessing is needed coming from the raw fitted GAM model output.
#'
#' Used in [plot_gam].
#'
#' @seealso [plot_gam]
#'
#' @author Patrick Schratz
#'
#' @examples
#' # load data (Source: ?mgcv::gam)
#' library(mgcv)
#' n <- 200
#' sig <- 2
#' dat <- gamSim(1, n = n, scale = sig, verbose = FALSE)
#' dat$x4 <- as.factor(c(rep("A", 50), rep("B", 50), rep("C", 50),
#'                     rep("D", 50)))
#' fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) +
#'                offset(x3) + x4, data = dat) # fit model
#'
#' tmp <- gam_to_df(fit_gam, "x2")
#' @export

gam_to_df <- function(model = NULL, pred = NULL) {

  plot_df <- no_plot(model)

  # get list index of spec. predictor
  set_pred <- which(grepl(pred, plot_df))

  df <- data.frame(x = plot_df[[set_pred]]$x,
                   se_upr = plot_df[[set_pred]]$fit + plot_df[[set_pred]]$se,
                   se_lwr = plot_df[[set_pred]]$fit - plot_df[[set_pred]]$se,
                   y = plot_df[[set_pred]]$fit)
  return(df)
}