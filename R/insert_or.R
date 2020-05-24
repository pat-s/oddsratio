#' @title Insert odds ratios of GAM(M)s into smoothing function
#'
#' @description This function inserts calculated odds ratios of GAM(M)s into a
#'   plot of a GAM(M) smoothing function.
#'
#' @import ggplot2
#'
#' @param plot_object A `ggplot` object from [plot_gam()].
#' @param or_object A [data.frame] as returned from [or_gam()].
#' @param values Whether to print predictor value information nearby the
#'   inserted vertical lines.
#' @param rect Whether to print a shaded rectangle between the
#' vertical lines.
#' @param arrow Whether to print arrows above the inserted values.
#' @param line_col,line_alpha,line_type,line_size Aesthetics of vertical lines.
#' @param text_col,text_alpha,text_size Aesthetics of inserted values.
#' @param rect_col,rect_alpha Aesthetics of shaded rectangle.
#' @param or_yloc,values_yloc Specifies y-location of inserted odds ratio
#'   values. Relative to plotted y-axis range. A positive (negative) value will
#'   place the the text higher (lower).
#' @param values_xloc x-axis location/shift of values relative to their vertical
#'   line. Default to 2\\% of x-axis range.
#' @param arrow_xloc_r,arrow_xloc_l,arrow_yloc,arrow_length,arrow_col Axis
#'   placement options of inserted arrows. Relative to respective axis ranges.
#'
#' @details
#' The idea behind this function is to add calculated odds ratios of fitted GAM
#' models ([or_gam()]) into a plot showing the smooth function ([plot_gam]) of
#' the chosen predictor for which the odds ratio was calculated for. Multiple
#' insertions can be made by iterative calling the function (see examples).
#'
#' Right now the function only accepts inputs from [or_gam()] objects with
#' `slice = FALSE`. If you want to insert multiple odds ratio values, call the
#' function multiple times.
#'
#' @return [ggplot2]
#' @seealso [plot_gam()], [or_gam()]
#'
#' @examples
#' library(oddsratio)
#' library(mgcv)
#' fit_gam <- gam(y ~ s(x0) + s(I(x1^2)) + s(x2) +
#'   offset(x3) + x4, data = data_gam) # fit model
#'
#' # create input objects (plot + odds ratios)
#' plot_object <- plot_gam(fit_gam, pred = "x2", title = "Predictor 'x2'")
#' or_object1 <- or_gam(
#'   data = data_gam, model = fit_gam,
#'   pred = "x2", values = c(0.099, 0.198)
#' )
#'
#' # insert first odds ratios to plot
#' plot_object <- insert_or(plot_object, or_object1,
#'   or_yloc = 3,
#'   values_xloc = 0.04, line_size = 0.5,
#'   line_type = "dotdash", text_size = 6,
#'   values_yloc = 0.5, arrow_col = "red"
#' )
#'
#' # calculate second odds ratio
#' or_object2 <- or_gam(
#'   data = data_gam, model = fit_gam, pred = "x2",
#'   values = c(0.4, 0.6)
#' )
#'
#' # add or_object2 into plot
#' insert_or(plot_object, or_object2,
#'   or_yloc = 2.1, values_yloc = 2,
#'   line_col = "green4", text_col = "black",
#'   rect_col = "green4", rect_alpha = 0.2,
#'   line_alpha = 1, line_type = "dashed",
#'   arrow_xloc_r = 0.01, arrow_xloc_l = -0.01,
#'   arrow_length = 0.01, rect = TRUE
#' )
#' @export
insert_or <- function(plot_object = NULL,
                      or_object = NULL,
                      line_col = "red",
                      line_size = 1.2,
                      line_type = "solid",
                      line_alpha = 1,
                      text_alpha = 1,
                      text_size = 4,
                      text_col = "black",
                      rect_alpha = 0.5,
                      rect_col = NULL,
                      rect = FALSE,
                      arrow = TRUE,
                      values = TRUE,
                      values_yloc = 0,
                      values_xloc = NULL,
                      or_yloc = 0,
                      arrow_length = NULL,
                      arrow_yloc = NULL,
                      arrow_col = NULL,
                      arrow_xloc_r = NULL,
                      arrow_xloc_l = NULL) {

  plot_object <- plot_object +
    geom_vline(
      xintercept = or_object$value1, color = line_col,
      size = line_size, linetype = line_type, alpha = line_alpha
    ) +
    geom_vline(
      xintercept = or_object$value2, color = line_col,
      size = line_size, linetype = line_type, alpha = line_alpha
    ) +
    annotate("text",
      x = mean(c(or_object$value2, or_object$value1)),
      y = min(plot_object$data$se_lwr) + or_yloc,
      label = paste0("OR: \n", round(or_object$oddsratio, 2)),
      color = text_col, size = text_size
    )

  if (rect) {
    if (is.null(rect_col)) {
      rect_col <- text_col
    }

    # set drawing order to place rect behind smoothing fun
    plot_object$layers <- c(
      geom_rect(
        data = plot_object$data[1, ], # avoids multiple rect drawings # nolint
        ymin = ggplot_build(plot_object)$layout$panel_params[[1]]$y.range[1],
        ymax = ggplot_build(plot_object)$layout$panel_params[[1]]$y.range[2],
        xmin = or_object$value1,
        xmax = or_object$value2,
        alpha = rect_alpha, fill = rect_col
      ),
      plot_object$layers
    )
  }

  if (values) {
    if (is.null(values_xloc)) {
      # calc x range for x-shift
      values_xloc <- 0.03
      values_xloc <- (max(plot_object$data$x) - min(plot_object$data$x)) *
        values_xloc
    }

    if (is.null(arrow_length)) {
      # calc arrow length from x axis range
      arrow_length <- (max(plot_object$data$x) - min(plot_object$data$x)) * 0.01
    }

    if (is.null(arrow_yloc)) {
      # calc arrow height from y axis range
      arrow_yloc <- (max(plot_object$data$y) - min(plot_object$data$y)) * 0.05
    }

    if (is.null(arrow_xloc_l)) {
      # calc arrow shift from x axis range
      arrow_xloc_l <- - (max(plot_object$data$y) - min(plot_object$data$y)) *
        0.002
    }

    if (is.null(arrow_xloc_r)) {
      # calc arrow shift from x axis range
      arrow_xloc_r <- (max(plot_object$data$y) - min(plot_object$data$y)) *
        0.002
    }

    plot_object <- plot_object +
      annotate("text",
        x = or_object$value1 - values_xloc,
        y = min(plot_object$data$se_lwr) + values_yloc,
        label = or_object$value1,
        color = text_col, alpha = text_alpha, size = text_size
      ) +
      annotate("text",
        x = or_object$value2 + values_xloc,
        y = min(plot_object$data$se_lwr) + values_yloc,
        label = or_object$value2,
        color = text_col, alpha = text_alpha, size = text_size
      )

    if (arrow) {

      if (is.null(arrow_col)) {
        # calc arrow shift from x axis range
        arrow_col <- text_col
      }

      plot_object <- plot_object +

        # left arrow
        geom_segment(
          x = or_object$value1 - values_xloc + arrow_xloc_l,
          xend = or_object$value1 - values_xloc + arrow_length,
          y = min(plot_object$data$se_lwr) + values_yloc +
            arrow_yloc,
          yend = min(plot_object$data$se_lwr) + values_yloc +
            arrow_yloc,
          color = arrow_col,
          arrow = arrow(length = unit(0.2, "cm"), type = "closed")
        ) +
        # right arrow
        geom_segment(
          x = or_object$value2 + values_xloc + arrow_xloc_r,
          xend = or_object$value2 + values_xloc - arrow_length,
          y = min(plot_object$data$se_lwr) + values_yloc +
            arrow_yloc,
          yend = min(plot_object$data$se_lwr) + values_yloc +
            arrow_yloc,
          color = arrow_col,
          arrow = arrow(length = unit(0.2, "cm"), type = "closed")
        )
    }
  }
  return(plot_object)
}
