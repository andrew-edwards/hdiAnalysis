##' Plot a time series of intervals from multiple samples, as calculated from
##' `create_intervals()` applied to a data frame object.
##'
##' Default plot is to show show ETIs, HDIs, or both (maybe only works for a
##' time series?), as saved in the `intervals_all` component of the list from
##' `create_intervals()`. See example and vignette. Also option to plot the density for each quantity (years in
##' our recruitment example).
##'
##' TODO the hel
##'
##' @param obj a `pacea_recruitment` object, which is a time series. Function
##'   will run on other objects (not give an error) but is not tested on those.
##' @param type
##' @param eti_bar_col
##' @param hdi_bar_col
##' @param add_line_at_0.4 logical whether to add a line at 0.4, specifically
##'   for Figure 2B, also adds the 'Now' and 'Projections' text
##' @param add_line_at_0.4_col colour for 0.4 line
##' @param add_line_at_0.4_lty linetype for 0.4 line
##' @param ylim
##' @param leg_loc
##' @param join_intervals
##' @param value the column to plot if no uncertainties, or what to plot as dots
##'   if showing uncertainties (likely always `median`)
##' @param style `no_uncertainty` for plain time series without uncertainty,
##'   gets overridden to have uncertainty bars if `low` and `high` are columns
##'   of `obj`
##' @param uncertainty_bar_col colour for uncertainty bars for certain types of
##'   plot (e.g. estimated fish recruitment)
##' @param y_max maximum y value for certain types of plot (use this if you get
##'   an error when specifying `ylim`)
##' @param add_line_at_1 whether to add a horizontal line at 1 (only sensible for scaled recruitments)
##' @param add_line_at_1_col colour for line at 1
##' @param add_line_at_1_lty line type of line at 1
##' @param ... further options passed onto `plot.default()`
##' @return plot of the time series as median with bars showing uncertainty (if
##'   `low` and `high` are columns of `obj) to the current device; returns nothing.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' res_all_years <- create_intervals(dplyr::select(hake_recruitment_mcmc,
##'                                                          -"Virgin"))     # TODO
##' plot(res_all_years)
##' }
plot.intervals_density_list <- function(obj,   # an intervals_density_list
                                        # object from create_intervals() on a
                                        # data frame
                        type = "comparison", # or eti or hdi
                        inc = 0.15,
                        x_tick_extra_years = 20,
                        add_big_ticks_x = TRUE,
                        start_big_ticks_x = NULL,
                        eti_bar_col = "blue",
                        hdi_bar_col = "red",
                        add_line_at_0.4 = FALSE,
                        add_line_at_0.4_col = "darkgreen",
                        add_line_at_0.4_lty = 5,
                        ylim = NULL,   # if NULL then created automatically
                        y_tick_start = 0,
                        y_tick_end = NULL,
                        y_tick_by = 1,
                        pch = 20,
                        cex = 0.8,   # Size of points for medians, set to 0 to
                                     # not show medians
                        inset = c(0, -0.02), #c(0.1,-0.02),   # For shifting legend
                        add_legend = TRUE,
                        leg_loc = "topright",
                        join_intervals = FALSE, # join up the ends of the
                                        # intervals, useful for sample size plot
                        arrowhead_length = 0.15,
                        mfrow = c(1, 1), # for plot_densities
                        ...
                        ){
  if(!(type %in% c("comparison", "eti", "hdi"))){
    stop("type needs to be comparison, eti, or hdi.")}

  if(type != "comparison"){
    plot_densities(obj,
                   type = type,  # need to maybe add some more arguments to flow
                   # through, like xlim, xlab, or just call them.
                   mfrow = mfrow,
                   ...)
  }


  if(type == "comparison"){       # Doing a comparison of intervals for each value of
    # quantity, which will be a time series if quantity
    # represents years

    intervals <- obj$intervals_all
    if(!is.numeric(intervals$quantity)){
      stop("plot.intervals_density_list() not yet implemented for non-numeric values of quantity")
    }

    if(is.null(ylim)){
      ylim = c(0,
               max(c(intervals$eti_upper,
                     intervals$hdi_upper)))
    }

    # ETI:
    eti_x_val <- intervals$quantity - inc        # Shift ETI ones to the left
    median_x_val <- eti_x_val                    # Where to plot median, change
    # if don't want both ETI and HDI

    plot(median_x_val,
         intervals$median,  # should be vector
         pch = pch,
         ylim = ylim,
         cex = cex,
         ...)

    segments(x0 = eti_x_val,
             y0 = intervals$eti_lower,
             x1 = eti_x_val,
             y1 = intervals$eti_upper,
             col = eti_bar_col)

    points(median_x_val,
           intervals$median,
           pch = pch,        # plot points again to be on top of bars
           cex = cex)

    # HDI:
    hdi_x_val <- intervals$quantity + inc        # Shift HDI ones to the right
    median_x_val <- hdi_x_val                    # Do this to plot median on HDI.

    # HDI:
    segments(x0 = hdi_x_val,
             y0 = intervals$hdi_lower,
             x1 = hdi_x_val,
             y1 = intervals$hdi_upper,
             col = hdi_bar_col)

    points(median_x_val,
           intervals$median,
           pch = pch,
           cex = cex)

    # abline(h = 0, col = "lightgrey")

    if(join_intervals){
      lines(eti_x_val,
            intervals$eti_lower,
            col = eti_bar_col,
            lty = 2)

      lines(eti_x_val,
            intervals$eti_upper,
            col = eti_bar_col,
            lty = 2)

      lines(hdi_x_val,
            intervals$hdi_lower,
            col = hdi_bar_col,
            lty = 2)

      lines(hdi_x_val,
            intervals$hdi_upper,
            col = hdi_bar_col,
            lty = 2)
    }

    # TODO leave these for now
    # For relative biomass plots
    if(add_line_at_0.4){
      abline(h = 0.4,
             col = add_line_at_0.4_col,
             lty = add_line_at_0.4_lty)
      # Also denote 'now' and 'projections', likely only want if adding the line
      # (implying relative spawning biomass plots; TODO should generalise).
      text(2024,
           0.07,
           "Now")
      # adj = c(0.5, 1))
      #         pos = 4)
      shape::Arrows(2025,
                    -0.05,
                    2027.5,
                    -0.05,
                    lwd = 1,
                    code = 2,
                    col = "black",,
                    arr.type = "triangle",
                    arr.adj = 1,
                    arr.length = arrowhead_length)
      text(2024.7,
           0.05,
           "Projections",
           pos = 4)
    }


    # Adapted from pacea::add_tickmarks():

    min = min(intervals$quantity)
    max = max(intervals$quantity)
    axis(1,
         seq(min,
             max),
         labels = FALSE,
         tcl = -0.2)

    # Slightly larger ticks every 10 values of quantity (every decade if these are
    # years) since not all get labelled automatically
    if(add_big_ticks_x){
      if(is.null(start_big_ticks_x)){
        start_big_ticks_x <- (min(intervals$quantity) %/% 10) * 10
      }

      axis(1,
           seq(start_big_ticks_x,
               max,
               by = 10),
           labels = FALSE,
           tcl = -0.3)
    }
    # y-axis tickmarks:
    if(is.null(y_tick_start)){
      y_tick_start <- floor(par("usr")[3])
    }
    if(is.null(y_tick_end)){
      y_tick_end  <- ceiling(par("usr")[4])
    }

    axis(2,
         seq(y_tick_start,
             y_tick_end,
             by = y_tick_by),
         labels = FALSE,
         tcl = -0.2)

    if(add_legend){
      legend(leg_loc,
             legend = c("Equal-tailed interval",
                        "Highest density interval"),
             lty = 1,
             lwd = 2,
             col = c(eti_bar_col,
                     hdi_bar_col),
             bty = "n",
             inset = inset)
    }


    #  add_tickmarks(intervals,
    #                y_tick_by = y_tick_by,
    #                y_tick_start = 0,
    #                y_tick_end = ceiling(par("usr")[4]),
    #                x_tick_extra_years = x_tick_extra_years,
    #                start_decade_ticks = start_decade_ticks)
  }
  invisible()
}
