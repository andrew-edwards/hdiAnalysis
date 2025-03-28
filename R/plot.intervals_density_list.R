##' Plot a time series of intervals from multiple samples, as calculated from
##' `create_intervals()` applied to a data frame object.
##'
##' Default plot is to show ETIs and HDIs as in Fig. 2 of the manuscript, as saved in the
##' `intervals_all` component of the list from
##' `create_intervals()`. See example and vignette. Also includes option to plot
##' separate time-series figures for ETIs and HDIs, or the density for each
##' quantity (years in our recruitment example).
##'
##' @param obj `intervals_density_list` object from running [create_intervals()]
##'   on a data frame, see full description in the return section of `?create_intervals`
##' @param type `comparison` to plot both intervals for each value of `quantity`
##'   (such as year, thus giving a time series, only works if `quantity` is
##'   numeric), `separate` to plot the ETI time series then the HDI time series
##'   (for comparison like in an assessment, since would have one or the other),
##'   or `eti` or `hdi` to plot the
##'   respective full distribution  and interval for each value of `quantity`;
##'   `quantity` is the first column of `obj$intervals_all_years`, corresponding
##'   to the original column names of the data frame used to create `obj`.
##' @param eti_bar_col colour used to plot the ETIs in comparison plot
##' @param hdi_bar_col colour used to plot the HDIs in comparison plot
##' @param add_line_at_0.4 logical whether to add a line at 0.4, specifically
##'   for Figure 2B, also adds the 'Now' and 'Projections' text, so use
##'   `add_line` if do not want those
##' @param add_line_at_0.4_col colour for 0.4 line
##' @param add_line_at_0.4_lty linetype for 0.4 line
##' @param add_line numeric value at which to add a specific line at
##'   (`add_line_at_0.4` is a bit specific to our hake example); if NULL (the
##'   default) then no line
##' @param add_line_col colour for `add_line`
##' @param add_line_lty linetype for `add_line`
##' @param inc amount to shift ETI to left and HDI to right in comparison, tweak
##'   as appropriate (depends on scale of x-axis)
##' @param add_big_ticks_x slightly larger ticks every 10 values of the x-axis
##'   quantity, since not all get labelled automatically
##' @param ylim range for y-axis, if `NULL` (the default) then created
##'   automatically (can probably subsume into `...`);
##' @param y_tick_start where to start y tickmarks
##' @param y_tick_end where to end y tickmarks
##' @param y_tick_by increment for y tickmarks
##' @param pch style of points for medians for comparison plots
##' @param cex size of points for medians for comparison plots; set to 0 to not
##'   show medians
##' @param add_legend logical, whether to add a legend to comparison plot
##' @param leg_loc location of legend, passed onto [legend()] as the `legend`
##'   argument
##' @param inset, vector of length two for shifting the legend (`inset` argument
##'   to [legend()].
##' @param join_intervals logical, if `TRUE` then join up the ends of the
##'   intervals, which can be visually useful
##' @param arrowhead_length length of arrowhead, specifically for the
##'   `add_line_at_0.4` relative biomass plot
##' @param mfrow `par(mfrow)` vector of length two (number of rows and number of
##'   columns to plot panels into) to pass into [plot_densities()]
##' @param add_separate_headings logical whether to add heading titles for each
##'   panel plot when `type == separate`
##' @param ... further arguments passed onto [[plot.default()] for `type ==
##'   "comparison"`, else passed onto [plot_densities()] for `type` being `eti`
##'   or `hdi` (and then onto [plot.intervals_density()]).
##' @return plot of the required style (multiple plots for `type` being `eti` or
##'   `hdi`)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # Also see the vignettes
##' res_all_years <- create_intervals(dplyr::select(hake_recruitment_mcmc,
##'                                                          -"Virgin"))
##' plot(res_all_years)
##' plot(res_all_years,
##'      type = "separate")
##' }
plot.intervals_density_list <- function(obj,
                                        type = "comparison",
                                        eti_bar_col = "blue",
                                        hdi_bar_col = "red",
                                        add_line_at_0.4 = FALSE,
                                        add_line_at_0.4_col = "darkgreen",
                                        add_line_at_0.4_lty = 5,
                                        add_line = NULL,
                                        add_line_col = "grey",
                                        add_line_lty = 5,
                                        inc = 0.15,
                                        add_big_ticks_x = TRUE,
                                        ylim = NULL,
                                        y_tick_start = 0,
                                        y_tick_end = NULL,
                                        y_tick_by = 1,
                                        pch = 20,
                                        cex = 0.8,
                                        add_legend = TRUE,
                                        leg_loc = "topright",
                                        inset = c(0, -0.02),
                                        join_intervals = FALSE,
                                        arrowhead_length = 0.15,
                                        mfrow = c(1, 1),
                                        add_separate_headings = TRUE,
                                        ...
                                        ){
  if(!(type %in% c("comparison", "separate", "eti", "hdi"))){
    stop("type needs to be comparison, separate, eti, or hdi.")}

  if(type %in% c("eti", "hdi")){
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

    # Leave these for now, though quite specific. Ideally would generalise.
    # For relative biomass plots
    if(add_line_at_0.4){
      abline(h = 0.4,
             col = add_line_at_0.4_col,
             lty = add_line_at_0.4_lty)
      # Also denote 'now' and 'projections', likely only want if adding the line
      # (implying relative spawning biomass plots; could generalise).
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

    if(!is.null(add_line)){
      abline(h = add_line,
             col = add_line_col,
             lty = add_line_lty)
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
      start_big_ticks_x <- (min(intervals$quantity) %/% 10) * 10

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
  }

  # Separate plots (could ideally functionalise some of the plotting, since
  # copying it from the comparison one above)
  if(type == "separate"){       # Doing separate ETI and HDI plots, which will
                                # be time series if quantity represents years

    intervals <- obj$intervals_all
    if(!is.numeric(intervals$quantity)){
      stop("plot.intervals_density_list() not yet implemented for non-numeric values of quantity")
    }

    if(is.null(ylim)){
      ylim = c(0,
               max(c(intervals$eti_upper,
                     intervals$hdi_upper)))
    }

    par(mfrow = c(2, 1))

    # ETI:
    plot(intervals$quantity,
         intervals$median,  # should be vector
         pch = pch,
         ylim = ylim,
         cex = cex,
         ...)

    segments(x0 = intervals$quantity,
             y0 = intervals$eti_lower,
             x1 = intervals$quantity,
             y1 = intervals$eti_upper,
             col = eti_bar_col)

    points(intervals$quantity,
           intervals$median,
           pch = pch,        # plot points again to be on top of bars
           cex = cex)
    if(join_intervals){
      lines(intervals$quantity,
            intervals$eti_lower,
            col = eti_bar_col,
            lty = 2)

      lines(intervals$quantity,
            intervals$eti_upper,
            col = eti_bar_col,
            lty = 2)
    }

    # For relative biomass plots
    if(add_line_at_0.4){
      abline(h = 0.4,
             col = add_line_at_0.4_col,
             lty = add_line_at_0.4_lty)
    }

    if(!is.null(add_line)){
      abline(h = add_line,
             col = add_line_col,
             lty = add_line_lty)
    }

    # Adapted from pacea::add_tickmarks(), should really adapt that, since have
    # repeated this below for HDI plot (and above for comparison plot).
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
      start_big_ticks_x <- (min(intervals$quantity) %/% 10) * 10

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

    if(add_separate_headings){
      mtext("A. Equal-tailed intervals",
            side = 3,
            adj = 0,
            line = 0.3)
    }

    # HDI:
    plot(intervals$quantity,
         intervals$median,  # should be vector
         pch = pch,
         ylim = ylim,
         cex = cex,
         ...)

    segments(x0 = intervals$quantity,
             y0 = intervals$hdi_lower,
             x1 = intervals$quantity,
             y1 = intervals$hdi_upper,
             col = hdi_bar_col)

    points(intervals$quantity,
           intervals$median,
           pch = pch,
           cex = cex)

    # abline(h = 0, col = "lightgrey")

    if(join_intervals){
      lines(intervals$quantity,
            intervals$hdi_lower,
            col = hdi_bar_col,
            lty = 2)

      lines(intervals$quantity,
            intervals$hdi_upper,
            col = hdi_bar_col,
            lty = 2)
    }

    # For relative biomass plots
    if(add_line_at_0.4){
      abline(h = 0.4,
             col = add_line_at_0.4_col,
             lty = add_line_at_0.4_lty)
    }

    if(!is.null(add_line)){
      abline(h = add_line,
             col = add_line_col,
             lty = add_line_lty)
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
      start_big_ticks_x <- (min(intervals$quantity) %/% 10) * 10

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

    if(add_separate_headings){
      mtext("B. Highest density intervals",
            side = 3,
            adj = 0,
            line = 0.3)
    }
  }
  invisible()
}
