##' Plot intervals as points and colour them in if inside/outside the ETI or HDI.
##'
##' Not in the paper, but useful for explaining in talks.
##'
##' @param values a vector of values to use, e.g. MCMC outputs
##' @param results object of class `intervals_density`, from doing
##'   `create_intervals(values)`. Would rather not have to recalculate this each time.
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # see hdi-talk.Rmd
##' }
plot_pointwise_intervals <- function(values,
                                     ints_dens,
                                     type = "eti",   # type of plot to do, can
                                        # be "moving_window" also
                                     col_main = "blue",
                                     col_main_text = NULL,
                                     col_tail = "red",
                                     jitter_amount = 0.005,
                                     interval_arrows = TRUE,
                                     arrowhead_gap = 0,
                                     arrowhead_length = 0.2,
                                     y_lim = c(0.95, 1.05),
                                     y_arrow = 1.02,
                                     moving_window_start = 2,  # index to start
                                        # the interval
                                     ...){
  vals <- values
  n <- length(vals)

  ints <- ints_dens$intervals
  credibility <- ints_dens$credibility
  eti_lower_percentile <- (1 - credibility)/2 * 100   # For annotating

  # Default is to make text the same colour for main interval
  if(is.null(col_main_text)){
    col_main_text <- col_main
  }

  points_col <- rep(col_main,
                    n)

  if(type == "eti"){
    interval_low <- ints$eti_lower
    interval_high <- ints$eti_upper
  } else {          # type == "moving_window"
    interval_low <- sort(vals)[moving_window_start]  # first point in interval
    points_in_interval <- credibility * n
    interval_high <- sort(vals)[moving_window_start + points_in_interval - 1] # last
                                        # point in interval

    # Prob won't actually need the HDI for this plot, just show moving windows
    #  as example.
    # interval_low <- ints$hdi_lower
    # interval_high <- ints$hdi_upper
  }

  red_points <- c(which(vals < interval_low),
                  which(vals > interval_high))
  points_col[red_points] <- col_tail

  plot(vals,
       y = jitter(rep(1, n),
                  amount = jitter_amount),
       ylim = y_lim,
       ylab = "",
       yaxt = 'n',
       col = points_col,
       pch = 20,
       ...)

  # Copied and likely adapted from plot.intervals_density()
  if(interval_arrows){
    # main interval (usually 90% or 95%)
    shape::Arrows(interval_low + arrowhead_gap,
                  y_arrow,
                  interval_high - arrowhead_gap,
                  y_arrow,
                  code = 3,
                  col = col_main,
                  arr.type = "triangle",
                  arr.adj = 1,
                  arr.length = arrowhead_length)
    text(mean(c(interval_low, interval_high)),
         y_arrow,
         paste0(credibility * 100, "%"),
         col = col_main_text,
         pos = 3)
    # Left-hand tail
    shape::Arrows(0,
                  y_arrow,
                  interval_low - arrowhead_gap,
                  y_arrow,
                  code = 3,
                  col = col_tail,
                  arr.type="triangle",
                  arr.adj = 1,
                  arr.length = arrowhead_length)
    # Right-hand tail
    shape::Arrows(interval_high + arrowhead_gap,
                  y_arrow,
                  par("usr")[2],
                  y_arrow,
                  code = 1,
                  col = col_tail,
                  arr.type="triangle",
                  arr.adj = 1,
                  arr.length = arrowhead_length)
    # Annotate if ETI
    if(type == "eti"){
      text(interval_low/2,
           y_arrow,
           paste0(eti_lower_percentile, "%"),
           col = col_tail,
           pos = 3)
      text(mean(c(interval_high, par("usr")[2])),
           y_arrow,
           paste0(eti_lower_percentile, "%"),
           col = col_tail,
           pos = 3)
    }
  }

  if(type == "eti"){
    mtext(paste0("Equal-tailed interval based on ", n, " samples"),
          side = 3, adj = 0, cex = 1.5,
          line = 0.3)
  }

  if(type == "moving_window"){
    mtext(paste0("Moving window interval with ",
                 moving_window_start-1,
                 ifelse(moving_window_start == 2,
                        " sample on the left,",
                        " samples on the left,"),
                 round((1 - credibility) * n - moving_window_start +1),
                 " on right"),
          side = 3, adj = 0, cex = 1.5,
          line = 0.3)
  }


}
