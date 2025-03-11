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
##' TODO move into talk
##' set.seed(42)
##' vec_sub <- sample(vec, 200)    # then can easily see the 5 at each end
##' res_sub <- create_intervals(vec_sub)
##' plot_pointwise_intervals(vec_sub, res_sub, jitter_amount = 0.0001,
##'   xlim = c(0, 42), xlab = "Recruitment (billions of fish)")
##'
##' # full amount
##' plot_pointwise_intervals(vec, res_vec, jitter_amount = 0.01, xlim = c(0, 42), xlab = "Recruitment (billions of fish)")
##' }
plot_pointwise_intervals <- function(values,
                                     ints_dens,
                                     type = "eti",   # type of plot to do
                                     # subsample = 1000,  # doesn't give exact
                                        # results since ETI is based on full
                                        # sample, so don't make too small, so do
                                     # outside of this
                                     col_main = "blue",
                                     col_main_text = NULL,
                                     col_tail = "red",
                                     jitter_amount = 0.005,
                                     interval_arrows = TRUE,
                                     arrowhead_gap = 0,
                                     arrowhead_length = 0.2,
                                     y_lim = c(0.95, 1.05),
                                     y_arrow = 1.02,
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
  } else { # type == "hdi"
    interval_low <- ints$hdi_lower
    interval_high <- ints$hdi_upper
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

  mtext(paste0("Equal-tailed interval based on ", n, " samples"),
        side = 3, adj = 0, cex = 1.5,
        line = 0.3)
}
