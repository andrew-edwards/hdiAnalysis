##' Add minor unlabelled tickmarks to an existing plot
##'
##' Add sensible smaller (unlabelled) tickmarks to both axes of an existing
##' plot. Adapted from `pacea::add_tickmarks()`. Is exported but unlikely to be needed externally.
##'
##' @param x_tick_start start of small tickmarks on x-axis
##' @param x_tick_by interval between small tickmarks on x-axis
##' @param x_tick_end end of small tickmarks on x-axis
##' @param y_tick_start start of small tickmarks on y-axis
##' @param y_tick_by interval between small tickmarks on y-axis
##' @param y_tick_end end of small tickmarks on y-axis
##' @return adds minor tickmarks to an existing plot
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' }
add_minor_tickmarks <- function(x_tick_start = 0,
                                x_tick_by = 2,
                                x_tick_end = NULL,
                                y_tick_start = 0,
                                y_tick_by = 0.01,
                                y_tick_end = NULL){
  if(is.null(x_tick_end)){
    x_tick_end  <- ceiling(par("usr")[2])
  }

  if(is.null(y_tick_end)){
    y_tick_end  <- ceiling(par("usr")[4])
  }

  # Small ticks every x_tick_by
  axis(1,
       seq(x_tick_start,
           x_tick_end,
           by = x_tick_by),
       labels = FALSE,
       tcl = -0.2)

  # Small ticks every y_tick_by
  axis(2,
       seq(y_tick_start,
           y_tick_end,
           by = y_tick_by),
       labels = FALSE,
       tcl = -0.2)
}
