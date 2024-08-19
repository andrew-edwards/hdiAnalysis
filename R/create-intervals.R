##' Generic function to make `create_intervals()` use the right method depending on class of input
##'
##' Based on `plot()` and `plot.default()`. The functions are
##'  `create_intervals.numeric()` and `create_intervals.data.frame()` for,
##'  respectively, a vector of numeric values and a data frame of values. See
##'  those respective helpfiles.
##' @param dat if a numeric vector of values then [create_intervals.numeric()]
##'  gets called, whereas if a data frame then [create_intervals.data.frame()]
##'  gets used.
##' @param ... arguments to pass onto [create_intervals.numeric()] or [create_intervals.data.frame()].
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##'
##' }
create_intervals <- function(dat,
                             ...){
  UseMethod("create_intervals")
}
