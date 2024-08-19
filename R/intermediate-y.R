##' Calculate an intermediate y value between two (x,y) points.
##'
##' Given x1 and x2 and corresponding y1 and y2, calculate the intermediate
##'  value at `x_val` where `x1 <= x_val <= x2`, by a simple linear interpolation.
##'
##' @param x_val numeric value for which to calculate the corresponding y value
##' @param x1, x2, y1, y2 numeric value representing co-ordinates (x1, y1) and
##'   (x2, y2) between which we want to calculate the interpolated y value at
##'   `x_val`. Require `x1 < x2` and `x1 <= x_val <= x2`.
##' @return the intermediate y value
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' intermediate_y(5, 4, 6, 10, 17)
##' }
intermediate_y <- function(x_val,
                           x1,
                           x2,
                           y1,
                           y2){
  stopifnot(x1 < x2 & x1 <= x_val & x_val <= x2)
  # y1 + gradient * how far along
  y1 + (y2 - y1) / (x2 - x1) * (x_val - x1)
}
