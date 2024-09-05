#' Format a number. Taken from our hake R package.
#'
#' @details
#' The formatted number will have comma-separated thousands, millions, etc.
#' and the exact number of decimal points given by `digits`, even if the
#' last one is a zero.
#'
#' @param x The number
#' @param digits The number of decimal places to show
#' @author Chris Grandin
#' @return A Character string. A formatted version of the number `x`.
#' @export
#' @examples
#' \dontrun{
#' f(123456)
#' }
f <- function(x, digits = 0){
  format(round(x,
               digits),
         big.mark = ",",
         nsmall = digits)
}
