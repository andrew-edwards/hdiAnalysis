##' Shorthand for `as.data.frame()` for printing all of a tibble.
##'
##' You often want to print all of a (small) tibble, but `as.data.frame()`
##'   is cumbersome, and so this is a useful shortcut.
##'
##' @param tib `tibble` to be printed to console, R Markdown file, etc.
##'
##' @return print `a` as a data.frame, so show all the rows.
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # For a tibble 'tib', to see all the rows and columns:
##' tib %>% a()
##' }
##' @export
a <- function(tib){
  as.data.frame(tib)
}
