##' Print a Markdown summary table of main results from [create_intervals()]
##'
##' Works on [create_intervals()] output applied to either a vector or a data
##' frame, creating an appropriate table that renders in an R Markdown
##' document. See the vignettes.
##' @param obj result from [create_intervals()], either with class
##' `intervals_density` if [create_intervals()] was applied to a vector, or of
##' class `intervals_density_list` if [create_intervals()] was applied to a data
##' frame; the respective function [summary_table.intervals_density()] or
##' [summary_table.intervals_density_list()] gets automatically used.
##' @param dig number of decimal places to show
##' @return Markdown code (for automatic use straight in an R Markdown document, for instance)
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # And see vignettes
##' res <- create_intervals(rec_2021)
##' summary_table(res)
##'
##' res_df <- create_intervals(hake_recruitment_mcmc)
##' summary_table(res_df)
##' }
summary_table <- function(obj, ...){
  UseMethod("summary_table")
}
