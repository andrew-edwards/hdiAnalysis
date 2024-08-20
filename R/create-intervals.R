##' Calculate equal-tailed intervals and highest density intervals and related
##' quantities.
##'
##' For a numeric vector (e.g. MCMC samples of an estimated population size) or
##' a data frame (such as MCMC samples of an estimated population size in 10
##' different years -- years would be the named columns, each row would be an
##' MCMC sample). Calculate the ETI, HDI, their respective widths, kernel
##' density, and other useful information. See our manuscript and Appendix for
##' further background.
##'
##' @param dat numeric vector of values (such as MCMC samples for a quantity),
##'   which automatically uses [create-intervals.numeric()], or a data frame
##'   (with, say, MCMC samples as rows and a quantity given in each named
##'   column), which automatically uses [create-intervals.data.frame()] and
##'   the column names, and removes `NA`s.
##' @param density if TRUE then use the density approach for the HDI
##'   calculation, rather than the `hdi()` default of just the sample values. If
##'   FALSE (the default) then the density kernel is only used to estimate the y
##'   values of the probability density function at specified points.
##' @param credibility numeric value between 0 and 1 specifying the interval to
##'   be specified (0.95 for 95%, 0.90 for 90%, etc.)
##' @param from the left-most point of the grid at which the density is to be
##'   estimated; if NULL then the default in `density()` will be used, which is
##'   'cut * bw' outside of 'min(x)' (see `?density`), and can fall below 0.
##' @param n the number of equally spaced points at which the density is
##'   to be estimated, to be passed onto `density()`. We found the `density()`
##'   default of 512 to give inaccurate results, so set a higher default here as
##'   1e05 (`?density` advises to use powers of 2 as the value gets rounded up
##'   anyway, but we found this not to be the case). Changing `n` changes the
##'   resolution of the density kernel but not the wiggliness.
##' @param allow_hdi_zero logical, if TRUE then allow HDI lower bound to include
##'   zero or be negative; if FALSE (the default) then do not allow this.
##' @param ... arguments to pass onto `density()`, including `to` which is
##'   the right-most equivalent to `from`.
##' @md
##' @return
##' * If `dat` is numeric then retuns a list object of class
##'   `intervals_density` (such that we can plot it
##'   with `plot.intervals_density()`, with objects:  TODO check if that md
##'   above is needed
##'   * intervals: one-row tibble with columns:
##'     * median: median of the data
##'     * eti_lower: lower end of the ETI
##'     * eti_upper: upper end of the ETI
##'     * hdi_lower: lower end of the HDI
##'     * hdi_upper: upper end of the HDI
##'     * width_eti: width of the ETI
##'     * width_hdi: width of the HDI
##'     * width_diff: difference in widths, how much smaller (more certain) the
##'   HDI is than the ETI
##'     * i_eti_lower: index for which `eti_lower` is between
##'   `dens$x[i_eti_lower]` and `dens$x[i_eti_lower + 1]`
##'     * y_eti_lower: linearly interpolated value based on `i_eti_lower`
##'   corresponding to the density at `eti_lower`
##'     * i_eti_upper, y_eti_upper: similar to `...lower` but for `upper`
##'     * i_hdi_lower: index for which `dens$x[i_hdi_lower] = hdi_lower`. The
##'   theoretical true value of the lower bound of HDI will lie between
##'   `dens$x[i_hdi_lower - 1]` and `dens$x[i_hdi_lower]`, but the high `n` used
##'   should make this range small enough
##'     * y_hdi_lower: the density at `dens$y[i_hdi_lower]` corresponding to `hdi_lower`
##'     * i_hdi_upper: index for which `dens$x[i_hdi_upper] = hdi_upper`. The
##'   theoretical true value of the upper bound of HDI will lie between
##'   `dens$x[i_hdi_upper]` and `dens$x[i_hdi_upper + 1]` (note the asymmetry to
##'   `i_hdi_lower`), but the high `n` used should make this range small enough
##'     * y_hdi_upper: the density at `dens$y[i_hdi_upper]` corresponding to `hdi_upper`
##'     * hdi_height: the height of the pdf returned from `HDInterval::hdi()`,
##'   corresponding to either `y_hdi_lower` or `y_hdi_upper` (depending on which
##'   is the first `dens$x` value to push the integrated sum of the sorted
##'   cumulative `dens$y` values over `credibility`; see
##'   `HDInterval::hdi.density()`. Is `NA` if `density = FALSE`.
##'     * warning: logical, if `TRUE` then a warning was produced during the
##'   `HDInterval::hdi()` calculation. If no warning printed then this warning
##'   was "The HDI is discontinuous but allowSplit = FALSE; the result is a
##'   valid CrI but not HDI.", else the new warning "New type of warning in
##'   create_intervals()." is printed and needs investigating. See
##'   `plot.intervals_density()` with `show_discontinuity = TRUE` to plot the
##'   discontinuities in the HDI.
##'     * allow_hdi_zero: logical of `allow_hdi_zero` used
##' * If `dat` is a data frame then return a list object of class
##'   `intervals_density_list` with:
##'  * element `[[i]]` corresponding to column `i` of the `dat_mcmc`. Each
##'   `[[i]]` element is itself a list of the form described above (since the
##'   intervals are calculated for each column in turn), plus also the
##'   `$name` element which is the name of column `i` of `dat_mcmc`.
##'  * intervals_all_years tibble of all the intervals, with the first column,
##'   `quantity`, corresponding to each column of `dat_mcmc`, such that row `i`
##'   corresponds to column `i` of `dat_mcmc`. `quantity` is numeric if no
##'   column names of `dat_mcmc` contain non-digits (e.g. represents years).
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' # See the vignettes for further details and refinements.
##' # Create intervals from the vector MCMC samples for hake recruitment in 2021:
##' res_vec <- create_intervals(rec_2021)
##' res_vec
##' plot(res_vec)    # Plot the default density plot showing the HDI
##'
##' # Create intervals from the data frame of MCMC samples for hake recruitment,
##' #  with each column representing a year:
##' res_df <- create_intervals(dplyr::select(hake_recruitment_mcmc, -"Virgin"))
##' res_df
##' plot(res_df)     # Plot the time series of calculated intervals
##' }
create_intervals <- function(dat,
                             ...){
  UseMethod("create_intervals")
}
