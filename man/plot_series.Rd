% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-series.R
\name{plot_series}
\alias{plot_series}
\title{Plot a time series of intervals from multiple samples calculated from
\code{create_intervals()} applied to a data frame object.}
\usage{
plot_series(
  obj,
  xlab = "Year",
  ylab = "Recruitment (billions of fish)",
  inc = 0.15,
  x_tick_extra_years = 20,
  start_decade_ticks = (min(obj$year)\%/\%10) * 10,
  eti_bar_col = "blue",
  hdi_bar_col = "red",
  y_max = 35,
  add_line_at_0.4 = FALSE,
  add_line_at_0.4_col = "darkgreen",
  add_line_at_0.4_lty = 5,
  y_tick_start = 0,
  y_tick_end = NULL,
  y_tick_by = 1,
  cex_val = 0.8,
  inset = c(0, 0),
  add_legend = TRUE,
  leg_loc = "topright",
  join_intervals = FALSE,
  arrowhead_length = 0.15,
  ...
)
}
\arguments{
\item{obj}{a \code{pacea_recruitment} object, which is a time series. Function
will run on other objects (not give an error) but is not tested on those.}

\item{y_max}{maximum y value for certain types of plot (use this if you get
an error when specifying \code{ylim})}

\item{...}{further options passed onto \code{plot.default()}}

\item{value}{the column to plot if no uncertainties, or what to plot as dots
if showing uncertainties (likely always \code{median})}

\item{style}{\code{no_uncertainty} for plain time series without uncertainty,
gets overridden to have uncertainty bars if \code{low} and \code{high} are columns
of \code{obj}}

\item{uncertainty_bar_col}{colour for uncertainty bars for certain types of
plot (e.g. estimated fish recruitment)}

\item{add_line_at_1}{whether to add a horizontal line at 1 (only sensible for scaled recruitments)}

\item{add_line_at_1_col}{colour for line at 1}

\item{add_line_at_1_lty}{line type of line at 1}
}
\value{
plot of the time series as median with bars showing uncertainty (if
\code{low} and \code{high} are columns of `obj) to the current device; returns nothing.
}
\description{
TODO can maybe delete this I think. Expect it got used for default plotting
functions. Is is hdi-analysis.Rmd, so think about if that needs rerunning
again; may just want to keep the pdf for historical reasons, as other
vignettes now show new calculations..
}
\details{
Will show ETIs, HDIs, or both.

TODO adapting from \code{plot_series} which I can delete once finalised.

Adapted from \code{pacea::plot.pacea_recruitment()}.

TODO Temporal plot for a time series (of class
\code{pacea_recruitment}) object. The \code{style} option here (unlike for
\code{plot.pacea_index()} defaults to \code{no_uncertainty} and gets changed to
\code{uncertainty} if \code{low} and \code{high} are columns of \code{obj}.
}
\examples{
\dontrun{

recruitment_res <- calc_density_many_years(dplyr::select(hake_recruitment_mcmc,
                                                         -"Virgin"))     # TODO
plot_series(recruitment_res$dens_intervals)
}
}
\author{
Andrew Edwards
}
