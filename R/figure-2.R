##' Plot Figure 2 as a pdf file with specific dimensions for the manuscript.
##'
##' Useful for the manuscript, maybe less so for other users, though could be
##'   useful as a template. Generally running in
##'   the vignettes folder then copying over to the write up repository.
##' @param file filename to save pdf file
##' @param height height of pdf file (inches)
##' @param width width of pdf file (inches)
##' @return nothing, but saves the pdf
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' figure_2()
##' }
figure_2 <- function(file = "figure-2.pdf",
                     height = 4,
                     width = 114/25.4){

  # From figure_recruit_biomass which currently made figure 2
  pdf(file = file,
      height = height,
      width = width,
      paper="special")

  par(mai = c(0.32, 0.3, 0.15, 0.12),
      mfrow = c(2,1),
      mgp = c(1.6,0.5,0),
      cex = 0.6)

  res_all_years <- create_intervals(dplyr::select(hake_recruitment_mcmc,
                                                  -"Virgin"))

  plot(res_all_years,
       xlab = "Year",
       ylab = "Recruitment (billions of fish)")

  mtext("A",
        side = 3,
        adj = 0,
        cex = 0.7,
        line = 0.3)

  # Relative biomass time series
  res_all_years_2 <- create_intervals(hake_relative_biomass_mcmc)

  plot(res_all_years_2,
       xlim = c(2010, 2027),
       ylim = c(0, 2.6),
       add_line_at_0.4 = TRUE,
       inc = 0.05,
       leg_loc = "topleft",
       xlab = "Year",
       ylab = "Relative spawning biomass")

  mtext("B",
        side = 3,
        adj = 0,
        cex = 0.7,
        line = 0.3)

  dev.off()

  invisible()
}
