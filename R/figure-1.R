##' Plot Figure 1 as a pdf file with specific dimensions for the manuscript.
##'
##' Useful for the manuscript, maybe less so for other users, though could be
##'   useful as a template. Generally running in
##'   the vignettes folder then copying over to the write up repository. Was
##'   also useful for running the tests.
##' @param file filename to save pdf file
##' @param height height of pdf file (inches)
##' @param width width of pdf file (inches)
##' @param xlim, xlab x-axis range and label, as passed onto [plot.numeric()]
##' @param ... further arguments passed onto [plot.numeric()]
##' @return nothing, but saves the pdf
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' figure_1()
##' }
figure_1 <- function(file = "figure-1.pdf",
                     height = 4,
                     width = 114/25.4,
                     xlim = c(0, 40),
                     xlab = "Recruitment (billions of fish)",
                     ...){

  res_2021 <- create_intervals(rec_2021)

  pdf(file = file,
      height = height,
      width = width,
      paper="special")

  par(mai = c(0.32, 0.3, 0.15, 0.12),
      mfrow = c(2,1),
      mgp = c(1.6,0.5,0),
      cex = 0.6)

   plot(res_2021,
       type = "eti",
       xlim = xlim,
       ylim = c(0, 0.11),
       interval_arrows = TRUE,
       xlab = xlab,
       arrowhead_gap = 0.1,
       x_minor_ticks_by = 2,
       ...)

  mtext("A. Equal-tailed interval", side = 3, adj = 0, cex = 0.7,
        line = 0.3)

  plot(res_2021,
       type = "hdi",
       xlim = xlim,
       ylim = c(0, 0.11),
       interval_arrows = TRUE,
       xlab = xlab,
       arrowhead_gap = 0.1,
       x_minor_ticks_by = 2,
       ...)

  mtext("B. Highest density interval", side = 3, adj = 0, cex = 0.7,
        line = 0.3)

  dev.off()

  invisible()
}
