##' Plot density function (smoothed) of MCMC recruitment for a given year, with tails shaded as specified
##'
##' @param dat_mcmc a numeric vector representing an MCMC sample.
##' @param rec_intervals result of `create_intervals(dat_mcmc)`; is calculated
##'   if not supplied. May be worth supplying so it's not being repeatedly calculated.
##' @param year Which year of recruitment (age-0) to plot
##' @param type type of intervals: either `hdi` or `equal`
##' @return invisible
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' plot_recruitment_density()
##' plot_recruitment_density(year = 2021)
##' }
plot_density_fig_sup <- function(dat_mcmc = one_year_mcmc,
                         dens_intervals = NULL,
                         year = 2021,
                         type = "hdi",
                         x_lim = c(0, 50),  # default for 2010
                         col_main = NA,
                         col_tail_low = rgb(86, 180, 233, maxColorValue = 255, alpha = 100), # Sky blue from color blind pallet (Okabe-Ito)
                         col_tail_high = rgb(86, 180, 233, maxColorValue = 255, alpha = 100), # Sky blue from color blind pallet (Okabe-Ito)
                         col_tail_line = rgb(86, 180, 233, maxColorValue = 255), # Sky blue from color blind pallet (Okabe-Ito)
                         col_95 = rgb(240, 228, 66, maxColorValue = 255, alpha = 100), # Yellow from color blind pallet (Okabe-Ito)
                         col_included = rgb(204, 121, 167, maxColorValue = 255, alpha=100), # Reddish purple from color blind pallet (Okabe-Ito) 
                         col_excluded = rgb(230, 159, 0, maxColorValue = 255, alpha =100), # Orange from color blind pallet (Okabe-Ito)
                         main_title = NULL,
                         x_lab = NULL){
  if(!(type %in% c("equal", "hdi"))){
    stop("type needs to equal or hdi.")}

  if(is.null(dens_intervals)){
    dens_intervals <- calc_density(dat_mcmc, n=1e6)
  }

  dens <- dens_intervals$density

  # Reorder Just use for title, maybe also low and high, actually prob not
  if(type == "equal"){
      if(is.null(main_title)) {
      main_title <- "Equal-tailed 95% interval"
    }
  } else { # type == "hdi"
    if(is.null(main_title)) {
      main_title <- "HDI 95% interval"
    }
  }

  if(is.null(x_lab)){
    x_lab <- paste0("Recruitment in ", year, " (billions of fish)")
  }

  # TODO change to x_interval_low etc.
  # low and high values of the interval for plotting, already calculated
  if(type == "equal"){
     interval_low <- dens_intervals$intervals$`2.5`
     interval_high <- dens_intervals$intervals$`97.5`
     y_interval_low <- dens_intervals$intervals$y_low_equal_interp
     y_interval_high <- dens_intervals$intervals$y_high_equal_interp
  } else { # type == "hdi"
     interval_low <- dens_intervals$intervals$hdi_lower
     interval_high <- dens_intervals$intervals$hdi_upper
     y_interval_low <- dens_intervals$intervals$y_low_hdi_interp
     y_interval_high <- dens_intervals$intervals$y_high_hdi_interp
  }

  plot(dens,
       xlab = x_lab,
       lwd = 1.2,
       xlim = x_lim,
       main = main_title,
       las=1)

  #Lower tail barkground
  # polygon(c(x_lim[1] - 10, x_lim[1] - 10, interval_low, interval_low),
  #         c(0, 10, 10, 0),
  #         col = col_tail_low,
  #         border = NA,
  #         main = "")
  
  # High tail background
  # polygon(c(interval_high, interval_high, x_lim[2] + 10, x_lim[2] +10),
  #         c(0, 10, 10, 0),
  #         col = col_tail_high,
  #         border = NA,
  #         main = "")
  # 
  # 
  # # Interval_Low tail
  # polygon(c(x_lim[1] - 10, x_lim[1] - 10, interval_low, interval_low),
  #         c(0, 10, 10, 0),
  #         col = col_tail_low,
  #         border = NA,
  #         main = "")
  
  # Area in 95% Credible interval
  # polygon(c(interval_low, interval_low, interval_high, interval_high),
  #         c(0, 10, 10, 0),
  #         col = col_95,
  #         border = NA,
  #         main = "")
  
  
    # STILL need to think and CHECK EVERYTHING AGAIN
  
  
  # Make an if once figured out:
  #abline(h = y_interval_low)
  
  
  
    
  
  # # Full distribution
  # polygon(dens,
  #         col = col_main,
  #         main = "")
  
  

  # Interval_Low tail
  polygon(c(dens$x[dens$x <= interval_low], interval_low, interval_low),
          c(dens$y[dens$x <= interval_low], y_interval_low, 0),
          col = col_tail_low,
          border = NA,
          main = "")

  # Upper tail
  polygon(c(interval_high, dens$x[dens$x >= interval_high], interval_high),
          c(y_interval_high, dens$y[dens$x >= interval_high], 0),
          col = col_tail_high,
          border = NA,
          main = "")

  

  # Area of included values but as probable as lower tail
  i_left_side <- max(which(dens$y > y_interval_low))
  
  polygon(c(dens$x[i_left_side+1], dens$x[dens$x > dens$x[i_left_side] & dens$x <= interval_high], interval_high, interval_high, dens$x[i_left_side+1]),
          c(dens$y[i_left_side +1], dens$y[dens$x > dens$x[i_left_side] & dens$x <= interval_high], y_interval_high, 0, 0),
          col = col_included,
          border = NA,
          main = "")
  
  # Area of excluded values but more probable than upper tail
  i_right_side <- max(which(dens$y < y_interval_high & dens$x <= interval_high))
  
  polygon(c(dens$x[i_right_side], dens$x[dens$x > dens$x[i_right_side] & dens$x <= interval_low], interval_low, interval_low, dens$x[i_right_side]),
          c(dens$y[i_right_side], dens$y[dens$x > dens$x[i_right_side] & dens$x <= interval_low], y_interval_low, 0, 0),
          col = col_excluded,
          border = NA,
          main = "")
  # abline(h = y_interval_high, lty=3)
  # abline(h = y_interval_low, lty=3)
  # abline(v = interval_high, lty=2, col="blue")
  # abline(v = interval_low, lty=2, col="blue")
  
  # To make it so the line is not partly overshadowed 
  lines(dens,
       xlab = x_lab,
       lwd = 1.2)
  
  # Upper tail line
  lines(c(interval_high, dens$x[dens$x >= interval_high]),
        c(y_interval_high, dens$y[dens$x >= interval_high]),
        xlab = x_lab,
        col = col_tail_line,
        lwd = 1.4)
  
  lines(c(dens$x[dens$x <= interval_low], interval_low),
          c(dens$y[dens$x <= interval_low], y_interval_low),
          col = col_tail_line,
        lwd = 1.4)
  
  box()
  
  
  
  
  #rug(dens$x, side=3)
  rug(dat_mcmc, ticksize = 0.025, lwd=0.2)
  
  
  # legend(x = "topright", legend =c("1  lower tail", "2  upper tail", "3  included (but as probable as lower tail)"), 
  #        col = c(col_tail_low, col_tail_high, col_included), pch = 19, bty = "n", pt.cex = 2.2, x.intersp = -0.32)
  legend(x = "topright", legend =c("Tails", "95% interval", "Included (but as probable as lower tail)", "Excluded (but more probable that upper tail)"), 
         col = c(col_tail_low, col_95, col_included, col_excluded), pch = 19, bty = "n", pt.cex = 2.2)
  
  

  # Prob remove this once fixed, just easier to debug with values in
#  return(list(dens = dens,
#              low = low,
#               high = high,
#               i_low = i_low,
#               y_low = y_low,
#               i_high = i_high,
#               y_high = y_high))
}
