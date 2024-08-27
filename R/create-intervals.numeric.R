##' @rdname create_intervals
##' @export
create_intervals.numeric <- function(dat,
                                     density = FALSE,
                                     credibility = 0.95,
                                     n = 1e05,
                                     allow_hdi_zero = FALSE,
                                     tol = 1e-05,
                                     ...
                                     ){

  stopifnot(credibility > 0 & credibility < 1)
  stopifnot((min(dat) < 0 & !allow_hdi_zero) |
            min(dat) >= 0)

  # If all data are >= 0 and `from` is undefined then set from = 0, assume
  #  true distribution should be non-negative.
  if(min(dat) >= 0 & !hasArg(from)){
    from = 0
  }

  if(exists("from")){
    dens <- density(dat,
                    from = from,
                    n = n,
                    ...)       # Gives values with equal spacing, so high
                               #  resolution in the tail which has sparse
                               #  data. Gets changed in next section if needed
  } else {
    dens <- density(dat,
                    n = n,
                    ...)       # Use the default dens calculation
  }
# browser()
  dens$y <- dens$y / integrate_simpsons(dens,
                                        tol = tol)  # normalise to ensure
                                       # integrates to 1. TODO:
  # think more if this is okay to do.
  # OR may have to just extend the range of density, by making 'to' bigger than
  # the default. Or just increase cut quite a bit (will just take slightly
  # longer to run); do a browser() to check the integral above. Was 1.000003
  # ish, so not the problem in this case. i.e. it's not missing some off the
  # ends. But try cut idea.

  if(density){
    # These get overwritten if calculations are redone in next if()
    hdi_res_list <- with_warnings(HDInterval::hdi(dens,
                                                  credMass = credibility))
    hdi_height <- attr(hdi_res_list$value,
                       "height")

    if(hdi_res_list$value["lower"] == 0 & !allow_hdi_zero){
      # Redo dens and HDI to force lower bound to be >0, will be min(dat).
      dens <- density(dat,
                      from = min(dat),
                      n = n,
                      ...)

      dens$y <- dens$y / integrate_simpsons(dens,
                                            tol = tol)  # normalise as for
                                        # above. TODO write up in methods, and
                                        # the rest.
      hdi_res_list <- with_warnings(HDInterval::hdi(dens,
                                                    credMass = credibility))
      hdi_height <- attr(hdi_res_list$value,
                         "height")

    }
  } else {           # !density
    hdi_res_list <- with_warnings(HDInterval::hdi(dat,
                                                  credMass = credibility))
    hdi_height <- NA
  }

  hdi_res <- hdi_res_list$value

  if(length(hdi_res_list$warnings) == 0){
    hdi_res_warning <- FALSE
  } else {
    hdi_res_warning <- TRUE

    if(hdi_res_list$warnings !=
       "The HDI is discontinuous but allowSplit = FALSE;\n    the result is a valid CrI but not HDI."){
      warning("New type of warning in create_intervals().")
    }
  }

  eti_lower_quantile <- (1 - credibility)/2

  intervals <- tibble::tibble("median" = median(dat),
                              "eti_lower" = as.numeric(quantile(dat,
                                                                probs = eti_lower_quantile)),
                              "eti_upper" = as.numeric(quantile(dat,
                                                                probs = 1 - eti_lower_quantile)),
                              "hdi_lower" = as.numeric(hdi_res["lower"]),
                              "hdi_upper" = as.numeric(hdi_res["upper"])) %>%
    dplyr::mutate(width_eti = eti_upper - eti_lower,
                  width_hdi = hdi_upper - hdi_lower,
                  width_diff = width_eti - width_hdi)

  # old thoughts, might need some, some was figured out:
  # Calculate the pdf value at low and high x values, have to interpolate.
  # Don't need to for HDI, since values will be exact x values, which I hadn't
  # realised when first doing this. (Need to check about 2.5 and 97.5).
  # And since intervals are considered [ , ) and HDI intervals are actually
  # bigger than the 'true' HDI interval, my original interpolation was actually,
  # I think, just selecting the i_high+1 value by mistake. See .Rmd for
  # thoughts.
  # Actually no, dens is a smoothed version. Think I just need to go the other
  # way for the high value??

  # Need a value of y corresponding to the low and high ends of equal and HDI
  # intervals, so doing an interpolation.

  # ETI
  i_eti_lower <- findInterval(intervals$eti_lower,
                              dens$x)  # low is between dens$x[i_eti_lower] and
                                       # dens$x[i_eti_lower + 1]

  y_eti_lower <- dens$y[i_eti_lower] +
    (dens$y[i_eti_lower + 1] - dens$y[i_eti_lower])/
      (dens$x[i_eti_lower + 1] - dens$x[i_eti_lower]) * # slope
    (intervals$eti_lower - dens$x[i_eti_lower])

  i_eti_upper <- findInterval(intervals$eti_upper,
                               dens$x)  # eti_upper is between x[i_eti_upper]
                                        # and x[i_eti_upper + 1]
  y_eti_upper <- dens$y[i_eti_upper] +
    (dens$y[i_eti_upper + 1] - dens$y[i_eti_upper])/
      (dens$x[i_eti_upper + 1] - dens$x[i_eti_upper]) * # slope
    (intervals$eti_upper - dens$x[i_eti_upper])

  # HDI. Want to interpolate the y's to get the height for drawing. If density not used then
  # same approach as for ETI. But if density is used then slightly different
  # because the HDI is slightly narrower than the true HDI, as per
  # HDInterval::hdi.density()
  if(!density){
    i_hdi_lower <- findInterval(intervals$hdi_lower,
                                dens$x)  # low is between dens$x[i_hdi_lower]
                                         # and dens$x[i_hdi_lower + 1]
    y_hdi_lower <- dens$y[i_hdi_lower] +
      (dens$y[i_hdi_lower + 1] - dens$y[i_hdi_lower])/
        (dens$x[i_hdi_lower + 1] - dens$x[i_hdi_lower]) * # slope
      (intervals$hdi_lower - dens$x[i_hdi_lower])

    i_hdi_upper <- findInterval(intervals$hdi_upper,
                                dens$x)  # hdi_upper is between x[i_hdi_upper]
                                         # and x[i_hdi_upper + 1]
    y_hdi_upper <- dens$y[i_hdi_upper] +
      (dens$y[i_hdi_upper + 1] - dens$y[i_hdi_upper])/
        (dens$x[i_hdi_upper + 1] - dens$x[i_hdi_upper]) *  # slope
      (intervals$hdi_upper - dens$x[i_hdi_upper])
  } else {    # density was used in HDI calculation, so the HDI endpoints are
              # exact values of dens$x. But lower is the first value above the
              # true HDI lower value (since it's the first index for which
              # y> = 0.95), and higher is the last
              # value below the true HDI high value (last index for which y >=
              # 0.95). So we could refine these slightly by interpolating between
              # the values of x. And it's not exactly the same code as above,
              # because of the asymmetry. But, probably simpler to just rerun
              # with a higher n, so do that. See notes for sketch of idea.

    i_hdi_lower <- which(dens$x == intervals$hdi_lower)   # should give a value,
                                                          # TODO need to test,
                                                          # and for upper
    # Keep the above one as default as already used for many calculations in
    # manuscript (though the next line should give same answer anyway), but then
    # home in if not perfectly  accurate. Had to add this for hake age1 calculation.
    if(length(i_hdi_lower) == 0){
      i_hdi_lower <- which.min(abs(dens$x - intervals$hdi_lower))
    }


    y_hdi_lower <- dens$y[i_hdi_lower]

    i_hdi_upper <- which(dens$x == intervals$hdi_upper)

    # Same as for i_hdi_lower
    if(length(i_hdi_upper) == 0){
      i_hdi_upper <- which.min(abs(dens$x - intervals$hdi_upper))
    }

    y_hdi_upper <- dens$y[i_hdi_upper]
  }

  # TODO think about left-skewed
  #  i_for_interval_a_low <- max(which(dens$y < int$eti_upper & dens$x <= int$eti_lower))
  i_for_interval_a_low <- min(which(dens$y > y_eti_upper &
                                    dens$x < intervals$eti_lower))   # TODO think about if
                                        # empty, test on symmetric
  a_lower <- dens$x[i_for_interval_a_low]
         # int$eti_lower)    # TODO adapt based on above thought

  i_for_interval_b_low <- min(which(dens$y < y_eti_lower &
                                    dens$x > intervals$eti_lower))   # TODO think about if
                                        # empty, test on symmetric
  b_lower <- dens$x[i_for_interval_b_low]
                  # int$eti_upper)    # TODO adapt based on above thought

  integral_full <- integrate_simpsons(dens,
                                      tol = tol)   # Should be 1 as we
                                        # normalised to ensure.


  integral_eti <- integrate_simpsons(dens,
                                     domain = c(intervals$eti_lower,
                                                intervals$eti_upper),
                                     tol = tol)

  integral_hdi <- integrate_simpsons(dens,
                                     domain = c(intervals$hdi_lower,
                                                intervals$hdi_upper),
                                     tol = tol)
  intervals <- dplyr::mutate(intervals,
                             a_lower = a_lower,
                             b_lower = b_lower,
                             i_eti_lower = i_eti_lower,
                             y_eti_lower = y_eti_lower,
                             i_eti_upper = i_eti_upper,
                             y_eti_upper = y_eti_upper,
                             i_hdi_lower = i_hdi_lower,
                             y_hdi_lower = y_hdi_lower,
                             i_hdi_upper = i_hdi_upper,
                             y_hdi_upper = y_hdi_upper,
                             hdi_height = hdi_height,
                             integral_full = integral_full,
                             integral_eti = integral_eti,
                             integral_hdi = integral_hdi,
                             warning = hdi_res_warning,
                             allow_hdi_zero = allow_hdi_zero)

  res <- list(intervals = intervals,
              density = dens,
              credibility = credibility)

  class(res) = c("intervals_density",
                 class(res))

  return(res)
}
