##' Plots densities and ETIs or HDIs for multiplie quantities (e.g. years).
##'
##' Plots the density and ETI/HDI for each quantitiey (such as year) in a
##'  `intervals_density_list` object that is returned from [create_intervals()]
##'  applied to a data frame. Called from [plot.intervals_density_list()],
##'  probably not needed by users.
##'
##' @param obj
##' @param type
##' @param quantity
##' @param mfrow
##' @param ...
##' @return
##' @export
##' @author Andrew Edwards
##' @examples
##' @ontrun{
##' @
##' @}
plot_densities <- function(obj,
                           type,
                           quantity = NULL,
                           mfrow = c(1, 1),
                           ...){

  # plot all quantities, esp for vignette since html
  if(is.null(quantity)){
    quantity <- obj$intervals_all$quantity
  }

  mfrow_orig <- par()$mfrow
  par(mfrow = mfrow)

  for(i in 1:length(quantity)){
    this_quantity <- quantity[i]

    this_quantity_i <- which(obj$intervals_all$quantity == this_quantity)
    #if(this_quantity != quantity[i]){
    #  stop("quantity input not consistent with obj$intervals_all$quantity; fix code")
    #}   # think not needed now I've done it properly

    # dat_mcmc_this_quant <- obj$res_all[[i]]

    #results_index <- which(years_in_results == years[i])

    # This calls plot.intervals_density()
    plot(res_all_years$res_all[[this_quantity_i]],
         # main_title = as.character(this_quantity),
         # main_title_include = TRUE,
         type = type,
         ...)
    mtext(this_quantity,
          side = 3,
          adj = 0,
          cex = 0.7,
          line = 0.3) # TODO Add to function, cex is indpt of par(c
  }
  par(mfrow = mfrow_orig)
}
