##' Plots densities and ETIs or HDIs for multiplie quantities (e.g. years).
##'
##' Plots the density and ETI/HDI for each quantity (such as year) in a
##'  `intervals_density_list` object that is returned from [create_intervals()]
##'  applied to a data frame. Called from [plot.intervals_density_list()],
##'  probably not needed by users.
##'
##' @inheritParams plot.intervals_density_list
##' @param ... extra arguments passed onto [plot.intervals_density()]
##' @return plots multiple panels of density and ETI or HDI
##' @export
##' @author Andrew Edwards
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

    # This calls plot.intervals_density()
    plot(obj$res_all[[this_quantity_i]],
         # main_title = as.character(this_quantity),
         # main_title_include = TRUE,
         type = type,
         ...)
    mtext(this_quantity,
          side = 3,
          adj = 0,
          cex = 0.7,
          line = 0.3)
  }
  par(mfrow = mfrow_orig)
}
