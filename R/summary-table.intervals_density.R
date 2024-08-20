##' @rdname summary_table
##' @export
summary_table.intervals_density <- function(int_dens,
                                            dig = 2){
  stopifnot("intervals_density" %in% class(int_dens))

  int <- int_dens$intervals

  cat("| Quantity| Value | Interval width | \n",
      "| :-------| -----:| -----:| \n",
      "| Median  |", f(int$median, dig) ,"| -- | \n",
      "| ETI     |", f(int$eti_lower, dig), "--",
        f(int$eti_upper, dig), "|", f(int$width_eti,
                                                     dig), "| \n",
      "| HDI     |", f(int$hdi_lower, dig), "--",
        f(int$hdi_upper, dig), "|", f(int$width_hdi,
                                                     dig),  "| \n",
      "| Range a |", f(int$a_lower, dig), "--", f(int$eti_lower, dig), "|",
        f(int$eti_lower - int$a_lower, dig), "| \n",
      "| Range b |", f(int$b_lower, dig), "--", f(int$eti_upper, dig), "|",
        f(int$eti_upper - int$b_lower, dig), "| \n"
      )
}
