##' @rdname summary_table
##' @export
summary_table.intervals_density_list <- function(int_dens_list,
                                                 dig = 2){
  stopifnot("intervals_density_list" %in% class(int_dens_list))

  int_all <- int_dens_list$intervals_all

  dplyr::mutate(int_all,
                "Median" = median,
                "ETI lower" = eti_lower,
                "ETI upper" = eti_upper,
                "HDI lower" = hdi_lower,
                "HDI upper" = hdi_upper,
                "Interval difference" = width_diff,
                "Range a lower" = a_lower,          # TODO think if left-skewed
                "Range a upper" = eti_lower,
                "Range b lower" = b_lower,
                "Range b upper" = eti_upper) %>%
    dplyr::select(quantity,
                  "Median":"Range b upper") %>%
    knitr::kable(digits = 2)
}
