##' Calculate ETIs and HDIs for a data frame of quantities.
##'
##' For a data frame of MCMC (or other) samples as rows and some quantity
##' given in each named column, calculate the ETI, HDI, and kernel density for
##' each column, with `create_intervals()`.
##'
##' @param dat_mcmc data.frame of MCMC (or other) samples as rows, and a
##'   different quantity in each named column. `NA`s get removed.
##' @param ... arguments to pass to `create_intervals()` and maybe onto `density()`
##' @return list object, also of class `intervals_density_list` for ease of
##'   plotting, with:
##'  * element `[[i]]` corresponding to column `i` of the `dat_mcmc`. Each
##'   `[[i]]` element is itself a list, giving the same results as
##'   `create_intervals.numeric()` for each single vector, plus also with the
##'   `$name` element which is the name of column `i` of `dat_mcmc`.
##'  * intervals_all_years tibble of all the intervals, with the first column,
##'   `quantity`, corresponding to each column of `dat_mcmc`, such that row `i`
##'   corresponds to column `i` of `dat_mcmc`. `quantity` is numeric if no
##'   column names of `dat_mcmc` contain non-digits.
##' @export
##' @author Andrew Edwards
##' @examples
##' \dontrun{
##' res <- create_intervals(hake_recruitment_mcmc)
##' res
##' # See results.html vignette
##' }
create_intervals.data.frame <- function(dat_mcmc,
                                        credibility = 0.95,
                                        ...){
  res_all <- list()
  intervals_all <- tibble::tibble()

  for(i in 1:ncol(dat_mcmc)){
    name = names(dat_mcmc)[i]

    values <- dplyr::pull(dat_mcmc,
                          name)         # Full MCMC values for that
                                        # column, numeric vector

    values <- values[!is.na(values)]    # Remove NA's, useful for sample size analysis

    res <- create_intervals(values,
                            ...)

    res_all[[i]] <- res
    res_all[[i]]$name <- name

    intervals_all <- rbind(
      intervals_all,
      cbind("quantity" = name,
            res$intervals))
  }
  intervals_all <- tibble::as_tibble(intervals_all)

  # Look for any non-digit values in quantity. If there are not any then
  #  convert all values to numeric.
  if(!any(stringr::str_detect(intervals_all$quantity,
                              '[^.0-9]+'))){
    intervals_all$quantity <- as.numeric(intervals_all$quantity)
  }

  to_return <- list(res_all = res_all,
                    intervals_all = intervals_all)

  class(to_return) <- c("intervals_density_list",
                        class(to_return))
  return(to_return)
}
