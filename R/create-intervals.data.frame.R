##' @rdname create_intervals
create_intervals.data.frame <- function(dat_mcmc,
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
