# Also some plotting that does not get covered by figure_1() and figure_2()
test_that("create_intervals results on hake 2021 recruitment and others do not change", {
  # Check full intervals results for default run (not checking density)
  intervals_names <- c("median",
                       "eti_lower",
                       "eti_upper",
                       "hdi_lower",
                       "hdi_upper",
                       "width_eti",
                       "width_hdi",
                       "width_diff",
                       "a_lower",
                       "b_lower",
                       "i_eti_lower",
                       "y_eti_lower",
                       "i_eti_upper",
                       "y_eti_upper",
                       "i_hdi_lower",
                       "y_hdi_lower",
                       "i_hdi_upper",
                       "y_hdi_upper",
                       "hdi_height",
                       "integral_full",
                       "integral_eti",
                       "integral_hdi",
                       "warning",
                       "allow_hdi_zero")

  res_2021_default_create <- create_intervals(rec_2021)

  res_2021_default_manual <- as.data.frame(
    t(c(1.018730e+01,
        4.085088e+00,
        2.949938e+01,
        2.802050e+00,
        2.518480e+01,
        2.541430e+01,
        2.238275e+01,
        3.031547e+00,
        2.081478e+00,
        1.562305e+01,
        3.245000e+03,
        3.280246e-02,
        2.342700e+04,
        3.579759e-03,
        2.226000e+03,
        1.002599e-02,
        2.000100e+04,
        7.297533e-03,
        NA,
        1.000000e+00,
        9.418215e-01,
        9.453085e-01,
        0,
        0))) %>%
    tibble::as_tibble()

  names(res_2021_default_manual) <- intervals_names
  res_2021_default_manual$"hdi_height" <- NA
  res_2021_default_manual$"warning" <- as.logical(res_2021_default_manual$"warning")
  res_2021_default_manual$"allow_hdi_zero" <- as.logical(res_2021_default_manual$"allow_hdi_zero")

  expect_equal(res_2021_default_create$intervals,
               res_2021_default_manual,
               tolerance = 1e-5)

  # Test some plotting options
  expect_error(plot(res_2021_default_create,
                    type = "demure"))
  expect_invisible(plot(res_2021_default_create,
                        type = "eti",
                        explanatory_lines_a_b = TRUE,
                        explanatory_lines_extra = TRUE,
                        rug_top = TRUE))

  expect_output(summary_table(res_2021_default_create))

  # Density = TRUE, just check the first values
  res_2021_density_true_create <- create_intervals(rec_2021,
                                                   density = TRUE)

  res_2021_density_true_manual <- c(10.187300,  4.085088, 29.499385,  2.529758,
                                    25.517941) # calculated 2024-06-05

  expect_equal(res_2021_density_true_create$intervals[1:5] %>% as.numeric(),
               res_2021_density_true_manual,
               tolerance = 1e-5)

  # Density = TRUE, from = 0.1
  res_2021_density_from_create <- create_intervals(rec_2021,
                                                   density = TRUE,
                                                   from = 0.1)

  res_2021_density_from_manual <- c(10.1873,
                                    4.085088,
                                    29.49938,
                                    2.529608,
                                    25.517150) # calculated 2024-08-27, slightly
                                        # different to above that gives from=0

  expect_equal(res_2021_density_from_create$intervals[1:5] %>% as.numeric(),
               res_2021_density_from_manual,
               tolerance = 1e-5)

  # Density = TRUE, allow_hdi_zero = FALSE
  res_1 <- create_intervals(dplyr::pull(hake_recruitment_mcmc, "1966"),
                            density = TRUE,
                            allow_hdi_zero = FALSE)

  res_1_manual <- c(1.63342,
                    0.05706769,
                    10.65658,
                    0.00613024,
                    8.69117952)   # calculated 2024-08-27
  expect_equal(res_1$intervals[1:5] %>% as.numeric(),
               res_1_manual,
               tolerance = 1e-5)
})

test_that("create_intervals.numeric() works with negative MCMC values", {
  res_2 <- create_intervals(rec_2021 - 10)    # Shift to have lots of negative values

  res_2_manual <- c(1.873000e-01,
                    -5.914912e+00,
                    1.949938e+01,
                    -7.197950e+00,
                    1.518480e+01)   # 2024-08-27
  expect_equal(res_2$intervals[1:5] %>% as.numeric(),
               res_2_manual,
               tolerance = 1e-5)
})


test_that("decreasing distribution works, for which should be left-hand value or 0 and so add to coverage", {
  exp_sim <- rexp(8000)
  expect_invisible(res_3 <- create_intervals(exp_sim))
})

# test_that("left-skewed distribution works", {
#  exp_sim_2 <- max(exp_sim) - exp_sim
#  res_4 <- create_intervals(exp_sim_2)
# })

test_that("create_intervals.data.frame() works as does plotting in various ways", {
  res_5 <- create_intervals(dplyr::select(hake_recruitment_mcmc,
                                          c('1966', '1967')))

  res_5_manual_1966 <- c(1.966000e+03,
                         1.633420e+00,
                         5.706769e-02,
                         1.065658e+01,
                         6.130240e-03,
                         8.432360e+00,
                         1.059951e+01)  # 2024-08-27
  expect_equal(res_5$intervals[1, 1:7] %>% as.numeric(),
               res_5_manual_1966,
               tolerance = 1e-5)

  expect_invisible(plot(res_5,
                        type = "hdi"))
  expect_invisible(plot(res_5,
                        type = "eti"))

  expect_invisible(plot(res_5,
                        join_intervals = TRUE,
                        y_tick_start = NULL))

  expect_error(plot(res_5,
                    type = "very demure"))

  expect_no_error(summary_table(res_5)) # Returns a knitr kable, so just check
                                        #  it runs

  res_5_char <- res_5
  res_5_char$intervals_all$quantity <- c("hello", "goodbye")
  expect_error(plot(res_5_char))

})
