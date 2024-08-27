# create_intervals(). Quick test just to keep GHA happy for now.
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

  # Density = TRUE, just check the first values
  res_2021_density_true_create <- create_intervals(rec_2021,
                                                   density = TRUE)

  res_2021_density_true_manual <- c(10.187300,  4.085088, 29.499385,  2.529758,
                                    25.517941) # calculated 2024-06-05

  expect_equal(res_2021_density_true_create$intervals[1:5] %>% as.numeric(),
               res_2021_density_true_manual,
               tolerance = 1e-5)

  # Density = TRUE, from = NULL
  res_2021_density_from_create <- create_intervals(rec_2021,
                                                   density = TRUE,
                                                   from = NULL)

  res_2021_density_from_manual <- c(10.1873,
                                    4.085088,
                                    29.49938,
                                    2.529013,
                                    25.51883) # calculated 2024-08-27, slightly
                                        # different to above that has from=0

  expect_equal(res_2021_density_from_create$intervals[1:5] %>% as.numeric(),
               res_2021_density_from_manual,
               tolerance = 1e-5)

  # Density = TRUE, allow_hdi_zero = FALSE
  res_1 <- create_intervals(dplyr::pull(hake_recruitment_mcmc, "1966"))

  res_1_manual <- c(1.63342,
                    0.05706769,
                    10.65658,
                    0.00613024,
                    8.43236)   # calculated 2024-08-27
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
