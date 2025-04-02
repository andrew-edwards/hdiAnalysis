test_that("figures 1, 2, and S1 generation code works", {
  expect_invisible(figure_1())
  file.remove("figure-1.pdf")
  expect_invisible(figure_2())
  file.remove("figure-2.pdf")
  expect_invisible(figure_s1())
  file.remove("figure-s1.pdf")
})

test_that("remaining options for plot.intervals_density_list() work", {
  res_all_years_short <- create_intervals(dplyr::select(hake_recruitment_mcmc,
                                                        `1966`:`1969`))
  expect_invisible(plot(res_all_years_short,
                        add_line = 5))
  expect_invisible(plot(res_all_years_short,
                        type = "separate"))

  expect_invisible(plot(res_all_years_short,
                        type = "separate",
                        join_intervals = TRUE))

  expect_invisible(plot(res_all_years_short,
                        type = "separate",
                        add_line_at_0.4 = TRUE))

  expect_invisible(plot(res_all_years_short,
                        type = "separate",
                        add_line = 0.5))

  expect_invisible(plot(res_all_years_short,
                        type = "separate",
                        y_ticks_start = NULL))

  res_all_years_short_2 <- res_all_years_short
  res_all_years_short_2$intervals_all$quantity  <- rep("a", 4)
  expect_error(plot(res_all_years_short_2,
                        type = "separate"))

})


test_that("plot_pointwise__intervals() works", {
  res_vec <- create_intervals(rec_2021)
  set.seed(42)
  vec_sub <- sample(rec_2021, 200)    # then can easily see the 5 at each end
  res_sub <- create_intervals(vec_sub)

  expect_invisible(plot_pointwise_intervals(rec_2021,
                                            res_vec))

  expect_invisible(plot_pointwise_intervals(rec_2021,
                                            res_vec,
                                            type = "moving_window"))
})
