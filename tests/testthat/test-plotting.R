test_that("figures 1, 2, and S1 generation code works", {
  expect_invisible(figure_1())
  file.remove("figure-1.pdf")
  expect_invisible(figure_2())
  file.remove("figure-2.pdf")
  expect_invisible(figure_s1())
  file.remove("figure-s1.pdf")
})

test_that("remaining plotting functions/options work", {
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
})
