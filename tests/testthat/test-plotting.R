test_that("figures 1, 2, and S1 generation code works", {
  expect_invisible(figure_1())
  file.remove("figure-1.pdf")
  expect_invisible(figure_2())
  file.remove("figure-2.pdf")
  expect_invisible(figure_s1())
  file.remove("figure-s1.pdf")
})
