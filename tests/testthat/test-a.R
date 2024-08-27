# Test helper function a()

test_that("a() works on a tibble", {
  expect_equal(hake_recruitment_mcmc %>% a(),
               as.data.frame(hake_recruitment_mcmc))
})
