context("Tests of gg_scoreplot() for mvr models")

test_that("gg_scoreplot returns ggplot object", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
  p <- gg_scoreplot(mod)

  expect_s3_class(p, "ggplot")
})

test_that("gg_scoreplot validates component selection", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  expect_error(gg_scoreplot(mod, comps = 1), "exactly two")
  expect_error(gg_scoreplot(mod, comps = 1:3), "exactly two")
})
