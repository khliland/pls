context("Tests of additional ggplot-based plots for mvr")

test_that("gg_predplot, gg_corrplot and gg_coefplot return ggplot objects", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  d <- yarn[1:20, , drop = FALSE]
  mod <- plsr(density ~ NIR, ncomp = 4, data = d, validation = "CV")

  expect_s3_class(gg_predplot(mod, ncomp = 1:2), "ggplot")
  expect_s3_class(
    gg_predplot(mod, ncomp = 1:2, which = c("train", "validation")),
    "ggplot"
  )
  expect_s3_class(gg_corrplot(mod), "ggplot")
  expect_s3_class(gg_corrplot(mod, ploty = TRUE), "ggplot")
  expect_s3_class(gg_coefplot(mod, ncomp = 1:2), "ggplot")
})

test_that("gg_loadingplot supports scatter and line modes", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  d <- yarn[1:20, , drop = FALSE]
  mod <- plsr(density ~ NIR, ncomp = 4, data = d)

  expect_s3_class(gg_loadingplot(mod, scatter = TRUE), "ggplot")
  expect_s3_class(gg_loadingplot(mod, scatter = FALSE), "ggplot")
})

test_that("gg_coefplot supports jackknife whiskers", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  d <- yarn[1:20, , drop = FALSE]
  mod <- plsr(density ~ NIR, ncomp = 2, data = d, validation = "LOO",
              jackknife = TRUE)

  expect_s3_class(gg_coefplot(mod, ncomp = 2, se.whiskers = TRUE), "ggplot")
  expect_error(
    gg_coefplot(mod, ncomp = 2, se.whiskers = TRUE, intercept = TRUE),
    "se.whiskers"
  )
})
