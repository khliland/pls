context("Tests of ggplot-based loading, biplot and autoplot for mvr")

test_that("gg_loadingplot and gg_biplot return ggplot objects", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  expect_s3_class(gg_loadingplot(mod), "ggplot")
  expect_s3_class(gg_biplot(mod), "ggplot")
  expect_s3_class(gg_biplot(mod, arrows = TRUE), "ggplot")
})

test_that("autoplot.mvr delegates by type", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")

  expect_s3_class(ggplot2::autoplot(mod, type = "scores"), "ggplot")
  expect_s3_class(ggplot2::autoplot(mod, type = "loadings"), "ggplot")
  expect_s3_class(ggplot2::autoplot(mod, type = "biplot"), "ggplot")
  expect_s3_class(
    ggplot2::autoplot(mod, type = "validation", val.type = "RMSEP",
                      estimate = "CV"),
    "ggplot"
  )
  expect_s3_class(autoplot(mod, type = "scores"), "ggplot")
})

test_that("component and scaling validation works", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  expect_error(gg_loadingplot(mod, comps = 1), "exactly two")
  expect_error(gg_biplot(mod, comps = 1), "exactly two")
  expect_error(gg_biplot(mod, arrows = NA), "arrows")
  expect_error(gg_biplot(mod, loading.scale = 0), "positive")
})
