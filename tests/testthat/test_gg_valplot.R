context("Tests of gg_valplot() for validation metrics")

test_that("gg_valplot.mvr returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 4, data = yarn, validation = "CV")
  p <- gg_valplot(mod, val.type = "RMSEP", estimate = c("train", "CV"),
                  main = "Validation title")

  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$x, "number of components")
  expect_identical(p$labels$y, "RMSEP")
  expect_identical(p$labels$title, "Validation title")
})

test_that("gg_valplot.mvrVal supports multiple responses", {
  skip_if_not_installed("ggplot2")
  data(oliveoil)

  mod <- plsr(sensory ~ chemical, ncomp = 3, data = oliveoil,
              validation = "LOO")
  val <- RMSEP(mod, estimate = "CV")
  p <- gg_valplot(val)

  expect_s3_class(p, "ggplot")
  expect_true(inherits(p$facet, "FacetWrap"))
})

test_that("gg_valplot routes val.type to metric function", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 4, data = yarn, validation = "CV")
  p <- gg_valplot(mod, val.type = "R2", estimate = "train")

  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$y, "R2")
})
