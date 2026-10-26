context("Tests of ggplot-based loading, biplot and autoplot for mvr")

test_that("gg_loadingplot and gg_biplot return ggplot objects", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  expect_s3_class(gg_loadingplot(mod), "ggplot")
  biplot <- gg_biplot(mod, main = "Biplot title")
  expect_s3_class(biplot, "ggplot")
  expect_identical(biplot$labels$title, "Biplot title")
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
    ggplot2::autoplot(mod, type = "rawdata", plottype = "line"),
    "ggplot"
  )
  expect_s3_class(
    ggplot2::autoplot(mod, type = "validation", val.type = "RMSEP",
                      estimate = "CV"),
    "ggplot"
  )
  expect_s3_class(autoplot(mod, type = "scores"), "ggplot")
})

test_that("gg_rawdataplot selects plot mode with plottype", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  raw <- gg_rawdataplot(mod, plottype = "line", main = "Raw data title")
  expect_s3_class(raw, "ggplot")
  expect_identical(raw$labels$title, "Raw data title")
  expect_s3_class(gg_rawdataplot(mod, plottype = "violin"), "ggplot")
})

test_that("biplot and raw data titles survive Plotly conversion", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
  biplot <- gg_biplot(mod, plotly = TRUE, main = "Plotly biplot")
  raw <- gg_rawdataplot(mod, plotly = TRUE, main = "Plotly raw data")

  expect_identical(plotly::plotly_build(biplot)$x$layout$title$text,
                   "Plotly biplot")
  expect_identical(plotly::plotly_build(raw)$x$layout$title$text,
                   "Plotly raw data")
})

test_that("gg_rawdataplot thins categorical plotly labels to max_ticks", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")

  X <- matrix(stats::rnorm(20 * 60), 20, 60)
  colnames(X) <- paste0("V", 1:60)

  box <- plotly::plotly_build(gg_rawdataplot(X, plottype = "boxplot",
                                             plotly = TRUE, max_ticks = 12L))
  expect_lte(length(box$x$layout$xaxis$ticktext), 12L)
  expect_gt(length(box$x$layout$xaxis$ticktext), 1L)

  line <- plotly::plotly_build(gg_rawdataplot(X, plottype = "line",
                                              plotly = TRUE, max_ticks = 12L))
  expect_lte(length(line$x$layout$xaxis$ticktext), 12L)
  expect_identical(line$x$layout$xaxis$type, "linear")
})

test_that("component and scaling validation works", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  expect_s3_class(gg_loadingplot(mod, comps = 1), "ggplot")
  expect_error(gg_biplot(mod, comps = 1), "exactly two")
  expect_error(gg_biplot(mod, arrows = NA), "arrows")
  expect_error(gg_biplot(mod, loading.scale = 0), "positive")
})
