context("Tests of additional ggplot-based plots for mvr")

test_that("gg_predplot, gg_corrplot and gg_coefplot return ggplot objects", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  d <- yarn[1:20, , drop = FALSE]
  mod <- plsr(density ~ NIR, ncomp = 4, data = d, validation = "CV")

  pred <- gg_predplot(mod, ncomp = 1:2, main = "Prediction title")
  expect_s3_class(pred, "ggplot")
  expect_identical(pred$labels$title, "Prediction title")
  expect_s3_class(
    gg_predplot(mod, ncomp = 1:2, which = c("train", "validation")),
    "ggplot"
  )
  corr <- gg_corrplot(mod, main = "Correlation title")
  expect_s3_class(corr, "ggplot")
  expect_identical(corr$labels$title, "Correlation title")
  expect_s3_class(gg_corrplot(mod, ploty = TRUE), "ggplot")
  coef <- gg_coefplot(mod, ncomp = 1:2, main = "Coefficient title")
  expect_s3_class(coef, "ggplot")
  expect_identical(coef$labels$title, "Coefficient title")
})

test_that("gg_loadingplot supports scatter and line modes", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  d <- yarn[1:20, , drop = FALSE]
  mod <- plsr(density ~ NIR, ncomp = 4, data = d)

  loading <- gg_loadingplot(mod, scatter = TRUE, main = "Loading title")
  expect_s3_class(loading, "ggplot")
  expect_identical(loading$labels$title, "Loading title")
  expect_s3_class(gg_loadingplot(mod, scatter = FALSE), "ggplot")
})

test_that("gg_loadingplot plots extracted loadings", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  d <- yarn[1:20, , drop = FALSE]
  mod <- plsr(density ~ NIR, ncomp = 4, data = d)
  loading_mat <- loadings(mod)

  expect_s3_class(gg_loadingplot(loading_mat), "ggplot")
  expect_identical(
    gg_loadingplot(loading_mat, main = "Extracted loadings")$labels$title,
    "Extracted loadings"
  )
  expect_s3_class(gg_loadingplot(loading_mat, scatter = TRUE), "ggplot")
  plot_23 <- gg_loadingplot(loading_mat, comps = 2:3, scatter = TRUE)
  expect_equal(plot_23$labels$x, compnames(loading_mat, comps = 2, explvar = TRUE))
  expect_equal(plot_23$labels$y, compnames(loading_mat, comps = 3, explvar = TRUE))
  expect_s3_class(
    gg_loadingplot(loading_mat, scatter = TRUE, labels = TRUE),
    "ggplot"
  )
  expect_s3_class(
    gg_loadingplot(loading_mat, labels = rownames(loading_mat), pretty.xlabels = FALSE),
    "ggplot"
  )
  expect_error(gg_loadingplot(loading_mat, comps = 1:3), "one or two")
})

test_that("gg_loadingplot handles a single component", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 1, data = yarn[1:20, , drop = FALSE])
  loading_mat <- loadings(mod)

  default_plot <- gg_loadingplot(mod)
  selected_plot <- gg_loadingplot(loading_mat, comps = 1)
  for (p in list(default_plot, selected_plot)) {
    built <- ggplot2::ggplot_build(p)$data[[1]]
    expect_equal(built$x, seq_len(nrow(loading_mat)))
    expect_equal(built$y, as.numeric(loading_mat[, 1]))
  }

  scatter_plot <- gg_loadingplot(loading_mat, comps = 1, scatter = TRUE)
  scatter_data <- ggplot2::ggplot_build(scatter_plot)$data[[1]]
  expect_equal(scatter_data$x, seq_len(nrow(loading_mat)))
  expect_equal(scatter_data$y, as.numeric(loading_mat[, 1]))
  expect_error(gg_loadingplot(mod, comps = 2), "not available")
})

test_that("gg_loadingplot supports plotly for extracted loadings", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 4, data = yarn[1:20, , drop = FALSE])
  p <- gg_loadingplot(loadings(mod), plotly = TRUE, main = "Plotly loadings")
  expect_s3_class(p, "plotly")
  expect_identical(plotly::plotly_build(p)$x$layout$title$text, "Plotly loadings")
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

test_that("gg_corrplot plotly anchors the y-axis to x for round circles", {
  skip_if_not_installed("plotly")
  data(yarn)

  d <- yarn[1:20, , drop = FALSE]
  mod <- plsr(density ~ NIR, ncomp = 3, data = d, validation = "none")
  built <- plotly::plotly_build(gg_corrplot(mod, comps = 1:3, plotly = TRUE))
  yaxes <- grep("^yaxis", names(built$x$layout), value = TRUE)
  expect_true(length(yaxes) >= 1)
  for (yaxis in yaxes) {
    expect_identical(built$x$layout[[yaxis]]$scaleanchor, "x")
    expect_identical(built$x$layout[[yaxis]]$scaleratio, 1)
  }
})

test_that("gg_corrplot static circles keep a 1:1 aspect and hide the single-type legend", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  d <- yarn[1:20, , drop = FALSE]
  mod <- plsr(density ~ NIR, ncomp = 3, data = d, validation = "none")
  corr <- gg_corrplot(mod, comps = 1:2)
  expect_identical(corr$coordinates$ratio, 1)
  expect_identical(corr$guides$guides$colour, "none")
})
