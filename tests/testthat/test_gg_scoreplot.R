context("Tests of gg_scoreplot() for mvr models")

test_that("gg_scoreplot returns ggplot object", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
  p <- gg_scoreplot(mod, main = "Score title")

  expect_s3_class(p, "ggplot")
  expect_identical(p$labels$title, "Score title")
  expect_null(gg_scoreplot(mod)$labels$title)
})

test_that("gg_scoreplot plots extracted scores", {
  skip_if_not_installed("ggplot2")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
  score_mat <- scores(mod)

  expect_s3_class(gg_scoreplot(score_mat), "ggplot")
  expect_identical(
    gg_scoreplot(score_mat, main = "Extracted scores")$labels$title,
    "Extracted scores"
  )
  expect_s3_class(gg_scoreplot(score_mat, comps = 2:3, size = 2), "ggplot")
  expect_error(gg_scoreplot(score_mat, comps = 1:3), "one or two")

  rownames(score_mat) <- NULL
  expect_s3_class(gg_scoreplot(score_mat), "ggplot")
})

test_that("gg_scoreplot handles a single component", {
  skip_if_not_installed("ggplot2")
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 1, data = yarn)
  score_mat <- scores(mod)

  default_plot <- gg_scoreplot(mod)
  selected_plot <- gg_scoreplot(score_mat, comps = 1)
  for (p in list(default_plot, selected_plot)) {
    built <- ggplot2::ggplot_build(p)$data[[1]]
    expect_equal(built$x, seq_len(nrow(score_mat)))
    expect_equal(built$y, as.numeric(score_mat[, 1]))
  }

  expect_error(gg_scoreplot(mod, comps = 2), "not available")
  expect_error(gg_scoreplot(scores(plsr(density ~ NIR, ncomp = 3, data = yarn)),
                            comps = 1:3), "one or two")
})

test_that("gg_scoreplot returns plotly object when requested", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
  p <- gg_scoreplot(mod, plotly = TRUE, main = "Plotly scores")

  expect_s3_class(p, "plotly")
  expect_identical(plotly::plotly_build(p)$x$layout$title$text, "Plotly scores")
})

test_that("gg_scoreplot supports plotly for extracted scores", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
  p <- gg_scoreplot(scores(mod), plotly = TRUE, main = "Extracted Plotly scores")
  expect_s3_class(p, "plotly")
  expect_identical(
    plotly::plotly_build(p)$x$layout$title$text,
    "Extracted Plotly scores"
  )
})
