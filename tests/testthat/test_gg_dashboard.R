context("Tests of gg_dashboard() for mvr models")

test_that("gg_dashboard returns a static dashboard object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
  dash <- gg_dashboard(mod)

  expect_s3_class(dash, "gtable")
})

test_that("gg_dashboard returns plotly output when requested", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
  dash <- gg_dashboard(mod, plotly = TRUE)

  expect_s3_class(dash, "plotly")
  expect_true(isTRUE(dash$x$layout$showlegend))

  legend_targets <- vapply(
    dash$x$data,
    function(trace) if (is.null(trace$legend)) "" else trace$legend,
    character(1)
  )
  expect_false(any(legend_targets == "legend2"))
})

test_that("gg_dashboard namespaces plotly legend groups by panel", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
  dash <- gg_dashboard(
    mod,
    panels = c("validation", "coef"),
    plotly = TRUE,
    plotly_legend = "panel"
  )

  legend_groups <- vapply(
    dash$x$data,
    function(trace) if (is.null(trace$legendgroup)) "" else trace$legendgroup,
    character(1)
  )
  named_groups <- legend_groups[nzchar(legend_groups)]

  expect_true(length(named_groups) > 0)
  expect_true(all(grepl("^panel[0-9]+::", named_groups)))

  legend_targets <- vapply(
    dash$x$data,
    function(trace) if (is.null(trace$legend)) "" else trace$legend,
    character(1)
  )
  expect_true(any(legend_targets == "legend2"))
  expect_true("legend2" %in% names(dash$x$layout))
})

test_that("gg_dashboard validates panel names and arguments", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  expect_error(gg_dashboard(mod, panels = character(0)), "at least one")
  expect_error(gg_dashboard(mod, panels = c("score", "score")), "duplicates")
  expect_error(gg_dashboard(mod, panels = "does-not-exist"), "Unknown dashboard panel")
  expect_error(gg_dashboard(mod, panel_args = list(foo = list())), "Unknown panel_args")
  expect_error(gg_dashboard(mod, plotly_legend = "invalid"), "arg")
})

test_that("gg_dashboard supports alternative panels", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  data(oliveoil)

  mod <- plsr(chemical ~ sensory, ncomp = 4, data = oliveoil, validation = "CV")
  dash <- gg_dashboard(
    mod,
    panels = c("biplot", "corr", "coef"),
    panel_args = list(coef = list(response = 1))
  )

  expect_s3_class(dash, "gtable")
})
