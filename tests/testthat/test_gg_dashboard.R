context("Tests of gg_dashboard() for mvr models")

test_that("gg_dashboard returns a static dashboard object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
  dash <- gg_dashboard(mod)

  expect_s3_class(dash, "gtable")
})

test_that("gg_dashboard returns html output when requested", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
  dash <- gg_dashboard(mod, plotly = TRUE)

  expect_s3_class(dash, "shiny.tag")
  expect_equal(dash$attribs$class, "pls-dashboard")
  expect_true(grepl("display: grid;", dash$attribs$style))
})

test_that("gg_dashboard validates panel names and arguments", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  expect_error(gg_dashboard(mod, panels = character(0)), "at least one")
  expect_error(gg_dashboard(mod, panels = c("score", "score")), "duplicates")
  expect_error(gg_dashboard(mod, panels = "does-not-exist"), "Unknown dashboard panel")
  expect_error(gg_dashboard(mod, panel_args = list(foo = list())), "Unknown panel_args")
})

test_that("gg_dashboard supports alternative panels", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  data(oliveoil)

  mod <- plsr(chemical ~ sensory, ncomp = 4, data = oliveoil, validation = "CV")
  dash <- gg_dashboard(
    mod,
    panels = c("biplot", "corr", "coef", "rawdata"),
    panel_args = list(coef = list(response = 1),
                      rawdata = list(plottype = "line"))
  )

  expect_s3_class(dash, "gtable")
})

test_that("gg_dashboard validates the link argument", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  expect_error(gg_dashboard(mod, link = NA), "`link` must be a single")
  expect_error(gg_dashboard(mod, link = c(TRUE, FALSE)), "`link` must be a single")
})

test_that("gg_dashboard rejects select = TRUE for the scores panel", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

  expect_error(
    gg_dashboard(mod, panel_args = list(scores = list(select = TRUE))),
    "select = TRUE. is not supported"
  )
  expect_error(
    gg_dashboard(mod, plotly = TRUE,
                 panel_args = list(scores = list(select = TRUE))),
    "select = TRUE. is not supported"
  )
})

test_that("gg_dashboard links panels with crosstalk groups when requested", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("crosstalk")
  skip_if_not_installed("htmltools")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
  html_of <- function(x) as.character(htmltools::renderTags(x)$html)

  linked <- html_of(gg_dashboard(mod, plotly = TRUE))
  expect_true(grepl('"ctGroups":["samples"]', linked, fixed = TRUE))
  expect_true(grepl('"ctGroups":["variables"]', linked, fixed = TRUE))

  unlinked <- html_of(gg_dashboard(mod, plotly = TRUE, link = FALSE))
  expect_false(grepl("ctGroups", unlinked, fixed = TRUE))
})

test_that("gg_dashboard links the biplot to both sample and variable groups", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("crosstalk")
  skip_if_not_installed("htmltools")
  data(oliveoil)

  mod <- plsr(chemical ~ sensory, ncomp = 4, data = oliveoil, validation = "CV")
  html <- as.character(htmltools::renderTags(
    gg_dashboard(mod, panels = c("biplot", "corr"), plotly = TRUE)
  )$html)

  expect_true(grepl("var('selection').on('change', apply)", html, fixed = TRUE))
  expect_true(grepl('"groups":["samples","variables"]', html, fixed = TRUE))
  expect_true(grepl('"meta":{"set":"variables"', html, fixed = TRUE))
  expect_true(grepl('"meta":{"set":"samples"', html, fixed = TRUE))
})

test_that("gg_dashboard clears all linked groups on double-click", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("crosstalk")
  skip_if_not_installed("htmltools")
  data(oliveoil)

  mod <- plsr(chemical ~ sensory, ncomp = 4, data = oliveoil, validation = "CV")
  html <- as.character(htmltools::renderTags(
    gg_dashboard(mod, panels = c("biplot", "scores"), plotly = TRUE)
  )$html)

  expect_true(grepl("plotly_doubleclick", html, fixed = TRUE))
  expect_true(grepl("plotly_deselect", html, fixed = TRUE))
  expect_true(grepl("crosstalk.group(g).var('selection').set(null)", html, fixed = TRUE))
  expect_true(grepl("'selectedpoints'", html, fixed = TRUE))
  expect_true(grepl('"groups":["samples","variables"]', html, fixed = TRUE))
})

test_that("gg_dashboard uses click selection for spectral line panels", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("plotly")
  skip_if_not_installed("crosstalk")
  skip_if_not_installed("htmltools")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
  html <- as.character(htmltools::renderTags(
    gg_dashboard(mod, panels = c("loadings", "scores"), plotly = TRUE)
  )$html)

  expect_true(grepl("plotly_click", html, fixed = TRUE))
  expect_true(grepl("plotly_selected", html, fixed = TRUE))
  expect_true(grepl("_plsMask", html, fixed = TRUE))
})

test_that("gg_dashboard ignores link when plotly = FALSE", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("gridExtra")
  data(yarn)

  mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
  expect_s3_class(gg_dashboard(mod, link = TRUE), "gtable")
  expect_s3_class(gg_dashboard(mod, link = FALSE), "gtable")
})
