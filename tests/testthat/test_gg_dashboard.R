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
    panels = c("biplot", "corr", "coef"),
    panel_args = list(coef = list(response = 1))
  )

  expect_s3_class(dash, "gtable")
})
