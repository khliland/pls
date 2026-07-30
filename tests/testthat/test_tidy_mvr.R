context("Tests of tidy/glance/augment for mvr")

test_that("tidy.mvr returns expected long-format fields", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 4, data = yarn, validation = "CV")

  td <- tidy(mod)
  expect_true(is.data.frame(td))
  expect_true(all(c("kind", "name", "response", "component", "value") %in% names(td)))
  expect_true(all(c("weights", "loadings", "coefficients") %in% unique(td$kind)))
})

test_that("glance.mvr returns one-row summary", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 4, data = yarn, validation = "CV")

  gl <- glance(mod)
  expect_true(is.data.frame(gl))
  expect_equal(nrow(gl), 1)
  expect_true(all(c("method", "ncomp", "nobs", "nresp", "r2_train_mean",
                    "rmsep_train_mean", "r2_cv_mean", "rmsep_cv_mean") %in% names(gl)))
})

test_that("augment.mvr adds fitted and residual columns", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 4, data = yarn)

  aug <- augment(mod)
  expect_true(is.data.frame(aug))
  expect_true(any(grepl("^\\.fitted_", names(aug))))
  expect_true(any(grepl("^\\.resid_", names(aug))))
})

test_that("augment.mvr on newdata adds fitted columns", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 4, data = yarn)
  newd <- yarn[1:5, , drop = FALSE]

  aug <- augment(mod, data = newd)
  expect_true(is.data.frame(aug))
  expect_true(any(grepl("^\\.fitted_", names(aug))))
  expect_false(any(grepl("^\\.resid_", names(aug))))
})

test_that("tidy.mvrVal returns long-format validation metrics", {
  data(yarn)
  mod <- plsr(density ~ NIR, ncomp = 4, data = yarn, validation = "CV")

  rm <- RMSEP(mod, estimate = c("train", "CV"))
  td <- tidy(rm)

  expect_true(is.data.frame(td))
  expect_true(all(c("metric", "estimate", "response", "component",
                    "model", "cumulative", "value") %in% names(td)))
  expect_true(all(unique(td$metric) == "RMSEP"))
  expect_true(all(c("train", "CV") %in% unique(td$estimate)))

  r2 <- tidy(R2(mod, estimate = "train"))
  expect_true(all(unique(r2$metric) == "R2"))

  # Verify that long-format labels map back to original array values.
  val_arr <- rm$val
  mapped_vals <- mapply(
    function(est, resp, model) val_arr[est, resp, model],
    td$estimate, td$response, td$model,
    USE.NAMES = FALSE
  )
  expect_equal(td$value, as.numeric(mapped_vals), tolerance = 1e-12)
})
