context("Tests of refit() for mvr models")

data(yarn)

base_model <- plsr(density ~ NIR, ncomp = 6, data = yarn)
base_mf <- model.frame(base_model)
selected_ids <- rownames(base_mf)[1:5]

test_that("refit() excludes selected rows when exclude = TRUE", {
  updated <- refit(base_model, selection = selected_ids, exclude = TRUE)
  updated_ids <- rownames(model.frame(updated))

  expect_equal(nrow(model.frame(updated)), nrow(base_mf) - length(selected_ids))
  expect_false(any(selected_ids %in% updated_ids))
})

test_that("refit() keeps only selected rows when exclude = FALSE", {
  updated <- expect_warning(
    refit(base_model, selection = selected_ids, exclude = FALSE),
    "ncomp"
  )
  updated_ids <- rownames(model.frame(updated))

  expect_equal(nrow(model.frame(updated)), length(selected_ids))
  expect_setequal(updated_ids, selected_ids)
})

test_that("refit() validates input", {
  expect_error(
    refit(base_model, selection = "no-such-id", exclude = TRUE),
    "Unknown selected IDs"
  )
  expect_error(
    refit(base_model, selection = selected_ids, exclude = NA),
    "exclude"
  )
  expect_error(
    refit(base_model, selection = rownames(base_mf), exclude = TRUE),
    "leaves no observations"
  )
})
