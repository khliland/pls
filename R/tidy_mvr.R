### tidy_mvr.R: broom-style methods for mvr models


#' @title Tidy mvr Model Components
#'
#' @description
#' \code{tidy.mvr} returns model components as a long data frame for plotting
#' and tabulation.
#'
#' @param x an object.
#' @param ... further arguments passed to methods.
#' @return A data frame.
#' @export
#'
#' @examples
#' data(yarn)
#' d <- yarn[1:20, , drop = FALSE]
#' mod <- plsr(density ~ NIR, ncomp = 2, data = d, validation = "CV")
#'
#' td <- tidy(mod, what = c("weights", "loadings", "coefficients"))
#' head(td)
#'
#' rm <- RMSEP(mod, estimate = c("train", "CV"))
#' td_rm <- tidy(rm)
#' head(td_rm)
tidy <- function(x, ...) UseMethod("tidy")


#' @rdname tidy
#' @param what character vector. Any subset of \code{c("weights",
#' "loadings", "coefficients")}. Defaults to all.
#' @param ncomp positive integer. Number of components for coefficient output.
#' @param intercept logical. Include intercept in coefficient output.
#' @export
tidy.mvr <- function(x, what = c("weights", "loadings", "coefficients"),
                     ncomp = x$ncomp, intercept = FALSE, ...) {
  all_what <- c("weights", "loadings", "coefficients")
  what <- all_what[pmatch(what, all_what)]
  if (any(is.na(what)))
    stop("`what` must be a subset of: ", paste(all_what, collapse = ", "))

  out <- list()

  if ("weights" %in% what) {
    W <- loading.weights(x)
    out[[length(out) + 1]] <- data.frame(
      kind = "weights",
      name = rep(rownames(W), ncol(W)),
      response = NA_character_,
      component = rep(seq_len(ncol(W)), each = nrow(W)),
      value = c(W),
      stringsAsFactors = FALSE
    )
  }

  if ("loadings" %in% what) {
    L <- loadings(x)
    out[[length(out) + 1]] <- data.frame(
      kind = "loadings",
      name = rep(rownames(L), ncol(L)),
      response = NA_character_,
      component = rep(seq_len(ncol(L)), each = nrow(L)),
      value = c(L),
      stringsAsFactors = FALSE
    )
  }

  if ("coefficients" %in% what) {
    B <- coef(x, ncomp = ncomp, intercept = intercept)
    dn <- dimnames(B)
    out[[length(out) + 1]] <- data.frame(
      kind = "coefficients",
      name = rep(dn[[1]], length(dn[[2]])),
      response = rep(dn[[2]], each = length(dn[[1]])),
      component = ncomp,
      value = c(B[, , 1, drop = FALSE]),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, out)
}


#' @rdname tidy
#' @export
tidy.mvrVal <- function(x, ...) {
  vals <- x$val
  dn <- dimnames(vals)
  idx <- expand.grid(
    estimate = dn[[1]],
    response = dn[[2]],
    model = dn[[3]],
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  comp_map <- setNames(x$comps, dn[[3]])

  data.frame(
    metric = rep(x$type, nrow(idx)),
    estimate = idx$estimate,
    response = idx$response,
    component = unname(comp_map[idx$model]),
    model = idx$model,
    cumulative = rep(x$cumulative, nrow(idx)),
    value = c(vals),
    stringsAsFactors = FALSE
  )
}


#' @title Glance Summary for mvr Models
#'
#' @description
#' \code{glance.mvr} returns one-row summary metrics from an \code{mvr} model.
#'
#' @param x an object.
#' @param ... further arguments passed to methods.
#' @return A one-row data frame.
#' @export
#'
#' @examples
#' data(yarn)
#' d <- yarn[1:20, , drop = FALSE]
#' mod <- plsr(density ~ NIR, ncomp = 2, data = d, validation = "CV")
#' glance(mod)
glance <- function(x, ...) UseMethod("glance")


#' @rdname glance
#' @param ncomp positive integer. Number of components used for metrics.
#' @export
glance.mvr <- function(x, ncomp = x$ncomp, ...) {
  r2_train <- R2(x, estimate = "train", ncomp = ncomp, intercept = FALSE)$val
  rmsep_train <- RMSEP(x, estimate = "train", ncomp = ncomp,
                       intercept = FALSE)$val

  cv_available <- !is.null(x$validation)
  if (cv_available) {
    r2_cv <- R2(x, estimate = "CV", ncomp = ncomp, intercept = FALSE)$val
    rmsep_cv <- RMSEP(x, estimate = "CV", ncomp = ncomp,
                      intercept = FALSE)$val
    r2_cv_mean <- mean(r2_cv, na.rm = TRUE)
    rmsep_cv_mean <- mean(rmsep_cv, na.rm = TRUE)
  } else {
    r2_cv_mean <- NA_real_
    rmsep_cv_mean <- NA_real_
  }

  data.frame(
    method = x$method,
    ncomp = ncomp,
    nobs = nrow(x$scores),
    nresp = length(x$Ymeans),
    has_validation = cv_available,
    r2_train_mean = mean(r2_train, na.rm = TRUE),
    rmsep_train_mean = mean(rmsep_train, na.rm = TRUE),
    r2_cv_mean = r2_cv_mean,
    rmsep_cv_mean = rmsep_cv_mean,
    stringsAsFactors = FALSE
  )
}


#' @title Augment Data with mvr Fitted Values
#'
#' @description
#' \code{augment.mvr} adds fitted values and residuals to the model frame (or
#' to \code{newdata}, for fitted values only).
#'
#' @param x an object.
#' @param ... further arguments passed to methods.
#' @return A data frame with added prediction columns.
#' @export
#'
#' @examples
#' data(yarn)
#' d <- yarn[1:20, , drop = FALSE]
#' mod <- plsr(density ~ NIR, ncomp = 2, data = d)
#'
#' aug_train <- augment(mod)
#' head(aug_train)
#'
#' aug_new <- augment(mod, data = d[1:5, , drop = FALSE])
#' head(aug_new)
augment <- function(x, ...) UseMethod("augment")


#' @rdname augment
#' @param data optional data frame to augment. If omitted, the model frame is
#' used.
#' @param ncomp positive integer. Number of components used for predictions.
#' @export
augment.mvr <- function(x, data = NULL, ncomp = x$ncomp, ...) {
  use_train <- is.null(data)

  if (use_train) {
    data <- model.frame(x)
    pred <- predict(x, ncomp = ncomp)
    res <- residuals(x)[, , ncomp, drop = FALSE]
  } else {
    pred <- predict(x, ncomp = ncomp, newdata = data)
    res <- NULL
  }

  pred <- pred[, , 1, drop = FALSE]
  pred_names <- dimnames(pred)[[2]]

  out <- data
  for (j in seq_along(pred_names)) {
    out[[paste0(".fitted_", pred_names[j])]] <- pred[, j, 1]
    if (!is.null(res)) {
      out[[paste0(".resid_", pred_names[j])]] <- res[, j, 1]
    }
  }

  out
}
