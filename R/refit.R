### refit.R: Refit mvr models after selecting observations


#' @title Refit a PLSR/PCR model after selecting observations
#'
#' @description
#' Refit an existing \code{mvr} model after selecting observations from the
#' training set. The selection can be interpreted either negatively
#' (exclude selected observations) or positively (keep only selected
#' observations).
#'
#' @param object an object to refit.
#' @param ... additional arguments passed to methods.
#'
#' @return A refitted model object.
#' @export
refit <- function(object, ...) UseMethod("refit")


#' @rdname refit
#' @param selection character or numeric vector identifying selected
#' observations. Values are matched against row names in the model frame used
#' for fitting.
#' @param exclude logical. If \code{TRUE} (default), selected observations are
#' removed before refitting. If \code{FALSE}, only selected observations are
#' kept.
#' @examples
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
#' ids <- rownames(model.frame(mod))[1:5]
#' mod_excluded <- refit(mod, selection = ids, exclude = TRUE)
#' mod_included <- refit(mod, selection = ids, exclude = FALSE)
#' @export
refit.mvr <- function(object, selection = NULL, exclude = TRUE, ...) {
  if (is.null(selection))
    return(object)

  if (!is.logical(exclude) || length(exclude) != 1 || is.na(exclude))
    stop("`exclude` must be a single non-missing logical value.")

  train_data <- model.frame(object)
  ids <- rownames(train_data)
  if (is.null(ids)) ids <- as.character(seq_len(nrow(train_data)))

  selection <- unique(as.character(selection))
  if (length(selection) == 0)
    return(object)
  if (anyNA(selection))
    stop("`selection` cannot contain missing values.")

  missing_ids <- setdiff(selection, ids)
  if (length(missing_ids) > 0)
    stop("Unknown selected IDs: ", paste(missing_ids, collapse = ", "))

  keep <- if (isTRUE(exclude)) !(ids %in% selection) else ids %in% selection
  if (!any(keep))
    stop("Selection leaves no observations for refitting.")

  refit_data <- train_data[keep,, drop = FALSE]

  dots <- list(...)
  if (is.null(dots$ncomp)) {
    max_ncomp <- min(nrow(refit_data) - 1, ncol(model.matrix(object, data = refit_data)))
    if (max_ncomp < 1)
      stop("Selection leaves too few observations to fit a model.")
    if (object$ncomp > max_ncomp) {
      warning("`ncomp` reduced to ", max_ncomp, " for refit data.")
      dots$ncomp <- max_ncomp
    }
  }

  do.call(update, c(list(object, data = refit_data, subset = NULL), dots))
}
