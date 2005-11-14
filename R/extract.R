### extract.R:  Extraction functions
### $Id$

## coef.mvr: Extract the base variable regression coefficients from
## an mvr object.
coef.mvr <- function(object, comps = object$ncomp, intercept = FALSE,
                       cumulative = TRUE, ...)
{
    if (cumulative) {
        ## Cumulative coefficients:
        B <- object$coefficients[,,comps, drop=FALSE]
        if (intercept == TRUE) {      # Intercept has only meaning for
                                      # cumulative coefficients
            dB <- dim(B)
            dB[1] <- dB[1] + 1
            dnB <- dimnames(B)
            dnB[[1]] <- c("(Intercept)", dnB[[1]])
            BInt <- array(dim = dB, dimnames = dnB)
            BInt[-1,,] <- B
            for (i in seq(along = comps))
                BInt[1,,i] <- object$Ymeans - object$Xmeans %*% B[,,i]
            B <- BInt
        }
    } else {
        ## Individual coefficients:
        B <- object$coefficients[,,comps, drop=FALSE]
        g1 <- which(comps > 1)
        ## Indiv. coef. must be calculated since object$coefficients is
        ## cumulative coefs.
        B[,,g1] <- B[,,g1, drop=FALSE] -
            object$coefficients[,,comps[g1] - 1, drop=FALSE]
        dimnames(B)[[3]] <- paste("Comp", comps)
    }
    return(B)
}

## fitted.mvr: Extract the fitted values.  It is needed because the case
## na.action == "na.exclude" must be treated differently from what is done
## in fitted.default.
fitted.mvr <- function(object, ...) {
    if (inherits(object$na.action, "exclude")) {
        naExcludeMvr(object$na.action, object$fitted.values)
    } else {
        object$fitted.values
    }
}

## residuals.mvr: Extract the residuals.  It is needed because the case
## na.action == "na.exclude" must be treated differently from what is done
## in residuals.default.
residuals.mvr <- function(object, ...) {
    if (inherits(object$na.action, "exclude")) {
        naExcludeMvr(object$na.action, object$residuals)
    } else {
        object$residuals
    }
}

## naExcludeMvr: Perform the equivalent of naresid.exclude and
## napredict.exclude on three-dimensional arrays where the first dimension
## corresponds to the observations.
## Almost everything here is lifted verbatim from naresid.exclude (R 2.2.0)
naExcludeMvr <- function(omit, x, ...) {
    if (length(omit) == 0 || !is.numeric(omit))
        stop("invalid argument 'omit'")
    if (length(x) == 0)
        return(x)
    n <- nrow(x)
    keep <- rep.int(NA, n + length(omit))
    keep[-omit] <- 1:n
    x <- x[keep,,, drop = FALSE]        # This is where the real difference is!
    temp <- rownames(x)
    if (length(temp)) {
        temp[omit] <- names(omit)
        rownames(x) <- temp
    }
    return(x)
}


## loadings is in stats, but unfortunately doesn't work for prcomp objects).

## scores: Return the scores (also works for prcomp/princomp objects):
scores <- function(object, ...) UseMethod("scores")
scores.default <- function(object, ...) object$scores
scores.prcomp <- function(object, ...) object$x

## Yscores: Return the Yscores
Yscores <- function(object) object$Yscores

## loading.weights: Return the loading weights:
loading.weights <- function(object) object$loading.weights

## Yloadings: Return the Yloadings
Yloadings <- function(object) object$Yloadings

## model.frame.mvr: Extract or generate the model frame from a `mvr' object.
## It is simply a slightly modified `model.frame.lm'.
model.frame.mvr <- function(formula, ...)
{
    dots <- list(...)
    nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
    if (length(nargs) || is.null(formula$model)) {
        fcall <- formula$call
        fcall$method <- "model.frame"
        fcall[[1]] <- as.name("mvr")
        fcall[names(nargs)] <- nargs
        env <- environment(formula$terms)
        if (is.null(env)) env <- parent.frame()
        eval(fcall, env, parent.frame())
    }
    else formula$model
}

## model.matrix.mvr: Extract the model matrix from an `mvr' object.
## It is a modified version of model.matrix.lm.
model.matrix.mvr <- function(object, ...)
{
    if (n_match <- match("x", names(object), 0))
        object[[n_match]]
    else {
        data <- model.frame(object, ...)
        mm <- NextMethod("model.matrix", data = data)
        ## model.matrix.default prepends the term name to the colnames of
        ## matrices.  If there is only one predictor term, and the
        ## corresponding matrix has colnames, remove the prepended term name:
        mt <- terms(object)
        if (length(attr(mt, "term.labels")) == 1 &&
            !is.null(colnames(data[[attr(mt, "term.labels")]])))
            colnames(mm) <- sub(attr(mt, "term.labels"), "", colnames(mm))
        return(mm)
    }
}

## The following "extraction" functions are mostly used in plot and summary
## functions.

## The names of the response variables:
respnames <- function(object)
    dimnames(fitted(object))[[2]]

## The names of the prediction variables:
prednames <- function(object, intercept = FALSE) {
    if (identical(TRUE, intercept))
        c("(Intercept)", rownames(loadings(object)))
    else
        rownames(loadings(object))
}

## The names of the components:
## Note: The components must be selected prior to the format statement
compnames <- function(object, comps = 1:object$ncomp, explvar = FALSE) {
    labs <- colnames(scores(object))[comps]
    if (identical(TRUE, explvar) && !is.null(evar <- explvar(object)[comps]))
        labs <- paste(labs, " (", format(evar, digits = 2, trim = TRUE),
                      " %)", sep = "")
    return(labs)
}

## The explained X variance:
explvar <- function(object)
    switch(class(object)[1],
           mvr = 100 * object$Xvar / object$Xtotvar,
           princomp =,
           prcomp = 100 * object$sdev^2 / sum(object$sdev^2)
           )

