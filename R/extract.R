### extract.R:  Extraction functions

## coef.mvr: Extract the base variable regression coefficients from
## an mvr object.
coef.mvr <- function(object, ncomp = object$ncomp, comps, intercept = FALSE,
                     ...)
{
    if (missing(comps) || is.null(comps)) {
        ## Cumulative coefficients:
        B <- object$coefficients[,,ncomp, drop=FALSE]
        if (isTRUE(intercept)) {      # Intercept only has meaning for
                                      # cumulative coefficients
            dB <- dim(B)
            dB[1] <- dB[1] + 1
            dnB <- dimnames(B)
            dnB[[1]] <- c("(Intercept)", dnB[[1]])
            BInt <- array(dim = dB, dimnames = dnB)
            BInt[-1,,] <- B
            for (i in seq(along = ncomp))
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

## loadings is in stats, but doesn't work for prcomp objects, and is not
## generic, so we build our own:
loadings <- function(object, ...) UseMethod("loadings")
loadings.default <- function(object, ...) {
    L <- if (inherits(object, "prcomp")) object$rotation else object$loadings
    if (!(inherits(L, "loadings") || inherits(L, "list")))
        class(L) <- "loadings"
    attr(L, "explvar") <- explvar(object)
    L
}

## scores: Return the scores (also works for prcomp/princomp objects):
scores <- function(object, ...) UseMethod("scores")
scores.default <- function(object, ...) {
    S <- if (inherits(object, "prcomp")) object$x else object$scores
    if (!(inherits(S, "scores") || inherits(S, "list")))
        class(S) <- "scores"
    attr(S, "explvar") <- explvar(object)
    S
}

## Yscores: Return the Yscores
Yscores <- function(object) object$Yscores

## loading.weights: Return the loading weights:
loading.weights <- function(object) object$loading.weights

## Yloadings: Return the Yloadings
Yloadings <- function(object) object$Yloadings

## model.frame.mvr: Extract or generate the model frame from a `mvr' object.
## It is simply a slightly modified `model.frame.lm'.
model.frame.mvr <- function(formula, ...) {
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
model.matrix.mvr <- function(object, ...) {
    if (n_match <- match("x", names(object), 0))
        object[[n_match]]
    else {
        data <- model.frame(object, ...)
        mm <- NextMethod("model.matrix", data = data)
	mm <- delete.intercept(mm) # Deletes any intercept coloumn
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

## delete.intercept: utilitiy function that deletes the response coloumn from
## a model matrix, and adjusts the "assign" attribute:
delete.intercept <- function(mm) {
    ## Save the attributes prior to removing the intercept coloumn:
    saveattr <- attributes(mm)
    ## Find the intercept coloumn:
    intercept <- which(saveattr$assign == 0)
    ## Return if there was no intercept coloumn:
    if (!length(intercept)) return(mm)
    ## Remove the intercept coloumn:
    mm <- mm[,-intercept, drop=FALSE]
    ## Update the attributes with the new dimensions:
    saveattr$dim <- dim(mm)
    saveattr$dimnames <- dimnames(mm)
    ## Remove the assignment of the intercept from the attributes:
    saveattr$assign <- saveattr$assign[-intercept]
    ## Restore the (modified) attributes:
    attributes(mm) <- saveattr
    ## Return the model matrix:
    mm
}

## The following "extraction" functions are mostly used in plot and summary
## functions.

## The names of the response variables:
respnames <- function(object)
    dimnames(fitted(object))[[2]]

## The names of the prediction variables:
prednames <- function(object, intercept = FALSE) {
    if (isTRUE(intercept))
        c("(Intercept)", rownames(object$loadings))
    else
        rownames(object$loadings)
}

## The names of the components:
## Note: The components must be selected prior to the format statement
compnames <- function(object, comps, explvar = FALSE, ...) {
    M <- if (is.matrix(object)) object else scores(object)
    labs <- colnames(M)
    if (missing(comps))
        comps <- seq(along = labs)
    else
        labs <- labs[comps]
    if (isTRUE(explvar) && !is.null(evar <- explvar(M)[comps]))
        labs <- paste(labs, " (", format(evar, digits = 2, trim = TRUE),
                      " %)", sep = "")
    return(labs)
}


## The explained X variance:
explvar <- function(object)
    switch(class(object)[1],
           mvr = 100 * object$Xvar / object$Xtotvar,
           princomp =,
           prcomp = 100 * object$sdev^2 / sum(object$sdev^2),
           scores =,
           loadings = attr(object, "explvar")
           )
