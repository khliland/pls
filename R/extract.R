### extract.R:  Extraction functions

## coef.mvr: Extract the base variable regression coefficients from
## an mvr object.


#' @name coef.mvr
#' @title Extract Information From a Fitted PLSR or PCR Model
#'
#' @description Functions to extract information from \code{mvr} objects: Regression
#' coefficients, fitted values, residuals, the model frame, the model matrix,
#' names of the variables and components, and the \eqn{X} variance explained by
#' the components.
#'
#' @details These functions are mostly used inside other functions.  (Functions
#' \code{coef.mvr}, \code{fitted.mvr} and \code{residuals.mvr} are usually
#' called through their generic functions \code{\link{coef}},
#' \code{\link{fitted}} and \code{\link{residuals}}, respectively.)
#'
#' \code{coef.mvr} is used to extract the regression coefficients of a model,
#' i.e. the \eqn{B} in \eqn{y = XB} (for the \eqn{Q} in \eqn{y = TQ} where
#' \eqn{T} is the scores, see \code{\link{Yloadings}}).  An array of dimension
#' \code{c(nxvar, nyvar, length(ncomp))} or \code{c(nxvar, nyvar,
#' length(comps))} is returned.
#'
#' If \code{comps} is missing (or is \code{NULL}), \code{coef()[,,ncomp[i]]}
#' are the coefficients for models with \code{ncomp[i]} components, for \eqn{i
#' = 1, \ldots, length(ncomp)}.  Also, if \code{intercept = TRUE}, the first
#' dimension is \eqn{nxvar + 1}, with the intercept coefficients as the first
#' row.
#'
#' If \code{comps} is given, however, \code{coef()[,,comps[i]]} are the
#' coefficients for a model with only the component \code{comps[i]}, i.e. the
#' contribution of the component \code{comps[i]} on the regression
#' coefficients.
#'
#' \code{fitted.mvr} and \code{residuals.mvr} return the fitted values and
#' residuals, respectively.  If the model was fitted with \code{na.action =
#' na.exclude} (or after setting the default \code{na.action} to
#' \code{"na.exclude"} with \code{\link{options}}), the fitted values (or
#' residuals) corresponding to excluded observations are returned as \code{NA};
#' otherwise, they are omitted.
#'
#' \code{model.frame.mvr} returns the model frame; i.e. a data frame with all
#' variables neccessary to generate the model matrix.  See
#' \code{\link[stats]{model.frame}} for details.
#'
#' \code{model.matrix.mvr} returns the (possibly coded) matrix used as \eqn{X}
#' in the fitting.  See \code{\link[stats]{model.matrix}} for details.
#'
#' \code{prednames}, \code{respnames} and \code{compnames} extract the names of
#' the \eqn{X} variables, responses and components, respectively.  With
#' \code{intercept = TRUE} in \code{prednames}, the name of the intercept
#' variable (i.e. \code{"(Intercept)"}) is returned as well.  \code{compnames}
#' can also extract component names from score and loading matrices.  If
#' \code{explvar = TRUE} in \code{compnames}, the explained variance for each
#' component (if available) is appended to the component names.  For optimal
#' formatting of the explained variances when not all components are to be
#' used, one should specify the desired components with the argument
#' \code{comps}.
#'
#' \code{explvar} extracts the amount of \eqn{X} variance (in per cent)
#' explained by each component in the model.  It can also handle score and
#' loading matrices returned by \code{\link{scores}} and
#' \code{\link{loadings}}.
#'
#' @aliases coef.mvr fitted.mvr residuals.mvr model.frame.mvr model.matrix.mvr
#' prednames respnames compnames explvar
#' @param object,formula an \code{mvr} object.  The fitted model.
#' @param ncomp,comps vector of positive integers.  The components to include
#' in the coefficients or to extract the names of.  See below.
#' @param intercept logical.  Whether coefficients for the intercept should be
#' included.  Ignored if \code{comps} is specified.  Defaults to \code{FALSE}.
#' @param explvar logical.  Whether the explained \eqn{X} variance should be
#' appended to the component names.
#' @param \dots other arguments sent to underlying functions.  Currently only
#' used for \code{model.frame.mvr} and \code{model.matrix.mvr}.
#' @return \code{coef.mvr} returns an array of regression coefficients.
#'
#' \code{fitted.mvr} returns an array with fitted values.
#'
#' \code{residuals.mvr} returns an array with residuals.
#'
#' \code{model.frame.mvr} returns a data frame.
#'
#' \code{model.matrix.mvr} returns the \eqn{X} matrix.
#'
#' \code{prednames}, \code{respnames} and \code{compnames} return a character
#' vector with the corresponding names.
#'
#' \code{explvar} returns a numeric vector with the explained variances, or
#' \code{NULL} if not available.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{coef}}, \code{\link{fitted}},
#' \code{\link{residuals}}, \code{\link{model.frame}},
#' \code{\link{model.matrix}}, \code{\link{na.omit}}
#' @keywords regression multivariate
#' @examples
#'
#' data(yarn)
#' mod <- pcr(density ~ NIR, data = yarn[yarn$train,], ncomp = 5)
#' B <- coef(mod, ncomp = 3, intercept = TRUE)
#' ## A manual predict method:
#' stopifnot(drop(B[1,,] + yarn$NIR[!yarn$train,] %*% B[-1,,]) ==
#'           drop(predict(mod, ncomp = 3, newdata = yarn[!yarn$train,])))
#'
#' ## Note the difference in formatting:
#' mod2 <- pcr(density ~ NIR, data = yarn[yarn$train,])
#' compnames(mod2, explvar = TRUE)[1:3]
#' compnames(mod2, comps = 1:3, explvar = TRUE)
#'
#' @export
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
#' @rdname coef.mvr
#' @export
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
#' @rdname coef.mvr
#' @export
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


#' @title Adjust for Missing Values
#'
#' @description Use missing value information to adjust residuals and predictions.  This is
#' the \sQuote{mvr equivalent} of the \code{naresid.exclude} and
#' \code{napredict.exclude} functions.
#'
#' @details This is a utility function used to allow \code{predict.mvr} and
#' \code{residuals.mvr} to compensate for the removal of \code{NA}s in the
#' fitting process.
#'
#' It is called only when the \code{na.action} is \code{na.exclude}, and pads
#' \code{x} with \code{NA}s in the correct positions to have the same number of
#' rows as the original data frame.
#'
#' @param omit an object produced by an \code{na.action} function, typically
#' the \code{"na.action"} attribute of the result of \code{na.omit} or
#' \code{na.exclude}.
#' @param x a three-dimensional array to be adjusted based upon the missing
#' value information in \code{omit}.
#' @param \dots further arguments.  Currently not used.
#' @return \code{x}, padded with \code{NA}s along the first dimension
#' (\sQuote{rows}).
#' @author Bjørn-Helge Mevik and Ron Wehrens
#' @seealso \code{\link{predict.mvr}}, \code{\link{residuals.mvr}},
#' \code{\link{napredict}}, \code{\link{naresid}}
#' @keywords regression multivariate internal
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
#' @name scores
#' @title Extract Scores and Loadings from PLSR and PCR Models
#'
#' @description These functions extract score and loading matrices from fitted \code{mvr}
#' models.
#'
#' @details All functions extract the indicated matrix from the fitted model, and will
#' work with any object having a suitably named component.
#'
#' The default \code{scores} and \code{loadings} methods also handle
#' \code{prcomp} objects (their scores and loadings components are called
#' \code{x} and \code{rotation}, resp.), and add an attribute \code{"explvar"}
#' with the variance explained by each component, if this is available.  (See
#' \code{\link{explvar}} for details.)
#'
#' @aliases scores scores.default loadings loadings.default loading.weights
#' Yscores Yloadings
#' @param object a fitted model to extract from.
#' @param \dots extra arguments, currently not used.
#' @return A matrix with scores or loadings.
#' @note There is a \code{loadings} function in package \pkg{stats}.  It simply
#' returns any element named \code{"loadings"}.  See
#' \code{\link[stats]{loadings}} for details.  The function can be accessed as
#' \code{stats::loadings(...)}.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{coef.mvr}}
#' @keywords regression multivariate
#' @examples
#'
#' data(yarn)
#' plsmod <- plsr(density ~ NIR, 6, data = yarn)
#' scores(plsmod)
#' loadings(plsmod)[,1:4]
#'
#' @export
loadings <- function(object, ...) UseMethod("loadings")
#' @rdname scores
#' @export
loadings.default <- function(object, ...) {
    L <- if (inherits(object, "prcomp")) object$rotation else object$loadings
    if (!(inherits(L, "loadings") || inherits(L, "list")))
        class(L) <- "loadings"
    attr(L, "explvar") <- explvar(object)
    L
}

## scores: Return the scores (also works for prcomp/princomp objects):
#' @rdname scores
#' @export
scores <- function(object, ...) UseMethod("scores")
#' @rdname scores
#' @export
scores.default <- function(object, ...) {
    S <- if (inherits(object, "prcomp")) object$x else object$scores
    if (!(inherits(S, "scores") || inherits(S, "list")))
        class(S) <- "scores"
    attr(S, "explvar") <- explvar(object)
    S
}

## Yscores: Return the Yscores
#' @rdname scores
#' @export
Yscores <- function(object) object$Yscores

## loading.weights: Return the loading weights:
#' @rdname scores
#' @export
loading.weights <- function(object) object$loading.weights

## Yloadings: Return the Yloadings
#' @rdname scores
#' @export
Yloadings <- function(object) object$Yloadings

## model.frame.mvr: Extract or generate the model frame from a `mvr' object.
## It is simply a slightly modified `model.frame.lm'.
#' @rdname coef.mvr
#' @export
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
#' @rdname coef.mvr
#' @export
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


#' @title Delete intercept from model matrix
#'
#' @description A utility function to delete any intercept column from a model matrix, and
#' adjust the \code{"assign"} attribute correspondingly.  It is used by formula
#' handling functions like \code{mvr} and \code{model.matrix.mvr}.
#'
#'
#' @param mm Model matrix.
#' @return A model matrix without intercept column.
#' @author Bjørn-Helge Mevik and Ron Wehrens
#' @seealso \code{\link{mvr}}, \code{\link{model.matrix.mvr}}
#' @keywords internal
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
#' @rdname coef.mvr
#' @export
respnames <- function(object)
    dimnames(fitted(object))[[2]]

## The names of the prediction variables:
#' @rdname coef.mvr
#' @export
prednames <- function(object, intercept = FALSE) {
    if (isTRUE(intercept))
        c("(Intercept)", rownames(object$loadings))
    else
        rownames(object$loadings)
}

## The names of the components:
## Note: The components must be selected prior to the format statement
#' @rdname coef.mvr
#' @export
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
#' @rdname coef.mvr
#' @export
explvar <- function(object)
    switch(class(object)[1],
           mvr = 100 * object$Xvar / object$Xtotvar,
           princomp =,
           prcomp = 100 * object$sdev^2 / sum(object$sdev^2),
           scores =,
           loadings = attr(object, "explvar")
           )
