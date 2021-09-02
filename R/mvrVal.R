### mvrVal.R: Functions for calculating validation statistics, such
### as MSEP, RMSEP and R2, for mvr objects.

## Calculate the validation statistics needed for (R)MSEP and R^2.
## Note that it accepts any values for `estimate', but only calculates
## statistics for "train", "test" and "CV".

#' @name mvrVal
#' @title MSEP, RMSEP and R2 of PLSR and PCR models
#'
#' @description Functions to estimate the mean squared error of prediction (MSEP), root mean
#' squared error of prediction (RMSEP) and \eqn{R^2} (A.K.A. coefficient of
#' multiple determination) for fitted PCR and PLSR models.  Test-set,
#' cross-validation and calibration-set estimates are implemented.
#'
#' @details \code{RMSEP} simply calls \code{MSEP} and takes the square root of the
#' estimates.  It therefore accepts the same arguments as \code{MSEP}.
#'
#' Several estimators can be used.  \code{"train"} is the training or
#' calibration data estimate, also called (R)MSEC.  For \code{R2}, this is the
#' unadjusted \eqn{R^2}.  It is overoptimistic and should not be used for
#' assessing models.  \code{"CV"} is the cross-validation estimate, and
#' \code{"adjCV"} (for \code{RMSEP} and \code{MSEP}) is the bias-corrected
#' cross-validation estimate.  They can only be calculated if the model has
#' been cross-validated.  Finally, \code{"test"} is the test set estimate,
#' using \code{newdata} as test set.
#'
#' Which estimators to use is decided as follows (see below for
#' \code{mvrValstats}).  If \code{estimate} is not specified, the test set
#' estimate is returned if \code{newdata} is specified, otherwise the CV and
#' adjusted CV (for \code{RMSEP} and \code{MSEP}) estimates if the model has
#' been cross-validated, otherwise the training data estimate.  If
#' \code{estimate} is \code{"all"}, all possible estimates are calculated.
#' Otherwise, the specified estimates are calculated.
#'
#' Several model sizes can also be specified.  If \code{comps} is missing (or
#' is \code{NULL}), \code{length(ncomp)} models are used, with \code{ncomp[1]}
#' components, \ldots{}, \code{ncomp[length(ncomp)]} components.  Otherwise, a
#' single model with the components \code{comps[1]}, \ldots{},
#' \code{comps[length(comps)]} is used.  If \code{intercept} is \code{TRUE}, a
#' model with zero components is also used (in addition to the above).
#'
#' The \eqn{R^2} values returned by \code{"R2"} are calculated as \eqn{1 -
#' SSE/SST}, where \eqn{SST} is the (corrected) total sum of squares of the
#' response, and \eqn{SSE} is the sum of squared errors for either the fitted
#' values (i.e., the residual sum of squares), test set predictions or
#' cross-validated predictions (i.e., the \eqn{PRESS}).  For \code{estimate =
#' "train"}, this is equivalent to the squared correlation between the fitted
#' values and the response.  For \code{estimate = "train"}, the estimate is
#' often called the prediction \eqn{R^2}.
#'
#' \code{mvrValstats} is a utility function that calculates the statistics
#' needed by \code{MSEP} and \code{R2}.  It is not intended to be used
#' interactively.  It accepts the same arguments as \code{MSEP} and \code{R2}.
#' However, the \code{estimate} argument must be specified explicitly: no
#' partial matching and no automatic choice is made.  The function simply
#' calculates the types of estimates it knows, and leaves the other untouched.
#'
#' @aliases MSEP MSEP.mvr RMSEP RMSEP.mvr R2 R2.mvr mvrValstats
#' @param object an \code{mvr} object
#' @param estimate a character vector.  Which estimators to use.  Should be a
#' subset of \code{c("all", "train", "CV", "adjCV", "test")}.  \code{"adjCV"}
#' is only available for (R)MSEP.  See below for how the estimators are chosen.
#' @param newdata a data frame with test set data.
#' @param ncomp,comps a vector of positive integers.  The components or number
#' of components to use.  See below.
#' @param intercept logical.  Whether estimates for a model with zero
#' components should be returned as well.
#' @param se logical.  Whether estimated standard errors of the estimates
#' should be calculated.  Not implemented yet.
#' @param \dots further arguments sent to underlying functions or (for
#' \code{RMSEP}) to \code{MSEP}
#' @section Value: \code{mvrValstats} returns a list with components \describe{
#' \item{SSE}{three-dimensional array of SSE values.  The first dimension is
#' the different estimators, the second is the response variables and the third
#' is the models.} \item{SST}{matrix of SST values.  The first dimension is the
#' different estimators and the second is the response variables.}
#' \item{nobj}{a numeric vector giving the number of objects used for each
#' estimator.} \item{comps}{the components specified, with \code{0} prepended
#' if \code{intercept} is \code{TRUE}.} \item{cumulative}{\code{TRUE} if
#' \code{comps} was \code{NULL} or not specified.} }
#'
#' The other functions return an object of class \code{"mvrVal"}, with
#' components \describe{ \item{val}{three-dimensional array of estimates.  The
#' first dimension is the different estimators, the second is the response
#' variables and the third is the models.} \item{type}{\code{"MSEP"},
#' \code{"RMSEP"} or \code{"R2"}.} \item{comps}{the components specified, with
#' \code{0} prepended if \code{intercept} is \code{TRUE}.}
#' \item{cumulative}{\code{TRUE} if \code{comps} was \code{NULL} or not
#' specified.} \item{call}{the function call} }
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{crossval}}, \code{\link{mvrCv}},
#' \code{\link{validationplot}}, \code{\link{plot.mvrVal}}
#' @references Mevik, B.-H., Cederkvist, H. R. (2004) Mean Squared Error of
#' Prediction (MSEP) Estimates for Principal Component Regression (PCR) and
#' Partial Least Squares Regression (PLSR).  \emph{Journal of Chemometrics},
#' \bold{18}(9), 422--429.
#' @keywords regression multivariate
#' @examples
#'
#' data(oliveoil)
#' mod <- plsr(sensory ~ chemical, ncomp = 4, data = oliveoil, validation = "LOO")
#' RMSEP(mod)
#' \dontrun{plot(R2(mod))}
#'
#' @export
mvrValstats <- function(object, estimate,
                        newdata, ncomp = 1:object$ncomp, comps,
                        intercept = cumulative, se = FALSE, ...)
{
    ## Makes the code slightly simpler:
    cumulative <- missing(comps) || is.null(comps)

    if (any(estimate == "CV")) {
        ## Check that cross-validation is possible:
        if (!cumulative)
            stop("Cross-validation is not supported when `comps' is specified")
        if (is.null(object$validation))
            stop("`object' has no `validation' component")
    }

    ## The calculated stuff:
    nestimates <- length(estimate)
    nresp <- dim(fitted(object))[2]
    respnames <- dimnames(fitted(object))[[2]]
    SSE <- array(dim = c(nestimates, nresp,
                         if(cumulative) 1 + length(ncomp) else 2),
                 dimnames = list(estimate = estimate,
                 response = respnames,
                 model = if (cumulative) {
                     c("(Intercept)", paste(ncomp, "comps"))
                 } else {
                     c("(Intercept)", paste("(Intercept), Comp",
                                            paste(comps, collapse = ", ")))
                 }
                 ))
    SST <- array(dim = c(nestimates, nresp),
                 dimnames = list(estimate = estimate, response = respnames))
    nobj <- numeric(nestimates)
    names(nobj) <- estimate

    ## Calculate the statistics:
    for (i in seq(along = estimate)) {
        switch(estimate[i],
               train = {
                   resp <- as.matrix(model.response(model.frame(object)))
                   nobj[i] <- nrow(resp)
                   if (inherits(object$na.action, "exclude")) {
                       resp <- napredict(object$na.action, resp) # inserts NAs
                   }
                   res <- if (cumulative)
                       residuals(object, ...)[,,ncomp, drop=FALSE]
                   else
                       resp - predict(object, comps = comps, ...)

                   SST[i,] <- apply(resp, 2, var, na.rm = TRUE) *
                       (nobj[i] - 1)
                   SSE[i,,] <- cbind(SST[i,], colSums(res^2, na.rm = TRUE))
               },
               test = {
                   if (missing(newdata)) stop("Missing `newdata'.")
                   ## Remove any observations with NAs:
                   newdata <- model.frame(formula(object), data = newdata)
                   resp <- as.matrix(model.response(newdata))
                   pred <- if (cumulative)
                       predict(object, ncomp = ncomp, newdata = newdata,...)
                   else
                       predict(object, comps = comps, newdata = newdata,...)
                   nobj[i] <- nrow(newdata)
                   SST[i,] <- apply(resp, 2, var) * (nobj[i] - 1)
                   SSE[i,,] <- cbind(colSums(sweep(resp, 2, object$Ymeans)^2),
                                    colSums((pred - c(resp))^2))
               },
               CV = {
                   resp <- as.matrix(model.response(model.frame(object)))
                   nobj[i] <- nrow(resp)
                   SST[i,] <- apply(resp, 2, var) * (nobj[i] - 1)
                   SSE[i,,] <-
                       cbind(object$validation$PRESS0,
                             object$validation$PRESS[,ncomp, drop=FALSE])
               }
               )
    }

    if (cumulative) comps <- ncomp
    ## Either remove the intercept or add a "zeroth" component:
    if (isTRUE(intercept))
        comps <- c(0, comps)
    else
        SSE <- SSE[,,-1, drop=FALSE]

    return(list(SSE = SSE, SST = SST, nobj = nobj, comps = comps,
                cumulative = cumulative))
}


## R2: Return R^2
#' @rdname mvrVal
#' @export
R2 <- function(object, ...) UseMethod("R2")
#' @rdname mvrVal
#' @export
R2.mvr <- function(object, estimate, newdata, ncomp = 1:object$ncomp, comps,
                   intercept = cumulative, se = FALSE, ...)
{
    ## Makes the code slightly simpler:  FIXME: maybe remove
    cumulative <- missing(comps) || is.null(comps)

    ## Figure out which estimate(s) to calculate:
    allEstimates <- c("all", "train", "CV", "test")
    if (missing(estimate)) {
        ## Select the `best' available estimate
        if (!missing(newdata)) {
            estimate = "test"
        } else {
            if (!is.null(object$validation)) {
                estimate = "CV"
            } else {
                estimate = "train"
            }
        }
    } else {
        estimate <- allEstimates[pmatch(estimate, allEstimates)]
        if (any(is.na(estimate)))
            stop("`estimate' should be a subset of ",
                 paste(allEstimates, collapse = ", "))
        if (any(estimate == "all")) {
            estimate <- allEstimates[-1] # Try all estimates (except "all")
            if (missing(newdata))
                estimate <- setdiff(estimate, "test")
            if (is.null(object$validation) || !cumulative)
                estimate <- setdiff(estimate, "CV")
        }
    }

    ## Get the needed validation statistics:
    cl <- match.call(expand.dots = FALSE)
    cl$estimate <- estimate             # update estimate argument
    cl[[1]] <- as.name("mvrValstats")
    valstats <- eval(cl, parent.frame())

    ## Calculate the R^2s:
    R2 <- 1 - valstats$SSE / c(valstats$SST)

    return(structure(list(val = R2, type = "R2", comps = valstats$comps,
                          cumulative = valstats$cumulative, call = match.call()),
                     class = "mvrVal"))
}


## MSEP: Return MSEP
#' @rdname mvrVal
#' @export
MSEP <- function(object, ...) UseMethod("MSEP")
#' @rdname mvrVal
#' @export
MSEP.mvr <- function(object, estimate, newdata, ncomp = 1:object$ncomp, comps,
                       intercept = cumulative, se = FALSE, ...)
{
    ## Makes the code slightly simpler:
    cumulative <- missing(comps) || is.null(comps)

    ## Figure out which estimate(s) to calculate:
    allEstimates <- c("all", "train", "CV", "adjCV", "test")
    if (missing(estimate)) {
        ## Select the `best' available estimate
        if (!missing(newdata)) {
            estimate = "test"
        } else {
            if (!is.null(object$validation)) {
                estimate = c("CV", "adjCV")
            } else {
                estimate = "train"
            }
        }
    } else {
        estimate <- allEstimates[pmatch(estimate, allEstimates)]
        if (any(is.na(estimate)))
            stop("`estimate' should be a subset of ",
                 paste(allEstimates, collapse = ", "))
        if (any(estimate == "all")) {
            estimate <- allEstimates[-1] # Try all estimates (except "all")
            if (missing(newdata))
                estimate <- setdiff(estimate, "test")
            if (is.null(object$validation) || !cumulative)
                estimate <- setdiff(estimate, c("CV", "adjCV"))
        }
    }

    ## adjCV needs the statistics for CV and train, so we optionally
    ## have to add them:
    if (adjCV <- any(estimate == "adjCV")) {
        ## Note: this removes any duplicate elements
        calcestimates <- union(estimate, c("train", "CV"))
    } else {
        calcestimates <- estimate
    }
    ## Get the needed validation statistics:
    cl <- match.call(expand.dots = FALSE)
    cl$estimate <- calcestimates        # update estimate argument
    cl[[1]] <- as.name("mvrValstats")
    valstats <- eval(cl, parent.frame())

    ## Calculate the MSEPs:
    MSEP <- valstats$SSE / valstats$nobj
    if (adjCV) {
        ## Calculate the adjusted CV
        MSEP["adjCV",,] <- MSEP["CV",,]
        if (isTRUE(intercept)) {
            MSEP["adjCV",,-1] <- MSEP["adjCV",,-1] + MSEP["train",,-1] -
                object$validation$adj[,ncomp]
        } else {
            MSEP["adjCV",,] <- MSEP["adjCV",,] + MSEP["train",,] -
                object$validation$adj[,ncomp]
        }
        ## Remove any specially added estimates (this also adds back any
        ## duplicate elements):
        MSEP <- MSEP[estimate,,, drop=FALSE]
    }

    return(structure(list(val = MSEP, type = "MSEP", comps = valstats$comps,
                          cumulative = valstats$cumulative, call = match.call()),
                     class = "mvrVal"))
}

# RMSEP: A wrapper around MSEP to calculate RMSEPs
#' @rdname mvrVal
#' @export
RMSEP <- function(object, ...) UseMethod("RMSEP")
#' @rdname mvrVal
#' @export
RMSEP.mvr <- function(object, ...) {
    cl <- match.call()
    cl[[1]] <- as.name("MSEP")
    z <- eval(cl, parent.frame())
    z$val <- sqrt(z$val)
    z$type <- "RMSEP"
    z$call[[1]] <- as.name("RMSEP")
    z
}
