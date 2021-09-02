### crossval.R: Cross-validation functions.

## The basic cross-validation function


#' @title Cross-validation
#'
#' @description Performs the cross-validation calculations for \code{mvr}.
#'
#' This function is not meant to be called directly, but through the generic
#' functions \code{pcr}, \code{plsr}, \code{cppls} or \code{mvr} with the
#' argument \code{validation} set to \code{"CV"} or \code{"LOO"}.  All
#' arguments to \code{mvrCv} can be specified in the generic function call.
#'
#' If \code{segments} is a list, the arguments \code{segment.type} and
#' \code{length.seg} are ignored.  The elements of the list should be integer
#' vectors specifying the indices of the segments.  See
#' \code{\link{cvsegments}} for details.
#'
#' Otherwise, segments of type \code{segment.type} are generated.  How many
#' segments to generate is selected by specifying the number of segments in
#' \code{segments}, or giving the segment length in \code{length.seg}.  If both
#' are specified, \code{segments} is ignored.
#'
#' If \code{jackknife} is \code{TRUE}, jackknifed regression coefficients are
#' returned, which can be used for for variance estimation
#' (\code{\link{var.jack}}) or hypothesis testing (\code{\link{jack.test}}).
#'
#' \code{X} and \code{Y} do not need to be centered.
#'
#' Note that this function cannot be used in situations where \eqn{X} needs to
#' be recalculated for each segment (except for scaling by the standard
#' deviation), for instance with \code{msc} or other preprocessing.  For such
#' models, use the more general (but slower) function \code{\link{crossval}}.
#'
#' Also note that if needed, the function will silently(!) reduce \code{ncomp}
#' to the maximal number of components that can be cross-validated, which is
#' \eqn{n - l - 1}, where \eqn{n} is the number of observations and \eqn{l} is
#' the length of the longest segment.  The (possibly reduced) number of
#' components is returned as the component \code{ncomp}.
#'
#' By default, the cross-validation will be performed serially.  However, it
#' can be done in parallel using functionality in the \code{\link{parallel}}
#' package by setting the option \code{parallel} in \code{\link{pls.options}}.
#' See \code{\link{pls.options}} for the different ways to specify the
#' parallelism.
#'
#' @param X a matrix of observations.  \code{NA}s and \code{Inf}s are not
#' allowed.
#' @param Y a vector or matrix of responses.  \code{NA}s and \code{Inf}s are
#' not allowed.
#' @param ncomp the number of components to be used in the modelling.
#' @param Y.add a vector or matrix of additional responses containing relevant
#' information about the observations.  Only used for \code{cppls}.
#' @param weights a vector of individual weights for the observations.  Only
#' used for \code{cppls}.  (Optional)
#' @param method the multivariate regression method to be used.
#' @param scale logical.  If \code{TRUE}, the learning \eqn{X} data for each
#' segment is scaled by dividing each variable by its sample standard
#' deviation.  The prediction data is scaled by the same amount.
#' @param segments the number of segments to use, or a list with segments (see
#' below).
#' @param segment.type the type of segments to use.  Ignored if \code{segments}
#' is a list.
#' @param length.seg Positive integer.  The length of the segments to use.  If
#' specified, it overrides \code{segments} unless \code{segments} is a list.
#' @param jackknife logical.  Whether jackknifing of regression coefficients
#' should be performed.
#' @param trace logical; if \code{TRUE}, the segment number is printed for each
#' segment.
#' @param \dots additional arguments, sent to the underlying fit function.
#' @return A list with the following components: \item{method}{equals
#' \code{"CV"} for cross-validation.} \item{pred}{an array with the
#' cross-validated predictions.} \item{coefficients}{(only if \code{jackknife}
#' is \code{TRUE}) an array with the jackknifed regression coefficients.  The
#' dimensions correspond to the predictors, responses, number of components,
#' and segments, respectively.} \item{PRESS0}{a vector of PRESS values (one for
#' each response variable) for a model with zero components, i.e., only the
#' intercept.} \item{PRESS}{a matrix of PRESS values for models with 1,
#' \ldots{}, \code{ncomp} components.  Each row corresponds to one response
#' variable.} \item{adj}{a matrix of adjustment values for calculating bias
#' corrected MSEP.  \code{MSEP} uses this.} \item{segments}{the list of
#' segments used in the cross-validation.} \item{ncomp}{the actual number of
#' components used.} \item{gamma}{if method \code{cppls} is used, gamma values
#' for the powers of each CV segment are returned.}
#' @note The \code{PRESS0} is always cross-validated using leave-one-out
#' cross-validation.  This usually makes little difference in practice, but
#' should be fixed for correctness.
#'
#' The current implementation of the jackknife stores all jackknife-replicates
#' of the regression coefficients, which can be very costly for large matrices.
#' This might change in a future version.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}} \code{\link{crossval}} \code{\link{cvsegments}}
#' \code{\link{MSEP}} \code{\link{var.jack}} \code{\link{jack.test}}
#' @references Mevik, B.-H., Cederkvist, H. R. (2004) Mean Squared Error of
#' Prediction (MSEP) Estimates for Principal Component Regression (PCR) and
#' Partial Least Squares Regression (PLSR).  \emph{Journal of Chemometrics},
#' \bold{18}(9), 422--429.
#' @keywords regression multivariate
#' @examples
#'
#' data(yarn)
#' yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV", segments = 10)
#' \dontrun{plot(MSEP(yarn.pcr))}
#'
mvrCv <- function(X, Y, ncomp, Y.add = NULL, weights = NULL,
                  method = pls.options()$mvralg,
                  scale = FALSE, segments = 10,
                  segment.type = c("random", "consecutive", "interleaved"),
                  length.seg, jackknife = FALSE, trace = FALSE, ...)
{
    ## Initialise:
    Y <- as.matrix(Y)
    if (!(missing(Y.add) || is.null(Y.add)))
        Y.add <- as.matrix(Y.add)

    ## Save dimnames:
    dnX <- dimnames(X)
    dnY <- dimnames(Y)

    ## Remove dimnames for performance (doesn't seem to matter; in fact,
    ## as far as it has any effect, it hurts a tiny bit in most situations).
    ## dimnames(X) <- dimnames(Y) <- NULL

    ## Save dimensions:
    nobj  <- dim(X)[1]
    npred <- dim(X)[2]
    nresp <- dim(Y)[2]

    ## Check the `scale' parameter:
    if (!is.logical(scale) || length(scale) != 1)
        stop("'scale' must be 'TRUE' or 'FALSE'")

    ## Set up segments:
    if (is.list(segments)) {
        if (is.null(attr(segments, "type")))
            attr(segments, "type") <- "user supplied"
    } else {
        if (missing(length.seg)) {
            segments <- cvsegments(nobj, k = segments, type = segment.type)
        } else {
            segments <- cvsegments(nobj, length.seg = length.seg,
                                   type = segment.type)
        }
    }

    ## Reduce ncomp, if neccessary:
    ncomp <- min(ncomp, nobj - max(sapply(segments, length)) - 1)

    ## Select fit function:
    method <- match.arg(method,c("kernelpls", "widekernelpls", "simpls",
                                 "oscorespls", "cppls", "svdpc"))
    fitFunc <- switch(method,
                      kernelpls = kernelpls.fit,
                      widekernelpls = widekernelpls.fit,
                      simpls = simpls.fit,
                      oscorespls = oscorespls.fit,
                      cppls = cppls.fit,
                      svdpc = svdpc.fit)

    ## Helper function to perform the cross-validatoin for one segment.
    ## Defined inside mvrCv to be able to access local variables:
    mvrCvSeg <- function(n.seg) {
        if (trace) cat(n.seg, "")

        ## Set up train data:
        seg <- segments[[n.seg]]
        Xtrain <- X[-seg,, drop=FALSE]
        if (scale) {
            ntrain <- nrow(Xtrain)
            ## This is faster than sd(X), but cannot handle missing values:
            sdtrain <-
                sqrt(colSums((Xtrain - rep(colMeans(Xtrain), each = ntrain))^2) /
                     (ntrain - 1))
            if (any(abs(sdtrain) < .Machine$double.eps^0.5))
                warning("Scaling with (near) zero standard deviation")
            Xtrain <- Xtrain / rep(sdtrain, each = ntrain)
        }

        ## Fit the model:
        fit <- fitFunc(Xtrain, Y[-seg,, drop=FALSE], ncomp,
                       Y.add = Y.add[-seg,, drop=FALSE], stripped = TRUE,
                       weights = weights[-seg], ...)

        ## Set up test data:
        Xtest <- X
        if (scale) Xtest <- Xtest / rep(sdtrain, each = nobj)
        Xtest <- Xtest - rep(fit$Xmeans, each = nobj)

        ## Predict test data:
        pred <- array(0, dim = c(nobj, nresp, ncomp))
        Ymeansrep <- rep(fit$Ymeans, each = nobj)
        for (a in 1:ncomp)
            pred[,,a] <- Xtest %*% fit$coefficients[,,a] + Ymeansrep

        return(list(adj = length(seg) * colSums((pred - c(Y))^2),
                    cvPred = pred[seg,,, drop=FALSE],
                    gammas = if (method == "cppls") fit$gammas else NULL,
                    cvCoef = if (jackknife) fit$coefficients else NULL
                    ))
    }

    ## Perform the cross-validation, optionally in parallel:
    if (trace) cat("Segment: ")
    results <- lapplyFunc(pls.options()$parallel, seq_along(segments), mvrCvSeg)
    if (trace) cat("\n")

    ## Variables to save CV results in:
    adj <- matrix(0, nrow = nresp, ncol = ncomp)
    cvPred <- array(0, dim = c(nobj, nresp, ncomp))
    if (jackknife)
        cvCoef <- array(dim = c(npred, nresp, ncomp, length(segments)))
    if (method == "cppls") gammas <- list()

    ## Collect the results:
    for (n.seg in seq_along(segments)) {
        res <- results[[n.seg]]
        adj <- adj + res$adj
        cvPred[segments[[n.seg]],,] <- res$cvPred
        if (jackknife) cvCoef[,,,n.seg] <- res$cvCoef
        if (method == "cppls") gammas[[n.seg]] <- res$gammas
    }

    ## Calculate validation statistics:
    PRESS0 <- apply(Y, 2, var) * nobj^2 / (nobj - 1) # FIXME: Only correct for loocv!
    PRESS <- colSums((cvPred - c(Y))^2)

    ## Add dimnames:
    objnames <- dnX[[1]]
    if (is.null(objnames)) objnames <- dnY[[1]]
    respnames <- dnY[[2]]
    nCompnames <- paste(1:ncomp, "comps")
    names(PRESS0) <- respnames
    dimnames(adj) <- dimnames(PRESS) <-
        list(respnames, nCompnames)
    dimnames(cvPred) <- list(objnames, respnames, nCompnames)
    if (jackknife)
        dimnames(cvCoef) <- list(dnX[[2]], respnames, nCompnames,
                                 paste("Seg", seq_along(segments)))

    list(method = "CV", pred = cvPred, coefficients = if (jackknife) cvCoef,
         gammas = if (method == "cppls") gammas,
         PRESS0 = PRESS0, PRESS = PRESS, adj = adj / nobj^2,
         segments = segments, ncomp = ncomp)
}


## Genereral cross-validation function.


#' @title Cross-validation of PLSR and PCR models
#'
#' @description A \dQuote{stand alone} cross-validation function for \code{mvr} objects.
#'
#' @details This function performs cross-validation on a model fit by \code{mvr}.  It
#' can handle models such as \code{plsr(y ~ msc(X), \dots{})} or other models
#' where the predictor variables need to be recalculated for each segment.
#' When recalculation is not needed, the result of
#' \code{crossval(mvr(\dots{}))} is identical to \code{mvr(\dots{}, validation
#' = "CV")}, but slower.
#'
#' Note that to use \code{crossval}, the data \emph{must} be specified with a
#' \code{data} argument when fitting \code{object}.
#'
#' If \code{segments} is a list, the arguments \code{segment.type} and
#' \code{length.seg} are ignored.  The elements of the list should be integer
#' vectors specifying the indices of the segments.  See
#' \code{\link{cvsegments}} for details.
#'
#' Otherwise, segments of type \code{segment.type} are generated.  How many
#' segments to generate is selected by specifying the number of segments in
#' \code{segments}, or giving the segment length in \code{length.seg}.  If both
#' are specified, \code{segments} is ignored.
#'
#' If \code{jackknife} is \code{TRUE}, jackknifed regression coefficients are
#' returned, which can be used for for variance estimation
#' (\code{\link{var.jack}}) or hypothesis testing (\code{\link{jack.test}}).
#'
#' When tracing is turned on, the segment number is printed for each segment.
#'
#' By default, the cross-validation will be performed serially.  However, it
#' can be done in parallel using functionality in the \code{\link{parallel}}
#' package by setting the option \code{parallel} in \code{\link{pls.options}}.
#' See \code{\link{pls.options}} for the different ways to specify the
#' parallelism.  See also Examples below.
#'
#' @param object an \code{mvr} object; the regression to cross-validate.
#' @param segments the number of segments to use, or a list with segments (see
#' below).
#' @param segment.type the type of segments to use.  Ignored if \code{segments}
#' is a list.
#' @param length.seg Positive integer.  The length of the segments to use.  If
#' specified, it overrides \code{segments} unless \code{segments} is a list.
#' @param jackknife logical.  Whether jackknifing of regression coefficients
#' should be performed.
#' @param trace if \code{TRUE}, tracing is turned on.  If numeric, it denotes a
#' time limit (in seconds).  If the estimated total time of the
#' cross-validation exceeds this limit, tracing is turned on.
#' @param \dots additional arguments, sent to the underlying fit function.
#' @return The supplied \code{object} is returned, with an additional component
#' \code{validation}, which is a list with components \item{method}{euqals
#' \code{"CV"} for cross-validation.} \item{pred}{an array with the
#' cross-validated predictions.} \item{coefficients}{(only if \code{jackknife}
#' is \code{TRUE}) an array with the jackknifed regression coefficients.  The
#' dimensions correspond to the predictors, responses, number of components,
#' and segments, respectively.} \item{PRESS0}{a vector of PRESS values (one for
#' each response variable) for a model with zero components, i.e., only the
#' intercept.} \item{PRESS}{a matrix of PRESS values for models with 1,
#' \ldots{}, \code{ncomp} components.  Each row corresponds to one response
#' variable.} \item{adj}{a matrix of adjustment values for calculating bias
#' corrected MSEP.  \code{MSEP} uses this.} \item{segments}{the list of
#' segments used in the cross-validation.} \item{ncomp}{the number of
#' components.} \item{gammas}{if method \code{cppls} is used, gamma values for
#' the powers of each CV segment are returned.}
#' @note The \code{PRESS0} is always cross-validated using leave-one-out
#' cross-validation.  This usually makes little difference in practice, but
#' should be fixed for correctness.
#'
#' The current implementation of the jackknife stores all jackknife-replicates
#' of the regression coefficients, which can be very costly for large matrices.
#' This might change in a future version.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}} \code{\link{mvrCv}} \code{\link{cvsegments}}
#' \code{\link{MSEP}} \code{\link{var.jack}} \code{\link{jack.test}}
#' @references Mevik, B.-H., Cederkvist, H. R. (2004) Mean Squared Error of
#' Prediction (MSEP) Estimates for Principal Component Regression (PCR) and
#' Partial Least Squares Regression (PLSR).  \emph{Journal of Chemometrics},
#' \bold{18}(9), 422--429.
#' @keywords regression multivariate
#' @examples
#'
#' data(yarn)
#' yarn.pcr <- pcr(density ~ msc(NIR), 6, data = yarn)
#' yarn.cv <- crossval(yarn.pcr, segments = 10)
#' \dontrun{plot(MSEP(yarn.cv))}
#'
#' \dontrun{
#' ## Parallelised cross-validation, using transient cluster:
#' pls.options(parallel = 4) # use mclapply (not available on Windows)
#' pls.options(parallel = quote(parallel::makeCluster(4, type = "PSOCK"))) # parLapply
#' ## A new cluster is created and stopped for each cross-validation:
#' yarn.cv <- crossval(yarn.pcr)
#' yarn.loocv <- crossval(yarn.pcr, length.seg = 1)
#'
#' ## Parallelised cross-validation, using persistent cluster:
#' library(parallel)
#' ## This creates the cluster:
#' pls.options(parallel = makeCluster(4, type = "FORK")) # not available on Windows
#' pls.options(parallel = makeCluster(4, type = "PSOCK"))
#' ## The cluster can be used several times:
#' yarn.cv <- crossval(yarn.pcr)
#' yarn.loocv <- crossval(yarn.pcr, length.seg = 1)
#' ## The cluster should be stopped manually afterwards:
#' stopCluster(pls.options()$parallel)
#'
#' ## Parallelised cross-validation, using persistent MPI cluster:
#' ## This requires the packages snow and Rmpi to be installed
#' library(parallel)
#' ## This creates the cluster:
#' pls.options(parallel = makeCluster(4, type = "MPI"))
#' ## The cluster can be used several times:
#' yarn.cv <- crossval(yarn.pcr)
#' yarn.loocv <- crossval(yarn.pcr, length.seg = 1)
#' ## The cluster should be stopped manually afterwards:
#' stopCluster(pls.options()$parallel)
#' ## It is good practice to call mpi.exit() or mpi.quit() afterwards:
#' mpi.exit()
#' }
#'
#' @export
crossval <- function(object, segments = 10,
                     segment.type = c("random", "consecutive", "interleaved"),
                     length.seg, jackknife = FALSE, trace = 15, ...)
{
    if (!inherits(object, "mvr")) stop("`object' not an mvr object.")
    ## Get data frame
    fitCall <- object$call
    data <- eval(fitCall$data, parent.frame())
    if (is.null(data)) stop("`object' must be fit with a `data' argument.")
    ## Optionally get weights
    if (cppls <- (object$method == "cppls")) {
        weights <- eval(fitCall$weights, parent.frame())
    }
    else weights <- NULL

    if (!is.null(fitCall$subset)) {
        ## Handle "subset" argument
        data <- data[eval(fitCall$subset, parent.frame()),]
        object$call$subset <- NULL
    }

    ## Handle NAs (according to na.action)
    if (is.na(match("na.action", names(fitCall)))) {
        ## Cannot use is.null(fitCall$na.action) here, since the meaning of
        ## `na.action = NULL' is not the same as that of a missing na.action
        ## argument.
        mf <- model.frame(formula(object), data = data)
    } else {
        mf <- model.frame(formula(object), data = data,
                          na.action = fitCall$na.action)
    }
    if (!is.null(NAs <- attr(mf, "na.action"))) {
        ## Some observations were dropped due to NAs.  Skip the same in data:
        data <- data[-NAs,]
    }

    ## Get response:
    Y <- as.matrix(model.response(mf))
    nresp <- dim(Y)[2]
    npred <- length(object$Xmeans)
    ## Calculate effective number of observations
    nobj <- nrow(data)

    ## Set up segments
    if (is.list(segments)) {
        if (is.null(attr(segments, "type")))
            attr(segments, "type") <- "user supplied"
    } else {
        if (missing(length.seg)) {
            segments <- cvsegments(nobj, k = segments, type = segment.type)
        } else {
            segments <- cvsegments(nobj, length.seg = length.seg,
                                   type = segment.type)
        }
    }

    jackknife <- isTRUE(jackknife)
    ncomp <- object$ncomp
    if (ncomp > nobj - max(sapply(segments, length)) - 1)
        stop("`ncomp' too large for cross-validation.",
             "\nPlease refit with `ncomp' less than ",
             nobj - max(sapply(segments, length)))

    ## Optionally turn on tracing:
    if (is.numeric(trace)) {
        trace <- object$fit.time * length(segments) > trace
    }

    ## Helper function to perform the cross-validatoin for one segment.
    ## Defined inside crossval to be able to access local variables:
    crossvalSeg <- function(n.seg) {
        if (trace) cat(n.seg, "")

        ## Run cv, using update and predict
        seg <- segments[[n.seg]]
        fit <- update(object, data = data[-seg,], weights = weights[-seg])
        pred <- predict(fit, newdata = data)

        return(list(adj = length(seg) * colSums((pred - c(Y))^2),
                    cvPred = pred[seg,,, drop=FALSE],
                    gammas = if (cppls) fit$gammas else NULL,
                    cvCoef = if (jackknife) fit$coefficients else NULL
                    ))
    }

    ## Perform the cross-validation, optionally in parallel:
    if (trace) cat("Segment: ")
    results <- lapplyFunc(pls.options()$parallel,
                          seq_along(segments), crossvalSeg,
                          quote(parallel::clusterCall(parSpec, library, "pls",
                                                      character.only = TRUE,
                                                      warn.conflicts = FALSE)))
    if (trace) cat("\n")

    ## Variables to save CV results in:
    cvPred <- array(dim = c(nobj, nresp, ncomp))
    adj <- matrix(0, nrow = nresp, ncol = ncomp)
    if (jackknife)
        cvCoef <- array(dim = c(npred, nresp, ncomp, length(segments)))
    if (cppls) gammas <- list()

    ## Collect the results:
    for (n.seg in seq_along(segments)) {
        res <- results[[n.seg]]
        adj <- adj + res$adj
        cvPred[segments[[n.seg]],,] <- res$cvPred
        if (jackknife) cvCoef[,,,n.seg] <- res$cvCoef
        if (cppls) gammas[[n.seg]] <- res$gammas
    }

    ## Calculate validation statistics:
    PRESS0 <- apply(Y, 2, var) * nobj^2 / (nobj - 1) # FIXME: Only correct for loocv!
    PRESS <- colSums((cvPred - c(Y))^2)

    ## Add dimnames:
    objnames <- rownames(data)
    if (is.null(objnames)) objnames <- rownames(Y)
    dimnames(cvPred) <- c(list(objnames), dimnames(fitted(object))[-1])
    if (is.null(names(PRESS0))) names(PRESS0) <- dimnames(object$Yloadings)[[1]]
    dimnames(PRESS) <- dimnames(adj)
    if (jackknife)
        dimnames(cvCoef) <- c(dimnames(coef(object)),
                              list(paste("Seg", seq_along(segments))))

    ## Return the original object, with a component `validation' added
    object$validation <- list(method = "CV", pred = cvPred,
                              coefficients = if (jackknife) cvCoef,
                              gammas = if (cppls) gammas,
                              PRESS0 = PRESS0, PRESS = PRESS,
                              adj = adj / nobj^2,
                              segments = segments, ncomp = ncomp)
    return(object)
}

## Internal function to apply FUN over X, optionally in parallel:
lapplyFunc <- function(parSpec, X, FUN, nonForkInit) {
    if (is.null(parSpec) || (is.numeric(parSpec) && parSpec == 1)) {
        ## Serially
        results <- lapply(X, FUN)
    } else {
        ## Parallel
        stop_cluster <- FALSE           # Whether to kill the workers afterwards

        if (is.numeric(parSpec) && parSpec > 1) {
            ## Number => number of workers with mclapply
            results <- parallel::mclapply(X, FUN, mc.cores = parSpec)
        } else {
            if (is.call(parSpec)) {
                ## Unevaluated call => evaluate it to create the cluster:
                parSpec <- eval(parSpec)
                stop_cluster <- TRUE
            }

            if (inherits(parSpec, "cluster")) {
                ## Run library(pls) on cluster if type != FORK
                if (!inherits(parSpec[[1]], "forknode")
                    && !missing(nonForkInit)) {
                    eval(nonForkInit)
                }
                results <- parallel::parLapply(parSpec, X, FUN)

                if (stop_cluster) {
                    parallel::stopCluster(parSpec)
                }
            } else {
                stop("Unknown parallelity specification: '", parSpec, "'")
            }
        }
    }

    return(results)
}
