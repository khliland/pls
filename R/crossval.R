### crossval.R: Cross-validation functions.
### $Id$


## The basic cross-validation function
mvrCv <- function(X, Y, ncomp, Y.add = NULL, weights = NULL,
                  method = pls.options()$mvralg,
                  scale = FALSE, segments = 10,
                  segment.type = c("random", "consecutive", "interleaved"),
                  length.seg, jackknife = FALSE, trace = FALSE, ...)
{
    ## Initialise:
    Y <- as.matrix(Y)
    if(!(missing(Y.add) || is.null(Y.add)))
        Y.add <- as.matrix(Y.add)

    ## Save dimnames:
    dnX <- dimnames(X)
    dnY <- dimnames(Y)

    ## Remove dimnames for performance (doesn't seem to matter; in fact,
    ## as far as it has any effect, it hurts a tiny bit in most situations).
    ## dimnames(X) <- dimnames(Y) <- NULL

    ## Save dimensions:
    nobj <- dim(X)[1]
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
crossval <- function(object, segments = 10,
                     segment.type = c("random", "consecutive", "interleaved"),
                     length.seg, jackknife = FALSE, trace = 15, ...)
{
    if(!inherits(object, "mvr")) stop("`object' not an mvr object.")
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
    if(!is.null(NAs <- attr(mf, "na.action"))) {
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
