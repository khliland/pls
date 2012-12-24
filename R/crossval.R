### crossval.R: Genereral cross-validation function.
### $Id$

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

    if (trace) cat("Segment: ")

    ## Perform the cross-validation, optionally in parallel:
    clspec <- pls.options()$parallel
    if (is.null(clspec) || (is.numeric(clspec) && clspec == 1)) {
        ## Serially
        results <- lapply(seq_along(segments), crossvalSeg)
    } else {
        ## Parallel
        require(parallel, warn.conflicts = FALSE)
        stop_cluster <- FALSE           # Whether to kill the workers afterwards

        if (is.numeric(clspec) && clspec > 1) {
            ## Number => number of workers with mclapply
            results <- mclapply(seq_along(segments), crossvalSeg,
                                mc.cores = clspec)
        } else {
            if (is.call(clspec)) {
                ## Unevaluated call => evaluate it to create the cluster:
                clspec <- eval(clspec)
                stop_cluster <- TRUE
            }

            if (inherits(clspec, "cluster")) {
                ## Run library(pls) on cluster if type != FORK
                if (!inherits(clspec[[1]], "forknode")) {
                    clusterCall(clspec, library, "pls", character.only = TRUE,
                                warn.conflicts = FALSE)
                }
                results <- parLapply(clspec, seq_along(segments), crossvalSeg)

                if (stop_cluster) {
                    stopCluster(clspec)
                }
            } else {
                stop("Unknown cluster specification: '", clspec, "'")
            }
        }
    }

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
                              list(paste("Seg", seq.int(along = segments))))

    ## Return the original object, with a component `validation' added
    object$validation <- list(method = "CV", pred = cvPred,
                              coefficients = if (jackknife) cvCoef,
                              gammas = if (cppls) gammas,
                              PRESS0 = PRESS0, PRESS = PRESS,
                              adj = adj / nobj^2,
                              segments = segments, ncomp = ncomp)
    return(object)
}
