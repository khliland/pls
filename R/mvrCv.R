### mvrCv.R: The basic cross-validation function
### $Id$

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
    method <- match.arg(method,c("kernelpls", "widekernelpls", "simpls", "oscorespls", "cppls", "svdpc"))
    fitFunc <- switch(method,
                      kernelpls = kernelpls.fit,
                      widekernelpls = widekernelpls.fit,
                      simpls = simpls.fit,
                      oscorespls = oscorespls.fit,
                      cppls = cppls.fit,
                      svdpc = svdpc.fit)

    ## Variables to save CV results in:
    adj <- matrix(0, nrow = nresp, ncol = ncomp)
    cvPred <- pred <- array(0, dim = c(nobj, nresp, ncomp))
    if (jackknife)
        cvCoef <- array(dim = c(npred, nresp, ncomp, length(segments)))
    if (method == "cppls") gammas <- list()

    mvrCvOne <- function(n.seg, X, Y, Y.add, ncomp, segments, scale, weights,
                         method, ...) {
        if (trace) cat(n.seg, "")

        nobj <- dim(X)[1]
        npred <- dim(X)[2]
        nresp <- dim(Y)[2]

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

        ## Optionally collect coefficients:
        #FIXME if (jackknife) cvCoef[,,,n.seg] <- fit$coefficients

        ## Optionally collect gamma-values from CPPLS
        #FIXME if (method == "cppls") gammas[[n.seg]] <- fit$gammas

        ## Set up test data:
        Xtest <- X
        if (scale) Xtest <- Xtest / rep(sdtrain, each = nobj)
        Xtest <- Xtest - rep(fit$Xmeans, each = nobj)

        ## Predict test data:
        pred <- array(0, dim = c(nobj, nresp, ncomp))
        Ymeansrep <- rep(fit$Ymeans, each = nobj)
        for (a in 1:ncomp)
            pred[,,a] <- Xtest %*% fit$coefficients[,,a] + Ymeansrep

        ## Save the cross-validated predictions:
        #FIXME cvPred[seg,,] <- pred[seg,,, drop=FALSE]
        #FIXME adj <- adj + length(seg) * colSums((pred - c(Y))^2)

        return(list(adj = length(seg) * colSums((pred - c(Y))^2),
                    cvPred = pred[seg,,, drop=FALSE],
                    gammas = if (method == "cppls") fit$gammas else NULL,
                    cvCoef = if (jackknife) fit$coefficients else NULL
                    ))
    }

    ## Idea for specification:
    ## not set, or 1: lapply(...)
    ## number > 1 => mclappy(..., number)
    ## cluster object => parLapply(cl, ...) (i.e., no stopCluster)
    ## quote(makeCluster(...)) (?)
    ##   => cl <- eval(...), parLapply(cl, ...), stopCluster(cl)

    ## Things to test/do
    ## - Test PSOCK and MPI on several hosts
    ## - Test using user-created cluster object
    ## - Create logic in pls.options and mvrCv to handle all cases

    if (trace) cat("Segment: ")
    #cat("Using lapply...\n")
    #results <- lapply(seq_along(segments), mvrCvOne, X, Y, Y.add, ncomp,
    #                    segments, scale, weights, method, ...)
    require(snow)                       # Workaround for MPI bug
    require(parallel)
    #cat("Using mclapply...\n")
    #results <- mclapply(seq_along(segments), mvrCvOne, X, Y, Y.add, ncomp,
    #                    segments, scale, weights, method, ...)
    #cat("Using parLapply, FORK...\n")
    #cl <- makeCluster(2, "FORK")
    #results <- parLapply(cl, seq_along(segments), mvrCvOne, X, Y, Y.add, ncomp,
    #                     segments, scale, weights, method, ...)
    #stopCluster(cl)
    #cat("Using parLapply, PSOCK...\n")
    #cl <- makeCluster(2, "PSOCK")
    #results <- parLapply(cl, seq_along(segments), mvrCvOne, X, Y, Y.add, ncomp,
    #                     segments, scale, weights, method, ...)
    #stopCluster(cl)
    cat("Using parLapply, MPI...\n")
    cl <- makeCluster(2, "MPI")
    results <- parLapply(cl, seq_along(segments), mvrCvOne, X, Y, Y.add, ncomp,
                         segments, scale, weights, method, ...)
    stopCluster(cl)

    if (trace) cat("\n")

    ## Collect the results:
    for (n.seg in 1:length(segments)) {
        res <- results[[n.seg]]
        adj <- adj + res$adj
        cvPred[segments[[n.seg]],,] <- res$cvPred
        if (method == "cppls") gammas[[n.seg]] <- res$gammas
        if (jackknife) cvCoef[,,,n.seg] <- res$cvCoef
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
                                 paste("Seg", seq.int(along = segments)))

    list(method = "CV", pred = cvPred, coefficients = if (jackknife) cvCoef,
         gammas = if (method == "cppls") gammas,
         PRESS0 = PRESS0, PRESS = PRESS, adj = adj / nobj^2,
         segments = segments, ncomp = ncomp)
}
