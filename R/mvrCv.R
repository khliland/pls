### mvrCv.R: The basic cross-validation function
### $Id$

mvrCv <- function(X, Y, ncomp,
                  method = pls.options()$mvralg,
                  scale = FALSE, segments = 10,
                  segment.type = c("random", "consecutive", "interleaved"),
                  length.seg, trace = FALSE, ...)
{
    ## Initialise:
    Y <- as.matrix(Y)
    ## Save dimnames:
    dnX <- dimnames(X)
    dnY <- dimnames(Y)
    ## Remove dimnames for performance (doesn't seem to matter; in fact,
    ## as far as it has any effect, it hurts a tiny bit in most situations).
    ## dimnames(X) <- dimnames(Y) <- NULL

    nobj <- dim(X)[1]
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
    method <- match.arg(method,c("kernelpls", "simpls", "oscorespls", "svdpc"))
    fitFunc <- switch(method,
                      simpls = simpls.fit,
                      kernelpls = kernelpls.fit,
                      oscorespls = oscorespls.fit,
                      svdpc = svdpc.fit)

    ## Variables to save CV results in:
    adj <- numeric(ncomp)
    cvPred <- pred <- array(0, dim = c(nobj, nresp, ncomp))

    if (trace) cat("Segment: ")
    for (n.seg in 1:length(segments)) {
        if (trace) cat(n.seg, "")

        ## Set up train data:
        seg <- segments[[n.seg]]
        Xtrain <- X[-seg,]
        if (scale) {
            ntrain <- nrow(Xtrain)
            ## This is faster than sd(X), but cannot handle missing values:
            sdtrain <-
                sqrt(colSums((Xtrain - rep(colMeans(Xtrain), each = ntrain))^2) /
                     (ntrain - 1))
            Xtrain <- Xtrain / rep(sdtrain, each = ntrain)
        }

        ## Fit the model:
        fit <- fitFunc(Xtrain, Y[-seg,], ncomp, stripped = TRUE, ...)

        ## Set up test data:
        Xtest <- X
        if (scale) Xtest <- Xtest / rep(sdtrain, each = nobj)
        Xtest <- Xtest - rep(fit$Xmeans, each = nobj)

        ## Predict test data:
        Ymeansrep <- rep(fit$Ymeans, each = nobj)
        for (a in 1:ncomp)
            pred[,,a] <- Xtest %*% fit$coefficients[,,a] + Ymeansrep

        ## Save the cross-validated predictions:
        cvPred[seg,,] <- pred[seg,,, drop=FALSE]
        adj <- adj + length(seg) * colSums((pred - c(Y))^2)
    }
    if (trace) cat("\n")

    ## Calculate MSEP:
    MSEP0 <- apply(Y, 2, var) * nobj / (nobj - 1) # FIXME: Only correct for loocv!
    MSEP <- colMeans((cvPred - c(Y))^2)

    ## Calculate R2:
    R2 <- matrix(nrow = nresp, ncol = ncomp)
    for (i in 1:nresp) R2[i,] <- cor(cvPred[,i,], Y[,i])^2

    ## Add dimnames:
    objnames <- dnX[[1]]
    if (is.null(objnames)) objnames <- dnY[[1]]
    respnames <- dnY[[2]]
    nCompnames <- paste(1:ncomp, "comps")
    names(MSEP0) <- respnames
    dimnames(adj) <- dimnames(MSEP) <- dimnames(R2) <-
        list(respnames, nCompnames)
    dimnames(cvPred) <- list(objnames, respnames, nCompnames)

    list(method = "CV", pred = cvPred,
         MSEP0 = MSEP0, MSEP = MSEP, adj = adj / nobj^2,
         R2 = R2, segments = segments, ncomp = ncomp)
}
