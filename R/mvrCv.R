### mvrCv.R: The basic cross-validation function
### $Id$

## X and Y can not contain NA.  They are not supposed to be centered.

mvrCv <- function(X, Y, ncomp,
                  ## Maybe use FUN directly?
                  method = c("kernelpls", "simpls", "oscorespls", "svdpc"),
                  segments = 10,
                  segment.type = c("random", "consecutive", "interleaved"),
                  length.seg, trace = FALSE, ...)
{
    ## Init:
    Y <- as.matrix(Y)
    dy <- dim(Y)
    dx <- dim(X)
    ## Remove and save dimnames for performance (does it matter?):
    dnX <- dimnames(X)
    dnY <- dimnames(Y)
    dimnames(X) <- dimnames(Y) <- NULL

    ## Set up segments:
    if (is.list(segments)) {
        if (is.null(attr(segments, "type")))
            attr(segments, "type") <- "user supplied"
    } else {
        if (missing(length.seg)) {
            segments <- cvsegments(dx[1], k = segments, type = segment.type)
        } else {
            segments <- cvsegments(dx[1], length.seg = length.seg,
                                   type = segment.type)
        }
    }

    ## Select fit function:
    method <- match.arg(method)
    fitFunc <- switch(method,
                      simpls = simpls.fit,
                      kernelpls = kernelpls.fit,
                      oscorespls = oscorespls.fit,
                      svdpc = svdpc.fit)

    ## Variables to save CV results in:
    adj <- numeric(ncomp)
    cvPred <- pred <- array(dim = c(dx[1], dy[2], ncomp))

    if (trace) cat("Segment: ")
    for (n.seg in 1:length(segments)) {
        if (trace) cat(n.seg, "")
        seg <- segments[[n.seg]]
        fit <- fitFunc(X[-seg,], Y[-seg,], ncomp, stripped = TRUE, ...)
        Xtest <- sweep(X, 2, fit$Xmeans)
        for (a in 1:ncomp) 
            pred[,,a] <-
                sweep(Xtest %*% fit$coefficients[,,a], 2, fit$Ymeans, "+")

        ## Save the cross-validated predictions:
        cvPred[seg,,] <- pred[seg,,, drop=FALSE]
        adj <- adj + length(seg) * colSums((pred - c(Y))^2)
    }
    if (trace) cat("\n")

    ## Calculate MSEP:
    MSEP0 <- apply(Y, 2, var) * dx[1] / (dx[1] - 1) # FIXME: Only correct for loocv!
    MSEP <- colMeans((cvPred - c(Y))^2)

    ## Calculate R2:
    R2 <- matrix(nrow = dy[2], ncol = ncomp)
    for (i in 1:dy[2]) R2[i,] <- cor(cvPred[,i,], Y[,i])
    
    ## Add dimnames:
    objnames <- dnX[[1]]
    if (is.null(objnames)) objnames <- dnY[[1]]
    yvarnames <- dnY[[2]]
    nCompnames <- paste(1:ncomp, "comps")
    names(MSEP0) <- yvarnames
    dimnames(adj) <- dimnames(MSEP) <- dimnames(R2) <-
        list(yvarnames, nCompnames)
    dimnames(cvPred) <- list(objnames, yvarnames, nCompnames)

    list(method = "CV", pred = cvPred,
         MSEP0 = MSEP0, MSEP = MSEP, adj = adj / dx[1]^2,
         R2 = R2, segments = segments)
}
