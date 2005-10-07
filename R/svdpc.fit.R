### svdpc.fit.R: SVD PC fit algorithm.
### $Id$

svdpc.fit <- function(X, Y, ncomp, stripped = FALSE, ...)
{
    Y <- as.matrix(Y)
    if (!stripped) {
        ## Save dimnames:
        dnX <- dimnames(X)
        dnY <- dimnames(Y)
    }
    ## Remove dimnames during calculation.  This can save time!
    dimnames(X) <- dimnames(Y) <- NULL

    ## Center variables:
    Xmeans <- colMeans(X)
    X <- sweep(X, 2, Xmeans)
    Ymeans <- colMeans(Y)
    Y <- sweep(Y, 2, Ymeans)

    B <- array(0, c(dim(X)[2], dim(Y)[2], ncomp))

    if (!stripped) Ypred <- array(0, c(dim(X)[1], dim(Y)[2], ncomp))

    huhn <- La.svd(X)
    U <- huhn$u[,1:ncomp, drop=FALSE]
    D <- huhn$d[1:ncomp]
    Vt <- huhn$vt[1:ncomp,, drop=FALSE]

    for (i in 1:ncomp) {
        B[,,i] <- t(Vt[1:i,, drop=FALSE]) %*%
            diag(1 / D[1:i], nrow = i) %*%
                t(U[,1:i, drop=FALSE]) %*% Y
        if (!stripped) Ypred[,,i] <- X %*% B[,,i]
    }

    if (stripped) {
        ## Return as quickly as possible
        list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans)
    } else {
        residuals <- - Ypred + c(Y)
        Ypred <- sweep(Ypred, 2, Ymeans, "+")# Add mean
        loadings <- t(Vt * D)
        projection <- t(Vt / D)

        ## Add dimnames and classes:
        objnames <- dnX[[1]]
        if (is.null(objnames)) objnames <- dnY[[1]]
        xvarnames <- dnX[[2]]
        yvarnames <- dnY[[2]]
        compnames <- paste("Comp", 1:ncomp)
        nCompnames <- paste(1:ncomp, "comps")
        dimnames(U) <- list(objnames, compnames)
        dimnames(loadings) <- dimnames(projection) <- list(xvarnames, compnames)
        dimnames(B) <- list(xvarnames, yvarnames, nCompnames)
        dimnames(Ypred) <- dimnames(residuals) <-
            list(objnames, yvarnames, nCompnames)
        names(D) <- compnames

        list(coefficients = B,
             scores = structure(U[,1:ncomp, drop=FALSE], class = "scores"),
             loadings = loadings,
             projection = projection,
             Xmeans = Xmeans, Ymeans = Ymeans,
             fitted.values = Ypred, residuals = residuals,
             Xvar = D^2, Xtotvar = sum(X^2))
    }
}
