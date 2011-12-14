### plsda.fit.R: PLS discriminant analysis
### $Id$
plsda.fit <- function(X, Y, ncomp, stripped = FALSE, weights = NULL, ...)
{
    ## X       - the data matrix
    ## Y       - the primary response matrix
    ## ncomp   - number of components
    ## weights - prior weighting of observations (optional)
    Y <- as.matrix(Y)

    nobj <- dim(X)[1]
    npred <- dim(X)[2]
    nresp <- dim(Y)[2]

    if(is.null(weights)){
        Xmeans <- colMeans(X)
        X <- X - rep(Xmeans, each = nobj)
        Pi <- apply(Y, 2, sum) / nobj
        weights <- rep(1, nobj)
    } else {
        Xmeans <- crossprod(weights, X) / sum(weights)
        X <- X - rep(Xmeans, each = nobj)
        Pi <- numeric(nresp)
        for(i in 1:nresp){
            Pi[i] <- sum(weights[Y[,i]==1]) / nresp
        }
    }

    X.orig <- X
    Ymeans = colMeans(Y)

    if(!stripped) {
        ## Save dimnames:
        dnX <- dimnames(X)
        dnY <- dimnames(Y)
    }
    dimnames(X) <- dimnames(Y) <- NULL


    ## Declaration of variables
    W   <- matrix(0, npred, ncomp)    # W-loadings
    TT  <- matrix(0, nobj, ncomp)     # T-scores
    P   <- matrix(0, npred, ncomp)    # P-loadings
    D   <- diag(sqrt(Pi) / nresp, ncol = nresp) # Weighting matrix
    tsqs <- rep.int(1, ncomp)         # t't
    Q   <- matrix(0, nresp, ncomp)    # Q-loadings
    B   <- array(0, c(npred, nresp, ncomp))

    if(!stripped){
        U <- TT                     # U-scores
        tsqs <- rep.int(1, ncomp)   # t't
        fitted <- array(0, c(nobj, nresp, ncomp))
    }

    for(a in 1:ncomp){
        W[,a]  <- svd(D %*% t(Y) %*% X, 0,1)$v
        TT[,a] <- X %*% W[,a]
        tsq <- crossprod(TT[,a])[1]
        tsqs[a] <- tsq
        P[,a]  <- (t(X) %*% TT[,a]) / tsq
        X <- X - TT[,a] %*% t(P[,a])
        Q[,a]  <- crossprod(Y, TT[,a]) / tsq
        B[,,a] <- W[,1:a, drop=FALSE] %*%
            tcrossprod(solve(crossprod(P[,1:a, drop=FALSE],
                                       W[,1:a, drop=FALSE])),
                       Q[,1:a, drop=FALSE])

        if (!stripped) {
            tsqs[a] <- tsq
            ## Extra step to calculate Y scores:
            U[,a] <- Y %*% Q[,a] / crossprod(Q[,a])[1] # Ok for nresp == 1 ??
            ## make u orth to previous X scores:
            if (a > 1) U[,a] <- U[,a] - TT %*% (crossprod(TT, U[,a]) / tsqs)
            fitted[,,a] <- X.orig %*% B[,,a]
        }
    }
    if(stripped){
	## Return as quickly as possible
        list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans)
    } else {
        fitted <- fitted + rep(Ymeans, each = nobj) # Add mean
        residuals <- - fitted + c(Y)

        ## Add dimnames:
        objnames <- dnX[[1]]
        if (is.null(objnames)) objnames <- dnY[[1]]
        prednames <- dnX[[2]]
        respnames <- dnY[[2]]
        compnames <- paste("Comp", 1:ncomp)
        nCompnames <- paste(1:ncomp, "comps")
        dimnames(TT) <- list(objnames, compnames)
        dimnames(W) <- dimnames(P) <-
            list(prednames, compnames)
        dimnames(Q) <- list(respnames, compnames)
        dimnames(B) <- list(prednames, respnames, nCompnames)
        dimnames(fitted) <- dimnames(residuals) <-
            list(objnames, respnames, nCompnames)
        class(TT) <- "scores"
        class(P) <- class(W) <- "loadings"

        list(coefficients = B,
             scores = TT, loadings = P,
             loading.weights = W,
             Yscores = U, Yloadings = Q,
             projection = W %*% solve(crossprod(P,W)),
             Xmeans = Xmeans, Ymeans = Ymeans,
             fitted.values = fitted, residuals = residuals,
             Xvar = colSums(P * P) * tsqs,
             Xtotvar = sum(X.orig * X.orig))
    }
}
