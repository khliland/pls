### kernelpls.fit.R: Kernel PLS fit algorithm.
### The method is particularly efficient if the number of
### variables is much smaller than the number of objects.
### Refined method by Dayal and MacGregor, J. Chemometr. 11 73-85, (1997).
### Modified kernel algorithm 1
### By Ron Wehrens.  Edited by Bjørn-Helge Mevik.
### $Id$

kernelpls.fit <- function(X, Y, ncomp, stripped = FALSE, ...)
{
    Y <- as.matrix(Y)
    if(!stripped) {
        ## Save dimnames:
        dnX <- dimnames(X)
        dnY <- dimnames(Y)
    }
    ## Remove dimnames during calculation.  This can save time!
    dimnames(X) <- dimnames(Y) <- NULL

    nobj <- dim(X)[1]
    nvar <- dim(X)[2]
    npred <- dim(Y)[2]

    ## Center variables:
    Xmeans <- colMeans(X)
    X <- sweep(X, 2, Xmeans)
    Ymeans <- colMeans(Y)
    Y <- sweep(Y, 2, Ymeans)
  
    PP <- matrix(0, ncol = ncomp, nrow = nvar) # X loadings
    QQ <- matrix(0, ncol = ncomp, nrow = npred)# Y loadings
    TT <- matrix(0, ncol = ncomp, nrow = nobj)
    RR <- matrix(0, ncol = ncomp, nrow = nvar)
    B <- array(0, c(dim(X)[2], dim(Y)[2], ncomp))

    if (!stripped) {
        UU <- matrix(0, ncol = ncomp, nrow = nobj)
        Ypred <- array(0, c(nobj, npred, ncomp))
    }
  
    XtY <- crossprod(X, Y)
    for (a in 1:ncomp) {
        if (npred == 1)
            ww <- XtY
        else {
            if (npred > nvar)
                qq <- La.svd(XtY)$vt[1,]
            else
                qq <- La.svd(XtY)$u[1,]
            ww <- XtY %*% qq
        }
    
        rr <- ww
        if (a > 1)
            for (j in 1:(a - 1))
                rr <- rr - (PP[,j] %*% ww) * RR[,j]
    
        tt <- scale(X %*% rr, scale = FALSE) # centered X scores
        tnorm <- sqrt(sum(tt * tt))
        tt <- tt / tnorm
        rr <- rr / tnorm
        pp <- crossprod(X, tt)          # X loadings
        qq <- crossprod(rr, XtY)        # Y loadings, row vector!
        uu <- Y %*% matrix(qq, ncol = 1) # Y block factor scores
        if (a > 1)
            uu <- uu - TT %*% crossprod(TT, uu) # uu orth to previous tt values
    
        ## Now deflate crossprod matrices
        XtY <- XtY - (pp %*% qq)
    
        ## store weights and loadings
        TT[,a] <- tt
        PP[,a] <- pp 
        QQ[,a] <- qq
        RR[,a] <- rr
        if (!stripped) UU[,a] <- uu

        B[,,a] <- RR[,1:a, drop=FALSE] %*% t(QQ[,1:a, drop=FALSE])
        if (!stripped) Ypred[,,a] <- X %*% B[,,a]
    }

    if (stripped) {
        ## Return as quickly as possible
        list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans)
    } else {
        residuals <- - Ypred + c(Y)
        Ypred <- sweep(Ypred, 2, Ymeans, "+")# Add mean

        ## Add dimnames:
        objnames <- dnX[[1]]
        if (is.null(objnames)) objnames <- dnY[[1]]
        xvarnames <- dnX[[2]]
        yvarnames <- dnY[[2]]
        compnames <- paste("Comp", 1:ncomp)
        nCompnames <- paste(1:ncomp, "comps")
        dimnames(TT) <- dimnames(UU) <- list(objnames, compnames)
        dimnames(RR) <- dimnames(PP) <- list(xvarnames, compnames)
        dimnames(QQ) <- list(yvarnames, compnames)
        dimnames(B) <- list(xvarnames, yvarnames, nCompnames)
        dimnames(Ypred) <- dimnames(residuals) <-
            list(objnames, yvarnames, nCompnames)
        class(TT) <- class(UU) <- "scores"
        class(PP) <- class(QQ) <- "loadings"

        list(coefficients = B,
             scores = TT, loadings = PP,
             Yscores = UU, Yloadings = QQ,
             projection = RR,
             Xmeans = Xmeans, Ymeans = Ymeans,
             fitted.values = Ypred, residuals = residuals,
             Xvar = colSums(PP^2), Xtotvar = sum(X^2))
    }
}
