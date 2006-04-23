### oscorespls.fit.R: The multiresponse orthogonal scores
### algorithm (Martens and Næs, pp. 121--122 and 157--158)
###
### $Id$

oscorespls.fit <- function(X, Y, ncomp, stripped = FALSE,
                           tol = .Machine$double.eps^0.5, ...)
{
    ## Initialise
    Y <- as.matrix(Y)
    dx <- dim(X)
    dy <- dim(Y)
    if (!stripped) {
        ## Save dimnames
        dnX <- dimnames(X)
        dnY <- dimnames(Y)
    }
    ## Remove dimnames for performance (does it matter?):
    dimnames(X) <- dimnames(Y) <- NULL

    W <- P <- matrix(0, dx[2], ncomp)
    Q <- matrix(0, dy[2], ncomp)
    residuals <- array(dim = c(dx[1], dy[2], ncomp))
    if (!stripped) {
        fitted <- residuals
        TEE <- U <- matrix(0, dx[1], ncomp)
    }
    ## C1
    X.mean <- colMeans(X)
    X <- sweep(X, 2, X.mean)
    if (!stripped) X0 <- X              # Needed for calc. of fitted
    Y.mean <- colMeans(Y)
    Y <- sweep(Y, 2, Y.mean)
    for(a in 1:ncomp) {
        ## Initial values:
        if (dy[2] == 1) {               # pls1
            u.a <- Y
        } else {                        # pls2
            ## The coloumn of Y with largest sum of squares:
            u.a <- Y[,which.max(colSums(Y^2))]
            t.a.old <- matrix(rep(0, dx[1]))
        }
        repeat {
            ## C2.1
            w.a <- crossprod(X, u.a)
            w.a <- w.a / drop(sqrt(crossprod(w.a)))
            ## C2.2
            t.a <- X %*% w.a
            ## C2.3
            t.tt <- t.a / drop(crossprod(t.a))
            p.a <- crossprod(X, t.tt)
            ## C2.4
            q.a <- crossprod(Y, t.tt)
            if (dy[2] == 1) break       # pls1: no iteration
            ## Convergence check for pls2:
            ## C2.4b
            convergence <- sum(abs((t.a - t.a.old) / t.a)) < tol
            ## C2.4c
            if (convergence)
                break
            else {
                u.a <- Y %*% q.a / drop(crossprod(q.a))
                t.a.old <- t.a          # Save for comparison
            }
        }
        ## C2.5
        X <- X - t.a %*% t(p.a)
        Y <- residuals[,,a] <- Y - t.a %*% t(q.a)
        ## Save scores etc:
        W[,a] <- w.a
        P[,a] <- p.a
        Q[,a] <- q.a
        if (!stripped) {
            TEE[,a] <- t.a
            U[,a] <- u.a
        }
    }

    ## Calculate regression coefficients and fitted values:
    PW <- crossprod(P, W)
    ## It is known that P^tW is right bi-diagonal (one response) or upper
    ## triangular (multiple responses), with all diagonal elements equal to 1.
    ## Old: On MSWin, this doesn't make much difference:
    ## R1.9.1/Linux: up to numeric accuracy (2.2e-16) this is a noop:
    diag(PW) <- 1
    ## Old: On MSWin, this destroys the coefficients for the last component (when
    ## ncomp = dx[1]):
    ## R1.9.1/Linux: When p > n, PW[-((n-1):n),n] can be far from 0, especially
    ## for the first components.  This has an effect on the n'th component.
    if (dy[2] == 1) {
        ## Single response; make shure all elements above the bidiagonal are 0:
        PW[row(PW) < col(PW)-1] <- 0
    }
    ## For small p, this is faster:
    ## R <- t(backsolve(PW, t(W), transpose = TRUE))
    ## When p is large, this is faster (due to the transpositions of W and R):
    R <- W %*% backsolve(PW, diag(ncomp))
    B <- array(dim = c(dx[2], dy[2], ncomp))
    for (i in 1:ncomp) {
        B[,,i] <- R[,1:i, drop=FALSE] %*% t(Q[,1:i, drop=FALSE])
        if (!stripped) fitted[,,i] <- X0 %*% B[,,i]
    }

    if (stripped) {
        ## Return as quickly as possible
        list(coefficients = B, Xmeans = X.mean, Ymeans = Y.mean)
    } else {
        fitted <- sweep(fitted, 2, Y.mean, "+") # Add mean

        ## Add dimnames and classes:
        objnames <- dnX[[1]]
        if (is.null(objnames)) objnames <- dnY[[1]]
        xvarnames <- dnX[[2]]
        yvarnames <- dnY[[2]]
        compnames <- paste("Comp", 1:ncomp)
        nCompnames <- paste(1:ncomp, "comps")
        dimnames(TEE) <- dimnames(U) <- list(objnames, compnames)
        dimnames(R) <- dimnames(W) <- dimnames(P) <- list(xvarnames, compnames)
        dimnames(Q) <- list(yvarnames, compnames)
        dimnames(B) <- list(xvarnames, yvarnames, nCompnames)
        dimnames(fitted) <- dimnames(residuals) <-
            list(objnames, yvarnames, nCompnames)
        class(TEE) <- class(U) <- "scores"
        class(P) <- class(W) <- class(Q) <- "loadings"

        list(coefficients = B,
             scores = TEE, loadings = P,
             loading.weights = W,
             Yscores = U, Yloadings = Q,
             projection = R,
             Xmeans = X.mean, Ymeans = Y.mean,
             fitted.values = fitted, residuals = residuals,
             Xvar = colSums(P^2) * colSums(TEE^2),# FIXME: Is this the best way?
             Xtotvar = sum(X0^2))
    }
}
