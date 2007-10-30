### cppls.fit.R: The Canonical Powered PLS algorithm
###
### $Id$
###
### Implements the CPPLS algorithm as described in FIXME

cppls.fit <- function(X, Y, ncomp, Y2 = NULL, stripped = FALSE,
                      lower = 0.5, upper = 0.5, wt, ...)
{
    ## X     - the data matrix
    ## Y     - the primary response matrix
    ## Y2 - the secondary response matrix (optional)
    ## ncomp - number of components
    ## lower - lower bounds for power algorithm (default=0.5)
    ## upper - upper bounds for power algorithm (default=0.5)
    ## wt    - prior weighting of observations (optional) - not implemented in cross-validation

    Yprim <- as.matrix(Y)
    Y <- cbind(Yprim, Y2)

    nobj <- dim(X)[1]
    npred <- dim(X)[2]
    nresp <- dim(Yprim)[2]

    if(missing(wt)){
        wt <- rep(1,nobj)
        Xmeans <- colMeans(X)
        X <- X - rep(Xmeans, each = nobj)
    } else {
        Xmeans <- crossprod(wt,X)/sum(wt)
        X <- X - rep(Xmeans, each = nobj)
    }

    ## FIXME: Center Y?
    Ymeans = colMeans(Yprim)
    Yprim <- Yprim - rep(Ymeans,each=nobj)
    Y <- Y - rep(colMeans(Y),each=nobj)

    if(!stripped) {
        ## Save dimnames:
        dnX <- dimnames(X)
        dnY <- dimnames(Yprim)
    }
    dimnames(X) <- dimnames(Y) <- dimnames(Yprim) <- NULL

    ## Declaration of variables
    W   <- matrix(0,npred,ncomp)    # W-loadings
    TT  <- matrix(0,nobj,ncomp)     # T-scores
    P   <- matrix(0,npred,ncomp)    # P-loadings
    Q   <- matrix(0,nresp,ncomp)    # Q-loadings
    cc  <- numeric(ncomp)
    pot <- numeric(ncomp)         	# Powers used to construct the w-s in R
    B   <- array(0, c(npred, nresp, ncomp))
    if(!stripped){
        U <- TT     				# U-scores
        tsqs <- rep.int(1, ncomp)   # t't
        fitted <- array(0, c(nobj, nresp, ncomp))
    }

    for(a in 1:ncomp){
        Rlist <- Rcal(X, Y, Yprim, wt, lower, upper)
        pot[a] <- Rlist$pot
        cc[a] <- Rlist$cc

        w.a <- Rlist$w/max(abs(Rlist$w)) # Stabilization
        ## Make new vectors orthogonal to old ones?
        ## w.a <- w.a - W[,1:(a-1)]%*%crossprod(W[,1:(a-1)], w.a)
        w.a[abs(w.a) < .Machine$double.eps] <- 0   # Removes insignificant values
        w.a <- w.a / norm(w.a)                     # Normalization
        t.a <- X %*% w.a                           # Score vectors
        tsq <- crossprod(t.a)[1]
        p.a <- crossprod(X,t.a) / tsq
        q.a <- crossprod(Yprim,t.a) / tsq
        X   <- X - tcrossprod(t.a,p.a)             # Deflation

        W[,a]  <- w.a
        TT[,a] <- t.a
        P[,a]  <- p.a
        Q[,a]  <- q.a
        B[,,a] <- tcrossprod(W[,1:a, drop=FALSE], Q[,1:a, drop=FALSE])

        if (!stripped) {
            tsqs[a] <- tsq
            ## Extra step to calculate Y scores:
            U[,a] <- Yprim %*% q.a / crossprod(q.a)[1] # Ok for nresp == 1 ??
            ## make u orth to previous X scores:
            if (a > 1) U[,a] <- U[,a] - TT %*% (crossprod(TT, U[,a]) / tsqs)
            fitted[,,a] <- tcrossprod(TT[,1:a, drop=FALSE], Q[,1:a, drop=FALSE])
        }
    }
    if (stripped) {
        ## Return as quickly as possible
        list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans)
    } else {
        residuals <- - fitted + c(Yprim)
        fitted <- fitted + rep(Ymeans, each = nobj) # Add mean

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
        class(P) <- class(W) <- class(Q) <- "loadings"

        list(coefficients = B,
             scores = TT, loadings = P,
             loading.weights = W,
             Yscores = U, Yloadings = Q,
             ## projection = R,
             Xmeans = Xmeans, Ymeans = Ymeans,
             fitted.values = fitted, residuals = residuals,
             Xvar = colSums(P * P) * tsqs,
             Xtotvar = sum(X * X),
             gammas = pot,
             canonical.correlations = cc)
    }
}


################
## Rcal function
Rcal <- function(X, Y, Yprim, wt, lower, upper) {
    CS <- CorrXY(X, Y)        # Matrix of corr(Xj,Yg) and vector of std(Xj)

    sng <- sign(CS$C)               # Signs of C {-1,0,1}
    C <- abs(CS$C)                  # Correlation without signs
    mS <- max(CS$S); S <- CS$S / mS # Divide by largest value
    mC <- max(C); C <- C / mC       #  -------- || --------

    ## Computation of the best vector of loadings
    lw <- lw_bestpar(X, S, C, sng, Yprim, wt, lower, upper)
}


################
## lw_bestpar function
lw_bestpar <- function(X, S, C, sng, Yprim, wt, lower, upper) {

    #########################
    # Optimization function #
    #########################
    f <- function(p, X, S, C, sng, Yprim, wt) {
        if(p == 0){         # 1 - Variable selection from standard deviation
            S[S < max(S)] <- 0
            W0 <- S
        } else if(p == 1) {        # 3 - Variable selection from correlation
            sng[C < max(C)] <- 0
            W0 <- rowSums(sng)
        } else {        # 2 - Standard deviation and correlation with powers
            S <- S^((1-p)/p)
            W0 <- (sng*(C^(p/(1-p))))*S
        }
        Z <- X %*% W0  # Transform X into W0
        Z[abs(Z) < .Machine$double.eps] <- 0
        -(cancorr(Z, Yprim, wt))^2
    }

    #####################################
    # Logic for optimization segment(s) #
    #####################################
    nOpt <- length(lower)
    pot  <- numeric(3*nOpt)
    ca   <- numeric(3*nOpt)

    for (i in 1:nOpt){
        ca[1+(i-1)*3]  <- f(lower[i], X, S, C, sng, Yprim, wt)
        pot[1+(i-1)*3] <- lower[i]
        if (lower[i] != upper[i]) {
            Pc <- optimize(f = f, interval = c(lower[i], upper[i]),
                           tol = .Machine$double.eps^0.25, maximum = FALSE,
                           X = X, S = S, C = C, sng = sng, Yprim = Yprim,
                           wt = wt)
            pot[2+(i-1)*3] <- Pc[[1]]; ca[2+(i-1)*3] <- Pc[[2]]
        }
        ca[3+(i-1)*3]  <- f(upper[i], X, S, C, sng, Yprim, wt)
        pot[3+(i-1)*3] <- upper[i]
    }


    ########################################################
    # Computation of final w-vectors based on optimization #
    ########################################################
    cc <- max(-ca)                      # Determine which is more succesful
    cmin <- which.max(-ca)              # Determine which is more succesful
    if (pot[cmin] == 0) {   # 1 - Variable selection from standard deviation
        w <- S[S < max(S)] <- 0
    } else if (pot[cmin] == 1) {   # 3 - Variable selection from correlation
        sng[C < max(C)] <- 0
        w <- rowSums(sng)
    } else {            # 2 - Standard deviation and correlation with powers
        p <- pot[cmin]                  # Power from optimization
        S <- S^((1-p)/p)
        W0 <- (sng*(C^(p/(1-p))))*S

        Z <- X %*% W0                   # Transform X into W
        Z[abs(Z) < .Machine$double.eps] <- 0
        Ar <- cancorr(Z, Yprim, wt, FALSE) # Computes canonical correlations between columns in XW and Y with rows weighted according to wt
        w <- W0 %*% Ar[,1, drop=FALSE]  # Optimal loadings
    }
    pot <- pot[cmin]
    list(w = w, pot = pot, cc = cc)
}


################
## CorrXY function
CorrXY <- function(X, Y) {
    ##  Computation of correlations between the columns of X and Y
    n <- dim(X)[1]
    cy <- colMeans(Y)
    cx <- colMeans(X)
    Y <- Y - tcrossprod(rep(1, n), cy)
    X <- X - tcrossprod(rep(1, n), cx)

    sdX <- std(X, cx, n)
    sdX[sdX < .Machine$double.eps] <- 0

    ccxy <- crossprod(X, Y) / (n * tcrossprod(sdX, std(Y, cy, n)))
    ccxy[!is.finite(ccxy)] <- 0
    CS <- list(C = ccxy, S = sdX)
}


################
## Weighted centering
Center <- function(X, wt){
    ## Centering of the data matrix X by subtracting the weighted column means
    ## according to the nonegative weights wt
    np <- dim(X)

    ## Calculation of column means:
    if (missing(wt))
        mX <- colMeans(X)
    else
        mX <- crossprod(wt, X) / sum(wt)

    ## Centering of X, similar to: #X = X-ones(n,1)*mX;
    X <- X - rep(mX, each = nobj)
}


################
## function norm
norm <- function(vec) {
    sqrt(crossprod(vec)[1])
}


################
## function std (n-vektet)
std <- function(X, m, n) {
    r <- numeric(length(m))
    for(i in 1:length(m)){
        r[i] <- sqrt(crossprod((X[,i] - m[i]))[1] / n)
    }
    r
}


################
## Machine precision of a number
eps <- function(X){
    .Machine$double.eps * 2^floor(log2(abs(X)))
}

################
## Dummy representation of Y
dummy <- function(Y) {
    m <- unique(Y)
    Yd <- matrix(0, length(Y), length(m))
    for(i in 1:length(m)){
        Yd[,i] <- 1*(Y == m[i])
    }
    Yd
}


################
## Stripped version of canonical correlation (cancor)
cancorr <- function (x, y, wt, opt = TRUE) {
    nr <- nrow(x)
    ncx <- ncol(x)
    ncy <- ncol(y)
    x <- x * wt
    y <- y * wt
    qx <- qr(x, tol = .Machine$double.eps)
    qy <- qr(y, tol = .Machine$double.eps)
    dx <- qx$rank
    if (!dx)
        stop("'x' has rank 0")
    dy <- qy$rank
    if (!dy)
        stop("'y' has rank 0")
    if(opt) {
        z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1:dx,, drop = FALSE],
                 nu = 0, nv = 0)
        ret <- z$d[1]
    } else {
        z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1:dx,, drop = FALSE],
                 nu = dx, nv = 0)
        ret <- backsolve((qx$qr)[1:dx,1:dx, drop = FALSE], z$u)
        if((ncx - nrow(ret)) > 0) {
            ret <- rbind(ret, matrix(0, ncx - nrow(ret), dx))
            ret[qx$pivot,] <- ret
        }
    }
    ret
}
