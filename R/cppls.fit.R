### cppls.fit.R: The Canonical Powered PLS algorithm
###
### Implements the CPPLS algorithm as described in
### Indahl, U.G., Liland, K.H., Næs, T. (2009).
### Canonical partial least squares - a unified PLS approach to classification and regression problems,
### Journal of Chemometrics 23, pp. 495-504



#' @title CPPLS (Indahl et al.)
#'
#' @description Fits a PLS model using the CPPLS algorithm.
#'
#' @details This function should not be called directly, but through the generic
#' functions \code{cppls} or \code{mvr} with the argument
#' \code{method="cppls"}. Canonical Powered PLS (CPPLS) is a generalisation of
#' PLS incorporating discrete and continuous responses (also simultaneously),
#' additional responses, individual weighting of observations and power
#' methodology for sharpening focus on groups of variables. Depending on the
#' input to \code{cppls} it can produce the following special cases: \itemize{
#' \item PLS: uni-response continuous \code{Y} \item PPLS: uni-response
#' continuous \code{Y}, \code{(lower || upper) != 0.5} \item PLS-DA (using
#' correlation maximisation - B/W): dummy-coded descrete response \code{Y}
#' \item PPLS-DA: dummy-coded descrete response \code{Y}, \code{(lower ||
#' upper) != 0.5} \item CPLS: multi-response \code{Y} (continuous, discrete or
#' combination) \item CPPLS: multi-response \code{Y} (continuous, discrete or
#' combination), \code{(lower || upper) != 0.5} } The name "canonical" comes
#' from canonical correlation analysis which is used when calculating vectors
#' of loading weights, while "powered" refers to a reparameterisation of the
#' vectors of loading weights which can be optimised over a given interval.
#'
#' @param X a matrix of observations.  \code{NA}s and \code{Inf}s are not
#' allowed.
#' @param Y a vector or matrix of responses.  \code{NA}s and \code{Inf}s are
#' not allowed.
#' @param ncomp the number of components to be used in the modelling.
#' @param Y.add a vector or matrix of additional responses containing relevant
#' information about the observations.
#' @param center logical, determines if the \eqn{X} and \eqn{Y} matrices are
#' mean centered or not. Default is to perform mean centering.
#' @param stripped logical.  If \code{TRUE} the calculations are stripped as
#' much as possible for speed; this is meant for use with cross-validation or
#' simulations when only the coefficients are needed.  Defaults to
#' \code{FALSE}.
#' @param lower a vector of lower limits for power optimisation. Defaults to
#' \code{0.5}.
#' @param upper a vector of upper limits for power optimisation. Defaults to
#' \code{0.5}.
#' @param trunc.pow logical. If \code{TRUE} an experimental alternative power
#' algorithm is used. (Optional)
#' @param weights a vector of individual weights for the observations.
#' (Optional)
#' @param \dots other arguments.  Currently ignored.
#' @return A list containing the following components is returned:
#' \item{coefficients}{an array of regression coefficients for 1, \ldots{},
#' \code{ncomp} components.  The dimensions of \code{coefficients} are
#' \code{c(nvar, npred, ncomp)} with \code{nvar} the number of \code{X}
#' variables and \code{npred} the number of variables to be predicted in
#' \code{Y}.} \item{scores}{a matrix of scores.} \item{loadings}{a matrix of
#' loadings.} \item{loading.weights}{a matrix of loading weights.}
#' \item{Yscores}{a matrix of Y-scores.} \item{Yloadings}{a matrix of
#' Y-loadings.} \item{projection}{the projection matrix used to convert X to
#' scores.} \item{Xmeans}{a vector of means of the X variables.}
#' \item{Ymeans}{a vector of means of the Y variables.} \item{fitted.values}{an
#' array of fitted values.  The dimensions of \code{fitted.values} are
#' \code{c(nobj, npred, ncomp)} with \code{nobj} the number samples and
#' \code{npred} the number of Y variables.} \item{residuals}{an array of
#' regression residuals.  It has the same dimensions as \code{fitted.values}.}
#' \item{Xvar}{a vector with the amount of X-variance explained by each
#' component.} \item{Xtotvar}{total variance in \code{X}.}
#' \item{gammas}{gamma-values obtained in power optimisation.}
#' \item{canonical.correlations}{Canonical correlation values from the
#' calculations of loading weights.} \item{A}{matrix containing vectors of
#' weights \code{a} from canonical correlation (\code{cor(Za,Yb)}).}
#' \item{smallNorms}{vector of indices of explanatory variables of length close
#' to or equal to 0.}
#'
#' If \code{stripped} is \code{TRUE}, only the components \code{coefficients},
#' \code{Xmeans}, \code{Ymeans} and \code{gammas} are returned.
#' @author Kristian Hovde Liland
#' @seealso \code{\link{mvr}} \code{\link{plsr}} \code{\link{pcr}}
#' \code{\link{widekernelpls.fit}} \code{\link{simpls.fit}}
#' \code{\link{oscorespls.fit}}
#' @references Indahl, U. (2005) A twist to partial least squares regression.
#' \emph{Journal of Chemometrics}, \bold{19}, 32--44.
#'
#' Liland, K.H and Indahl, U.G (2009) Powered partial least squares
#' discriminant analysis, \emph{Journal of Chemometrics}, \bold{23}, 7--18.
#'
#' Indahl, U.G., Liland, K.H. and Næs, T. (2009) Canonical partial least
#' squares - a unified PLS approach to classification and regression problems.
#' \emph{Journal of Chemometrics}, \bold{23}, 495--504.
#' @keywords regression classification multivariate
#' @examples
#'
#' data(mayonnaise)
#' # Create dummy response
#' mayonnaise$dummy <-
#'     I(model.matrix(~y-1, data.frame(y = factor(mayonnaise$oil.type))))
#'
#' # Predict CPLS scores for test data
#' may.cpls <- cppls(dummy ~ NIR, 10, data = mayonnaise, subset = train)
#' may.test <- predict(may.cpls, newdata = mayonnaise[!mayonnaise$train,], type = "score")
#'
#' # Predict CPLS scores for test data (experimental used design as additional Y information)
#' may.cpls.yadd <- cppls(dummy ~ NIR, 10, data = mayonnaise, subset = train, Y.add=design)
#' may.test.yadd <- predict(may.cpls.yadd, newdata = mayonnaise[!mayonnaise$train,], type = "score")
#'
#' # Classification by linear discriminant analysis (LDA)
#' library(MASS)
#' error <- matrix(ncol = 10, nrow = 2)
#' dimnames(error) <- list(Model = c('CPLS', 'CPLS (Y.add)'), ncomp = 1:10)
#' for (i in 1:10) {
#'     fitdata1 <- data.frame(oil.type = mayonnaise$oil.type[mayonnaise$train],
#'                            NIR.score = I(may.cpls$scores[,1:i,drop=FALSE]))
#'     testdata1 <- data.frame(oil.type = mayonnaise$oil.type[!mayonnaise$train],
#'                             NIR.score = I(may.test[,1:i,drop=FALSE]))
#'     error[1,i] <-
#'         (42 - sum(predict(lda(oil.type ~ NIR.score, data = fitdata1),
#'                   newdata = testdata1)$class == testdata1$oil.type)) / 42
#'     fitdata2 <- data.frame(oil.type = mayonnaise$oil.type[mayonnaise$train],
#'                            NIR.score = I(may.cpls.yadd$scores[,1:i,drop=FALSE]))
#'     testdata2 <- data.frame(oil.type = mayonnaise$oil.type[!mayonnaise$train],
#'                             NIR.score = I(may.test.yadd[,1:i,drop=FALSE]))
#'     error[2,i] <-
#'         (42 - sum(predict(lda(oil.type ~ NIR.score, data = fitdata2),
#'                   newdata = testdata2)$class == testdata2$oil.type)) / 42
#' }
#' round(error,2)
#'
#' @export
cppls.fit <- function(X, Y, ncomp, Y.add = NULL, center = TRUE,
                      stripped = FALSE, lower = 0.5, upper = 0.5,
                      trunc.pow = FALSE, weights = NULL, ...)
{
    ## X       - the data matrix
    ## Y       - the primary response matrix
    ## Y.add   - the additional response matrix (optional)
    ## ncomp   - number of components
    ## lower   - lower bounds for power algorithm (default=0.5)
    ## upper   - upper bounds for power algorithm (default=0.5)
    ## weights - prior weighting of observations (optional)

    Y.orig <- Yprim <- as.matrix(Y)
    Y <- cbind(Yprim, Y.add)

    if (!stripped) {
        ## Save dimnames:
        dnX <- dimnames(X)
        dnY <- dimnames(Yprim)
    }
    dimnames(X) <- dimnames(Y) <- dimnames(Yprim) <- NULL

    nobj  <- dim(X)[1]
    npred <- dim(X)[2]
    nresp <- dim(Yprim)[2]

    ## Center variables:
    if (center) {
        if (is.null(weights)) {
            Xmeans <- colMeans(X)
            X <- X - rep(Xmeans, each = nobj)
        } else {
            Xmeans <- c(crossprod(weights,X) / sum(weights))
            X <- X - rep(Xmeans, each = nobj)
        }
        Ymeans <- colMeans(Yprim)
        Yprim <- Yprim - rep(Ymeans, each = nobj)
        Y <- Y - rep(colMeans(Y), each = nobj)
    } else  {
        ## Set means to zero. Will ensure that predictions do not take the
        ## mean into account.
        Xmeans <- rep_len(0, npred)
        Ymeans <- rep_len(0, nresp)
    }

    X.orig <- X

    ## Declaration of variables
    W   <- matrix(0, npred, ncomp)    # W-loadings
    TT  <- matrix(0, nobj,  ncomp)    # T-scores
    P   <- matrix(0, npred, ncomp)    # P-loadings
    Q   <- matrix(0, nresp, ncomp)    # Q-loadings
    A   <- matrix(0, dim(Y)[2], ncomp)# Column weights for W0 (from CCA)
    cc  <- numeric(ncomp)
    pot <- rep(0.5, ncomp)           # Powers used to construct the w-s in R
    B   <- array(0, c(npred, nresp, ncomp))
    smallNorm <- numeric()
    if (!stripped) {
        U <- TT                     # U-scores
        tsqs <- rep.int(1, ncomp)   # t't
        fitted <- array(0, c(nobj, nresp, ncomp))
    }

    for (a in 1:ncomp) {
        if (length(lower) == 1 && lower == 0.5 && length(upper) == 1 && upper == 0.5 ) {
            Rlist <- Rcal(X, Y, Yprim, weights) # Default CPLS algorithm
        } else {
            Rlist <- RcalP(X, Y, Yprim, weights, lower, upper, trunc.pow) # Alternate CPPLS algorithm
            pot[a] <- Rlist$pot
        }
        cc[a] <- Rlist$cc
        w.a <- Rlist$w
        ifelse(!is.null(Rlist$a), aa <- Rlist$a, aa <- NA)
        A[,a] <- aa

        ## Make new vectors orthogonal to old ones?
        ## w.a <- w.a - W[,1:(a-1)]%*%crossprod(W[,1:(a-1)], w.a)
        w.a[abs(w.a) < pls.options()$w.tol] <- 0   # Removes insignificant values
        w.a <- w.a / norm(w.a)                     # Normalization
        t.a <- X %*% w.a                           # Score vectors
        tsq <- crossprod(t.a)[1]
        p.a <- crossprod(X,t.a) / tsq
        q.a <- crossprod(Yprim,t.a) / tsq
        X   <- X - tcrossprod(t.a,p.a)             # Deflation

        ## Check and compensate for small norms
        mm <- apply(abs(X), 2, sum)
        r <- which(mm < pls.options()$X.tol)
        if (length(r) > 0) {
            for (i in 1:length(r)) {
                if (sum(smallNorm == r[i]) == 0) {
                    ## Add new short small to list
                    smallNorm[length(smallNorm) + 1] <- r[i]
                }
            }
        }
        X[,smallNorm] <- 0 # Remove collumns having small norms

        W[,a]  <- w.a
        TT[,a] <- t.a
        P[,a]  <- p.a
        Q[,a]  <- q.a
        B[,,a] <- W[,1:a, drop=FALSE] %*%
            tcrossprod(
                solve(crossprod(P[,1:a, drop=FALSE], W[,1:a, drop=FALSE])),
                Q[,1:a, drop=FALSE]
            )

        if (!stripped) {
            tsqs[a] <- tsq
            ## Extra step to calculate Y scores:
            U[,a] <- Yprim %*% q.a / crossprod(q.a)[1] # Ok for nresp == 1 ??
            ## make u orth to previous X scores:
            if (a > 1) U[,a] <- U[,a] - TT %*% (crossprod(TT, U[,a]) / tsqs)
            fitted[,,a] <- X.orig %*% B[,,a]
        }
    }
    if (stripped) {
        ## Return as quickly as possible
        list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans, gammas = pot)
    } else {
        fitted <- fitted + rep(Ymeans, each = nobj) # Add mean
        residuals <- - fitted + c(Y.orig)

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
        colnames(A) <- compnames
        class(TT) <- "scores"
        class(P) <- class(W) <- class(Q) <- "loadings"

        list(coefficients = B,
             scores = TT, loadings = P,
             loading.weights = W,
             Yscores = U, Yloadings = Q,
             projection = W %*% solve(crossprod(P,W)),
             Xmeans = Xmeans, Ymeans = Ymeans,
             fitted.values = fitted, residuals = residuals,
             Xvar = colSums(P * P) * tsqs,
             Xtotvar = sum(X.orig * X.orig),
             gammas = pot,
             canonical.correlations = cc,
             smallNorm = smallNorm,
             A = A, trunc.pow = trunc.pow)
    }
}


#######################
## Rcal function (CPLS)
Rcal <- function(X, Y, Yprim, weights) {
    W0 <- crossprod(X,Y)
    Ar <- cancorr(X %*% W0, Yprim, weights, FALSE) # Computes canonical correlations between columns in XW and Y with rows weighted according to 'weights'
    w  <- W0 %*% Ar$A[,1, drop=FALSE]  # Optimal loadings
    ifelse(exists('Ar'), a <- Ar$A[,1], a <- NA)
    list(w = w, cc = Ar$r^2, a = a)
}


#########################
## RcalP function (CPPLS)
RcalP <- function(X, Y, Yprim, weights, lower, upper, trunc.pow) {
    CS <- CorrXY(X, Y, weights)     # Matrix of corr(Xj,Yg) and vector of std(Xj)
    sng <- sign(CS$C)               # Signs of C {-1,0,1}
    C <- abs(CS$C)                  # Correlation without signs
    mS <- max(CS$S); S <- CS$S / mS # Divide by largest value
    mC <- max(C); C <- C / mC       #  -------- || --------

    ## Computation of the best vector of loadings
    lw <- lw_bestpar(X, S, C, sng, Yprim, weights, lower, upper, trunc.pow)
}


################
## lw_bestpar function
lw_bestpar <- function(X, S, C, sng, Yprim, weights, lower, upper,
                       trunc.pow)
{
    if (!is.null(weights))
        weights <- sqrt(weights) # Prepare weights for cca
    ## Compute for S and each columns of C the distance from the median scaled to [0,1]
    if (trunc.pow) {
        medC <- t(abs(t(sng * C) - apply(sng * C, 2, median)))
        medC <- t(t(medC) / apply(medC, 2, max))
        medS <- abs(S - median(S))
        medS <- medS / max(medS)
    } else {
        medS <- medC <- NULL
    }

    #########################
    # Optimization function #
    #########################
    f <- function(p, X, S, C, sng, Yprim, weights, trunc.pow, medS, medC) {
        if(p == 0) {        # Variable selection from standard deviation
            S[S < max(S)] <- 0
            W0 <- S
        } else if(p == 1) { # Variable selection from correlation
            C[C < max(C)] <- 0
            W0 <- rowSums(C)
        } else {            # Standard deviation and correlation with powers
            if (trunc.pow) {
                ps <- (1 - p) / p
                if (ps < 1) {
                    S <- S^ps
                } else {
                    S[medS < (1 - 2 * p)] <- 0
                }
                pc <- p / (1 - p)
                if (pc < 1) {
                    W0 <- (sng * (C^pc)) * S
                } else {
                    C[medC < (2 * p - 1)] <- 0
                    W0 <- (sng * C) * S
                }
            } else {
                S <- S^((1 - p) / p)
                W0 <- (sng * (C^(p / (1 - p)))) * S
            }
        }
        Z <- X %*% W0  # Transform X into W0
        -(cancorr(Z, Yprim, weights))^2
    }

    #####################################
    # Logic for optimization segment(s) #
    #####################################
    nOpt <- length(lower)
    pot  <- numeric(3 * nOpt)
    ca   <- numeric(3 * nOpt)

    for (i in 1:nOpt) {
        ca[ 1 + (i - 1) * 3] <-
            f(lower[i], X, S, C, sng, Yprim, weights, trunc.pow, medS, medC)
        pot[1 + (i - 1) * 3] <- lower[i]
        if (lower[i] != upper[i]) {
            Pc <- optimize(f = f, interval = c(lower[i], upper[i]),
                           tol = 10^-4, maximum = FALSE,
                           X = X, S = S, C = C, sng = sng, Yprim = Yprim,
                           weights = weights, trunc.pow = trunc.pow, medS, medC)
            pot[2 + (i - 1) * 3] <- Pc[[1]]
            ca[ 2 + (i - 1) * 3] <- Pc[[2]]
        }
        ca[ 3 + (i - 1) * 3] <-
            f(upper[i], X, S, C, sng, Yprim, weights, trunc.pow, medS, medC)
        pot[3 + (i - 1) * 3] <- upper[i]
    }


    ########################################################
    # Computation of final w-vectors based on optimization #
    ########################################################
    cc <- max(-ca)                      # Determine which is more succesful
    cmin <- which.max(-ca)              # Determine which is more succesful
    if (pot[cmin] == 0) {        # Variable selection from standard deviation
        S[S < max(S)] <- 0
        w <- S
    } else if (pot[cmin] == 1) { # Variable selection from correlation
        C[C < max(C)] <- 0
        w <- rowSums(C)
    } else {                     # Standard deviation and correlation with powers
        p <- pot[cmin]           # Power from optimization
        if (trunc.pow) {         # New power algorithm
            ps <- (1 - p) / p
            if (ps < 1) {
                S <- S^ps
            } else {
                S[medS < (1 - 2 * p)] <- 0
            }
            pc <- p / (1 - p)
            if (pc < 1) {
                W0 <- (sng * (C^pc)) * S
            } else {
                C[medC < (2 * p-1)] <- 0
                W0 <- (sng * C) * S
            }
        } else {
            S <- S^((1 - p) / p)
            W0 <- (sng * (C^(p / (1 - p)))) * S
        }

        Z <- X %*% W0                   # Transform X into W
        Ar <- cancorr(Z, Yprim, weights, FALSE) # Computes canonical correlations between columns in XW and Y with rows weighted according to 'weights'
        w <- W0 %*% Ar$A[,1, drop=FALSE]  # Optimal loadings
    }
    pot <- pot[cmin]
    ifelse(exists('Ar'), a <- Ar$A[,1], a <- NA)
    list(w = w, pot = pot, cc = cc, a = a)
}


################
## CorrXY function
CorrXY <- function(X, Y, weights) {
    ##  Computation of correlations between the columns of X and Y
    n  <- dim(X)[1]
    if (is.null(weights)) {
        cx <- colMeans(X)
        cy <- colMeans(Y)
        X <- X - rep(cx, each = n)
        Y <- Y - rep(cy, each = n)
    } else {
        cx <- crossprod(weights,X) / sum(weights)
        cy <- crossprod(weights,Y) / sum(weights)
        X  <- X - rep(cx, each = n)
        Y  <- Y - rep(cy, each = n)
        X  <- X * weights
        Y  <- Y * weights
    }

    sdX <- sqrt(apply(X^2, 2, mean))
    inds <- which(sdX == 0, arr.ind = FALSE)
    sdX[inds] <- 1

    ccxy <- crossprod(X, Y) / (n * tcrossprod(sdX, sqrt(apply(Y^2, 2, mean))))
    sdX[ inds ] <- 0
    ccxy[inds,] <- 0
    list(C = ccxy, S = sdX)
}


################
## function norm
norm <- function(vec) {
    sqrt(crossprod(vec)[1])
}


################
## Stripped version of canonical correlation (cancor)
cancorr <- function (x, y, weights, opt = TRUE) {
    nr  <- nrow(x)
    ncx <- ncol(x)
    ncy <- ncol(y)
    if (!is.null(weights)) {
        x <- x * c(weights)
        y <- y * c(weights)
    }
    qx <- qr(x, LAPACK = TRUE)
    qy <- qr(y, LAPACK = TRUE)
    qxR <- qr.R(qx)
    ## Compute rank like MATLAB does
    dx <- sum(abs(diag(qxR)) >
              .Machine$double.eps * 2^floor(log2(abs(qxR[1]))) * max(nr, ncx))
    if (!dx)
        stop("'x' has rank 0")
    qyR <- qr.R(qy)
    ## Compute rank like MATLAB does
    dy <- sum(abs(diag(qyR)) >
              .Machine$double.eps * 2^floor(log2(abs(qyR[1]))) * max(nr, ncy))
    if (!dy)
        stop("'y' has rank 0")
    dxy <- min(dx, dy)
    if (opt) {
        z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1:dx,, drop = FALSE],
                 nu = 0, nv = 0)
        ret <- max(min(z$d[1], 1), 0)
    } else {
        z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1:dx,, drop = FALSE],
                 nu = dxy, nv = 0)
        A <- backsolve((qx$qr)[1:dx,1:dx, drop = FALSE], z$u) * sqrt(nr - 1)
        if ((ncx - nrow(A)) > 0) {
            A <- rbind(A, matrix(0, ncx - nrow(A), dxy))
        }
        A[qx$pivot,] <- A
        ret <- list(A = A, r = max(min(z$d[1], 1), 0))
    }
    ret
}
