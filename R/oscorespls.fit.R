### oscorespls.fit.R: The multiresponse orthogonal scores algorithm
###
### Implements an adapted version of the `orthogonal scores' algorithm as
###   described in Martens and Naes, pp. 121--122 and 157--158.



#' @title Orthogonal scores PLSR
#'
#' @description Fits a PLSR model with the orthogonal scores algorithm (aka the NIPALS
#' algorithm).
#'
#' @details This function should not be called directly, but through the generic
#' functions \code{plsr} or \code{mvr} with the argument
#' \code{method="oscorespls"}.  It implements the orthogonal scores algorithm,
#' as described in \cite{Martens and Næs (1989)}.  This is one of the two
#' \dQuote{classical} PLSR algorithms, the other being the orthogonal loadings
#' algorithm.
#'
#' @param X a matrix of observations.  \code{NA}s and \code{Inf}s are not
#' allowed.
#' @param Y a vector or matrix of responses.  \code{NA}s and \code{Inf}s are
#' not allowed.
#' @param ncomp the number of components to be used in the modelling.
#' @param center logical, determines if the \eqn{X} and \eqn{Y} matrices are
#' mean centered or not. Default is to perform mean centering.
#' @param stripped logical.  If \code{TRUE} the calculations are stripped as
#' much as possible for speed; this is meant for use with cross-validation or
#' simulations when only the coefficients are needed.  Defaults to
#' \code{FALSE}.
#' @param tol numeric.  The tolerance used for determining convergence in
#' multi-response models.
#' @param maxit positive integer.  The maximal number of iterations used in the
#' internal Eigenvector calculation.
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
#' component.} \item{Xtotvar}{Total variance in \code{X}.}
#'
#' If \code{stripped} is \code{TRUE}, only the components \code{coefficients},
#' \code{Xmeans} and \code{Ymeans} are returned.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}} \code{\link{plsr}} \code{\link{pcr}}
#' \code{\link{kernelpls.fit}} \code{\link{widekernelpls.fit}}
#' \code{\link{simpls.fit}}
#' @references Martens, H., Næs, T. (1989) \emph{Multivariate calibration.}
#' Chichester: Wiley.
#' @keywords regression multivariate
#' @export
oscorespls.fit <- function(X, Y, ncomp, center = TRUE, stripped = FALSE,
                           tol = .Machine$double.eps^0.5, maxit = 100, ...)
{
    ## Initialise
    Y <- as.matrix(Y)
    if (!stripped) {
        ## Save dimnames
        dnX <- dimnames(X)
        dnY <- dimnames(Y)
    }
    ## Remove dimnames for performance (doesn't seem to matter; in fact,
    ## as far as it has any effect, it hurts a tiny bit in most situations.
    dimnames(X) <- dimnames(Y) <- NULL

    nobj  <- dim(X)[1]
    npred <- dim(X)[2]
    nresp <- dim(Y)[2]

    W <- P <- matrix(0, nrow = npred, ncol = ncomp)
    tQ <- matrix(0, nrow = ncomp, ncol = nresp) # Y loadings; transposed
    B <- array(0, dim = c(npred, nresp, ncomp))
    if (!stripped) {
        TT <- U <- matrix(0, nrow = nobj, ncol = ncomp)
        tsqs <- numeric(ncomp)          # t't
        fitted <- residuals <- array(0, dim = c(nobj, nresp, ncomp))
    }

    ## C1
    if (center) {
        Xmeans <- colMeans(X)
        X <- X - rep(Xmeans, each = nobj)
        Ymeans <- colMeans(Y)
        Y <- Y - rep(Ymeans, each = nobj)
    } else {
        ## Set means to zero. Will ensure that predictions do not take the
        ## mean into account.
        Xmeans <- rep_len(0, npred)
        Ymeans <- rep_len(0, nresp)
    }

    ## Must be done here due to the deflation of X
    if (!stripped) Xtotvar <- sum(X * X)

    for (a in 1:ncomp) {
        ## Initial values:
        if (nresp == 1) {               # pls1
            u.a <- Y                    # FIXME: scale?
        } else {                        # pls2
            ## The coloumn of Y with largest sum of squares:
            u.a <- Y[,which.max(colSums(Y * Y))]
            t.a.old <- 0
        }
        nit <- 0
        repeat {
            nit <- nit + 1              # count the iterations
            ## C2.1
            w.a <- crossprod(X, u.a)
            w.a <- w.a / sqrt(c(crossprod(w.a)))

            ## C2.2
            t.a <- X %*% w.a

            ## C2.3
            tsq <- c(crossprod(t.a))
            t.tt <- t.a / tsq

            ## C2.4
            q.a <- crossprod(Y, t.tt)

            if (nresp == 1)
                break                   # pls1: no iteration

            ## C2.4b-c
            ## Convergence check for pls2:
            if (sum(abs((t.a - t.a.old) / t.a), na.rm = TRUE) < tol)
                break
            else {
                u.a <- Y %*% q.a / c(crossprod(q.a))
                t.a.old <- t.a          # Save for comparison
            }
            if (nit >= maxit) {
              warning("No convergence in ", maxit, " iterations\n")
              break
            }
        }

        ## C2.3 contd.
        p.a <- crossprod(X, t.tt)

        ## C2.5
        X <- X - t.a %*% t(p.a)
        Y <- Y - t.a %*% t(q.a)

        ## Save scores etc:
        W[,a]  <- w.a
        P[,a]  <- p.a
        tQ[a,] <- q.a
        if (!stripped) {
            TT[,a] <- t.a
            U[,a]  <- u.a
            tsqs[a] <- tsq
            ## (For very tall, slim X and Y, X0 %*% B[,,a] is slightly faster,
            ## due to less overhead.)
            fitted[,,a] <- TT[,1:a] %*% tQ[1:a,, drop=FALSE]
            residuals[,,a] <- Y
        }
    }

    ## Calculate rotation matrix:
    if (ncomp == 1) {
        ## For 1 component, R == W:
        R <- W
    } else {
        PW <- crossprod(P, W)
        ## It is known that P^tW is right bi-diagonal (one response) or upper
        ## triangular (multiple responses), with all diagonal elements equal to 1.
        if (nresp == 1) {
            ## For single-response models, direct calculation of (P^tW)^-1 is
            ## simple, and faster than using backsolve.
            PWinv <- diag(ncomp)
            bidiag <- - PW[row(PW) == col(PW)-1]
            for (a in 1:(ncomp - 1))
                PWinv[a,(a+1):ncomp] <- cumprod(bidiag[a:(ncomp-1)])
        } else {
            PWinv <- backsolve(PW, diag(ncomp))
        }
    R <- W %*% PWinv
    }

    ## Calculate regression coefficients:
    for (a in 1:ncomp) {
        B[,,a] <- R[,1:a, drop=FALSE] %*% tQ[1:a,, drop=FALSE]
    }

    if (stripped) {
        ## Return as quickly as possible
        list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans)
    } else {
        fitted <- fitted + rep(Ymeans, each = nobj) # Add mean

        ## Add dimnames and classes:
        objnames <- dnX[[1]]
        if (is.null(objnames)) objnames <- dnY[[1]]
        prednames <- dnX[[2]]
        respnames <- dnY[[2]]
        compnames <- paste("Comp", 1:ncomp)
        nCompnames <- paste(1:ncomp, "comps")
        dimnames(TT) <- dimnames(U) <- list(objnames, compnames)
        dimnames(R) <- dimnames(W) <- dimnames(P) <-
            list(prednames, compnames)
        dimnames(tQ) <- list(compnames, respnames)
        dimnames(B) <- list(prednames, respnames, nCompnames)
        dimnames(fitted) <- dimnames(residuals) <-
            list(objnames, respnames, nCompnames)
        class(TT) <- class(U) <- "scores"
        class(P) <- class(W) <- class(tQ) <- "loadings"

        list(coefficients = B,
             scores = TT, loadings = P,
             loading.weights = W,
             Yscores = U, Yloadings = t(tQ),
             projection = R,
             Xmeans = Xmeans, Ymeans = Ymeans,
             fitted.values = fitted, residuals = residuals,
             Xvar = colSums(P * P) * tsqs,
             Xtotvar = Xtotvar)
    }
}
