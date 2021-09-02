### kernelpls.fit.R: Kernel PLS fit algorithm for tall data.
###
### Implements an adapted version of the `algorithm 1' described in
###   Dayal, B. S. and MacGregor, J. F. (1997) Improved PLS algorithms.
###   \emph{Journal of Chemometrics}, \bold{11}, 73--85.
### (This is a modification of the algorithm described in
###   Lindgren F, Geladi P, Wold S (1993) The kernel algorithm for PLS.
###   J. Chemometrics 7, 45-59,
### incorporating the changes in
###   de Jong, S. and ter Braak,  C. J. F. (1994) Comments on the PLS kernel
###   algorithm.  \emph{Journal of Chemometrics}, \bold{8}, 169--174.



#' @title Kernel PLS (Dayal and MacGregor)
#'
#' @description Fits a PLSR model with the kernel algorithm.
#'
#' @details This function should not be called directly, but through the generic
#' functions \code{plsr} or \code{mvr} with the argument
#' \code{method="kernelpls"} (default).  Kernel PLS is particularly efficient
#' when the number of objects is (much) larger than the number of variables.
#' The results are equal to the NIPALS algorithm.  Several different forms of
#' kernel PLS have been described in literature, e.g.  by De Jong and Ter
#' Braak, and two algorithms by Dayal and MacGregor.  This function implements
#' the fastest of the latter, not calculating the crossproduct matrix of X.  In
#' the Dyal & MacGregor paper, this is \dQuote{algorithm 1}.
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
#' @seealso \code{\link{mvr}} \code{\link{plsr}} \code{\link{cppls}}
#' \code{\link{pcr}} \code{\link{widekernelpls.fit}} \code{\link{simpls.fit}}
#' \code{\link{oscorespls.fit}}
#' @references de Jong, S. and ter Braak, C. J. F. (1994) Comments on the PLS
#' kernel algorithm.  \emph{Journal of Chemometrics}, \bold{8}, 169--174.
#'
#' Dayal, B. S. and MacGregor, J. F. (1997) Improved PLS algorithms.
#' \emph{Journal of Chemometrics}, \bold{11}, 73--85.
#' @keywords regression multivariate
#' @export
kernelpls.fit <- function(X, Y, ncomp, center = TRUE,
                          stripped = FALSE, ...)
{
    Y <- as.matrix(Y)
    if (!stripped) {
        ## Save dimnames:
        dnX <- dimnames(X)
        dnY <- dimnames(Y)
    }
    ## Remove dimnames during calculation.  (Doesn't seem to make a
    ## difference here (2.3.0).)
    dimnames(X) <- dimnames(Y) <- NULL

    nobj  <- dim(X)[1]
    npred <- dim(X)[2]
    nresp <- dim(Y)[2]

    ## Center variables:
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

    ## Projection, loadings
    R <- P <- matrix(0, ncol = ncomp, nrow = npred)
    tQ <- matrix(0, ncol = nresp, nrow = ncomp)# Y loadings; transposed
    B <- array(0, c(npred, nresp, ncomp))

    if (!stripped) {
        W <- P                        # Loading weights
        U <- TT <- matrix(0, ncol = ncomp, nrow = nobj)# scores
        tsqs <- rep.int(1, ncomp)       # t't
        fitted <- array(0, c(nobj, nresp, ncomp))
    }

    ## 1.
    XtY <- crossprod(X, Y)

    for (a in 1:ncomp) {
        ## 2.
        if (nresp == 1) {
            w.a <- XtY / sqrt(c(crossprod(XtY)))
        } else {
            if (nresp < npred) {
                ## FIXME: is q proportional to q.a?
                q <- eigen(crossprod(XtY), symmetric = TRUE)$vectors[,1]
                w.a <- XtY %*% q
                w.a <- w.a / sqrt(c(crossprod(w.a)))
            } else {
                w.a <- eigen(XtY %*% t(XtY), symmetric = TRUE)$vectors[,1]
            }
        }

        ## 3.
        r.a <- w.a
        if (a > 5) {
            ## This is faster when a > 5:
            r.a <- r.a - colSums(crossprod(w.a, P[,1:(a-1), drop=FALSE]) %*%
                               t(R[,1:(a-1), drop=FALSE]))
        } else if (a > 1) {
            for (j in 1:(a - 1))
                r.a <- r.a - c(P[,j] %*% w.a) * R[,j]
        }

        ## 4.
        t.a <- X %*% r.a
        tsq <- c(crossprod(t.a))
        p.a <- crossprod(X, t.a) / tsq
        q.a <- crossprod(XtY, r.a) / tsq

        ## 5.
        XtY <- XtY - (tsq * p.a) %*% t(q.a)

        ## 6.-8.
        R[,a]  <- r.a
        P[,a]  <- p.a
        tQ[a,] <- q.a
        B[,,a] <- R[,1:a, drop=FALSE] %*% tQ[1:a,, drop=FALSE]
        if (!stripped) {
            tsqs[a] <- tsq
            ## Extra step to calculate Y scores:
            u.a <- Y %*% q.a / c(crossprod(q.a)) # Ok for nresp == 1 ??
            ## make u orth to previous X scores:
            if (a > 1) u.a <- u.a - TT %*% (crossprod(TT, u.a) / tsqs)
            U[,a]  <- u.a
            TT[,a] <- t.a
            W[,a]  <- w.a
            ## (For very tall, slim X and Y, X %*% B[,,a] is slightly faster
            ## due to less overhead.)
            fitted[,,a] <- TT[,1:a] %*% tQ[1:a,, drop=FALSE]
        }
    }

    if (stripped) {
        ## Return as quickly as possible
        list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans)
    } else {
        residuals <- - fitted + c(Y)
        if (center) {
            fitted <- fitted + rep(Ymeans, each = nobj) # Add mean
        }

        ## Add dimnames:
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
             Xtotvar = sum(X * X))
    }
}
