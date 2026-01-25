### svdpc.fit.R: SVD PC fit algorithm



#' @title Principal Component Regression
#'
#' @description Fits a PCR model using the singular value decomposition.
#'
#' @details This function should not be called directly, but through the generic
#' functions \code{pcr} or \code{mvr} with the argument \code{method="svdpc"}.
#' The singular value decomposition is used to calculate the principal
#' components.
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
#' loadings.} \item{Yloadings}{a matrix of Y-loadings.} \item{projection}{the
#' projection matrix used to convert X to scores.} \item{Xmeans}{a vector of
#' means of the X variables.} \item{Ymeans}{a vector of means of the Y
#' variables.} \item{fitted.values}{an array of fitted values.  The dimensions
#' of \code{fitted.values} are \code{c(nobj, npred, ncomp)} with \code{nobj}
#' the number samples and \code{npred} the number of Y variables.}
#' \item{residuals}{an array of regression residuals.  It has the same
#' dimensions as \code{fitted.values}.} \item{Xvar}{a vector with the amount of
#' X-variance explained by each component.} \item{Xtotvar}{Total variance in
#' \code{X}.}
#'
#' If \code{stripped} is \code{TRUE}, only the components \code{coefficients},
#' \code{Xmeans} and \code{Ymeans} are returned.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}} \code{\link{plsr}} \code{\link{pcr}}
#' \code{\link{cppls}}
#' @references Martens, H., Næs, T. (1989) \emph{Multivariate calibration.}
#' Chichester: Wiley.
#' @keywords regression multivariate
#' @export
svdpc.fit <- function(X, Y, ncomp, center = TRUE, stripped = FALSE, ...) {
    Y <- as.matrix(Y)
    if (!stripped) {
        ## Save dimnames:
        dnX <- dimnames(X)
        dnY <- dimnames(Y)
    }
    ## Remove dimnames during calculation  (doesn't seem to matter; in fact,
    ## as far as it has any effect, it hurts a tiny bit in most situations).
    ## dimnames(X) <- dimnames(Y) <- NULL

    nobj  <- dim(X)[1]
    npred <- dim(X)[2]
    nresp <- dim(Y)[2]

    B <- array(0, dim = c(npred, nresp, ncomp))
    if (!stripped) fitted <- array(0, dim = c(nobj, nresp, ncomp))

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

    huhn <- La.svd(X)
    D  <- huhn$d[1:ncomp]
    TT <- huhn$u[,1:ncomp, drop=FALSE] %*% diag(D, nrow = ncomp)
    P  <- t(huhn$vt[1:ncomp,, drop=FALSE])
    tQ <- crossprod(TT, Y) / D^2

    for (a in 1:ncomp) {
        B[,,a] <- P[,1:a, drop=FALSE] %*% tQ[1:a,]
        if (!stripped) fitted[,,a] <- TT[,1:a, drop=FALSE] %*% tQ[1:a,]
    }

    if (stripped) {
        ## Return as quickly as possible
        list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans)
    } else {
        residuals <- c(Y) - fitted
        fitted <- fitted + rep(Ymeans, each = nobj) # Add mean

        ## Add dimnames and classes:
        objnames <- dnX[[1]]
        if (is.null(objnames)) objnames <- dnY[[1]]
        prednames <- dnX[[2]]
        respnames <- dnY[[2]]
        compnames <- paste("Comp", 1:ncomp)
        nCompnames <- paste(1:ncomp, "comps")
        dimnames(TT) <- list(objnames, compnames)
        dimnames(P) <- list(prednames, compnames)
        dimnames(tQ) <- list(compnames, respnames)
        dimnames(B) <- list(prednames, respnames, nCompnames)
        dimnames(fitted) <- dimnames(residuals) <-
            list(objnames, respnames, nCompnames)
        names(D) <- compnames
        class(TT) <- "scores"
        R <- P                          # To avoid class "loadings" on projection
        class(P) <- class(tQ) <- "loadings"

        list(coefficients = B,
             scores = TT, loadings = P,
             Yloadings = t(tQ),
             projection = R,
             Xmeans = Xmeans, Ymeans = Ymeans,
             fitted.values = fitted, residuals = residuals,
             Xvar = D^2, Xtotvar = sum(X * X))
    }
}


#' @title NIPALS PCR with missing values
#'
#' @description A NIPALS-based PCR that tolerates missing entries in both
#' predictors and responses by only using observed cells when updating scores
#' and loadings.  It follows the same API as \code{svdpc.fit} so it can be used
#' whenever low-level PCR needs to handle incomplete data.
#'
#' @param X numeric matrix (or coercible) of predictors. Missing values are
#' allowed and handled internally.
#' @param Y numeric matrix (or coercible) of responses. Missing values are also
#' handled internally during the final regression step.
#' @param ncomp number of PCR components to extract.
#' @param center logical. If \code{TRUE} both \code{X} and \code{Y} are
#' centered column-wise (ignoring missing entries).
#' @param stripped logical. When \code{TRUE} only the coefficients and mean
#' vectors are returned for faster use in resampling.
#' @param maxiter maximum number of inner iterations per component.
#' @param tol convergence tolerance used when the direction vector stabilizes.
#' @param ... currently ignored.
#'
#' @return A list mirroring the return value of \code{svdpc.fit} but computed
#' via the NA-robust NIPALS PCR updates.
#' @export
nipalspc.fit <- function(X, Y, ncomp, center = TRUE, stripped = FALSE,
                         maxiter = 500, tol = 1e-06, ...)
{
    X <- as.matrix(X)
    Y <- as.matrix(Y)
    if (!stripped) {
        dnX <- dimnames(X)
        dnY <- dimnames(Y)
    }
    dimnames(X) <- dimnames(Y) <- NULL

    nobj <- nrow(X)
    npred <- ncol(X)
    nresp <- ncol(Y)

    mean_ignore_na <- function(mat) {
        colMeans(mat, na.rm = TRUE)
    }

    if (center) {
        Xmeans <- mean_ignore_na(X)
        Ymeans <- mean_ignore_na(Y)
        X <- sweep(X, 2, Xmeans, "-")
        Y <- sweep(Y, 2, Ymeans, "-")
    } else {
        Xmeans <- rep_len(0, npred)
        Ymeans <- rep_len(0, nresp)
    }

    Xcentre <- X
    Ycentre <- Y
    Xresid <- X
    maskX <- !is.na(Xcentre)

    safe_crossprod <- function(mat, vec) {
        apply(mat, 2, function(col) sum(col * vec, na.rm = TRUE))
    }

    safe_matvec <- function(mat, vec) {
        res <- numeric(nrow(mat))
        for (j in seq_len(ncol(mat))) {
            col <- mat[, j]
            idx <- !is.na(col)
            if (any(idx)) {
                res[idx] <- res[idx] + col[idx] * vec[j]
            }
        }
        res
    }

    safe_times <- function(mat, coeff) {
        res <- matrix(0, nrow = nrow(mat), ncol = ncol(coeff))
        for (j in seq_len(ncol(coeff))) {
            res[, j] <- safe_matvec(mat, coeff[, j])
        }
        res
    }

    normalize <- function(v) {
        n <- sum(v * v)
        if (n <= .Machine$double.eps) return(v)
        v / sqrt(n)
    }

    TT <- matrix(0, nrow = nobj, ncol = ncomp)
    P <- matrix(0, nrow = npred, ncol = ncomp)
    tsqs <- numeric(ncomp)
    B <- array(0, c(npred, nresp, ncomp))
    tQ <- matrix(0, nrow = ncomp, ncol = nresp)
    nused <- 0
    if (!stripped) {
        fitted <- array(0, c(nobj, nresp, ncomp))
    }

    for (a in seq_len(ncomp)) {
        start <- Xresid[, 1]
        start[is.na(start)] <- 0
        if (sum(start * start) == 0) {
            start <- rep_len(1, nobj)
        }
        w <- normalize(safe_crossprod(Xresid, start))
        if (sum(w * w) == 0) {
            warning("component ", a, " has zero loading weights", call. = FALSE)
            break
        }

        for (iter in seq_len(maxiter)) {
            t1 <- safe_matvec(Xresid, w)
            if (sum(t1 * t1) == 0) break
            p <- safe_crossprod(Xresid, t1) / sum(t1 * t1)
            w1 <- normalize(p)
            if (sum((w1 - w)^2) < tol^2) {
                w <- w1
                break
            }
            w <- w1
        }

        t1 <- safe_matvec(Xresid, w)
        tt <- sum(t1 * t1)
        if (tt == 0) {
            warning("component ", a, " collapsed to zero scores", call. = FALSE)
            break
        }

        p <- safe_crossprod(Xresid, t1) / tt
        TT[, a] <- t1
        P[, a] <- p
        tsqs[a] <- tt
        nused <- a

        reconX <- tcrossprod(t1, p)
        Xresid[maskX] <- Xresid[maskX] - reconX[maskX]
    }

    for (a in seq_len(nused)) {
        if (tsqs[a] <= 0) break
        tQ[a, ] <- safe_crossprod(Ycentre, TT[, a]) / tsqs[a]
    }

    for (a in seq_len(ncomp)) {
        if (a > nused) break
        B[,,a] <- P[, 1:a, drop = FALSE] %*% tQ[1:a, , drop = FALSE]
    }

    if (!stripped) {
        for (a in seq_len(ncomp)) {
            fitted[,,a] <- safe_times(Xcentre, matrix(B[,,a]))
        }
        residuals <- array(NA, c(nobj, nresp, ncomp))
        for (a in seq_len(ncomp)) {
            residuals[,,a] <- Ycentre - fitted[,,a]
        }
        if (center) {
            fitted <- fitted + rep(Ymeans, each = nobj)
        }
    }

    Xvar <- tsqs
    Xtotvar <- sum(Xcentre[maskX]^2)

    if (stripped) {
        return(list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans))
    }

    objnames <- dnX[[1]]
    if (is.null(objnames)) objnames <- dnY[[1]]
    prednames <- dnX[[2]]
    respnames <- dnY[[2]]
    compnames <- paste("Comp", seq_len(ncomp))
    nCompnames <- paste(seq_len(ncomp), "comps")

    dimnames(TT) <- list(objnames, compnames)
    dimnames(P) <- list(prednames, compnames)
    dimnames(tQ) <- list(compnames, respnames)
    dimnames(B) <- list(prednames, respnames, nCompnames)
    dimnames(fitted) <- dimnames(residuals) <-
        list(objnames, respnames, nCompnames)
    names(Xvar) <- compnames
    class(TT) <- "scores"
    R <- P
    class(P) <- class(tQ) <- "loadings"

    list(coefficients = B,
         scores = TT, loadings = P,
         Yloadings = t(tQ),
         projection = R,
         Xmeans = Xmeans, Ymeans = Ymeans,
         fitted.values = fitted, residuals = residuals,
         Xvar = Xvar, Xtotvar = Xtotvar)
}
