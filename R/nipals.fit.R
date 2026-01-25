
#' @title NIPALS PLS with missing values
#'
#' @description A NIPALS implementation that tolerates \code{NA}s in both
#' \code{X} and \code{Y} by ignoring them when updating scores and loadings.
#' This is useful when the design matrix is incomplete but the number of
#' components is relatively low.
#'
#' @param X numeric matrix (or coercible) of predictors. Missing values are
#' allowed and handled internally.
#' @param Y numeric matrix (or coercible) of responses. Missing values are also
#' handled internally.
#' @param ncomp number of PLS components to extract.
#' @param center logical whether to center \code{X} and \code{Y} before
#' fitting. Means ignore missing entries.
#' @param stripped logical. If \code{TRUE} only the coefficients and the mean
#' vectors are returned.
#' @param maxiter maximum number of inner iterations to force convergence on
#' each component.
#' @param tol tolerance used to stop the inner loop when the direction vector
#' changes very little.
#' @param ... currently ignored.
#'
#' @return A list with the same components as \code{nipals.fit}, but the
#' computations never fail in the presence of missing entries.
#' @export
nipals.fit <- function(X, Y, ncomp, center = TRUE, stripped = FALSE,
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
  Yresid <- Y
  maskX <- !is.na(Xcentre)
  maskY <- !is.na(Ycentre)

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

  R <- P <- matrix(0, nrow = npred, ncol = ncomp)
  Q <- matrix(0, nrow = ncomp, ncol = nresp)
  B <- array(0, c(npred, nresp, ncomp))
  tsqs <- numeric(ncomp)

  W <- matrix(0, npred, ncomp)
  Tmat <- matrix(0, nrow = nobj, ncol = ncomp)
  U <- matrix(0, nrow = nobj, ncol = ncomp)
  if (!stripped) {
    fitted <- array(0, c(nobj, nresp, ncomp))
  }

  for (a in seq_len(ncomp)) {
    start <- Yresid[, 1]
    start[is.na(start)] <- 0
    if (sum(start * start) == 0) {
      start <- rep_len(1, nobj)
    }
    v <- normalize(start)

    w <- normalize(safe_crossprod(Xresid, v))
    if (sum(w * w) == 0) {
      warning("component ", a, " has zero loading weights", call. = FALSE)
      break
    }

    for (iter in seq_len(maxiter)) {
      t1 <- safe_matvec(Xresid, w)
      if (sum(t1 * t1) == 0) break
      z <- normalize(safe_crossprod(Yresid, t1))
      if (sum(z * z) == 0) break
      v <- normalize(safe_matvec(Yresid, z))
      if (sum(v * v) == 0) break
      w1 <- normalize(safe_crossprod(Xresid, v))
      if (sum(w1 * w1) == 0) break
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

    q <- safe_crossprod(Yresid, t1) / tt
    p <- safe_crossprod(Xresid, t1) / tt
    W[, a] <- w
    Tmat[, a] <- t1
    U[, a] <- safe_matvec(Yresid, q)
    Q[a, ] <- q
    P[, a] <- p
    tsqs[a] <- tt

    reconX <- tcrossprod(t1, p)
    reconY <- tcrossprod(t1, q)
    Xresid[maskX] <- Xresid[maskX] - reconX[maskX]
    Yresid[maskY] <- Yresid[maskY] - reconY[maskY]
  }

  R <- W %*% solve(crossprod(P, W))
  for (a in seq_len(ncomp)) {
    Qsub <- Q[seq_len(a), , drop = FALSE]
    Rsub <- R[, seq_len(a), drop = FALSE]
    B[,,a] <- Rsub %*% Qsub
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

  Xvar <- colSums(P * P) * tsqs
  Xtotvar <- sum(Xcentre[!is.na(Xcentre)]^2)

  if (stripped) {
    return(list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans))
  }

  objnames <- dnX[[1]]
  if (is.null(objnames)) objnames <- dnY[[1]]
  prednames <- dnX[[2]]
  respnames <- dnY[[2]]
  compnames <- paste("Comp", seq_len(ncomp))
  nCompnames <- paste(seq_len(ncomp), "comps")

  dimnames(Tmat) <- dimnames(U) <- list(objnames, compnames)
  dimnames(R) <- dimnames(W) <- dimnames(P) <- list(prednames, compnames)
  dimnames(Q) <- list(compnames, respnames)
  dimnames(B) <- list(prednames, respnames, nCompnames)
  dimnames(fitted) <- dimnames(residuals) <-
    list(objnames, respnames, nCompnames)
  class(Tmat) <- class(U) <- "scores"
  class(P) <- class(W) <- class(Q) <- "loadings"

  list(coefficients = B,
       scores = Tmat, loadings = P,
       loading.weights = W,
       Yscores = U, Yloadings = t(Q),
       projection = R,
       Xmeans = Xmeans, Ymeans = Ymeans,
       fitted.values = fitted, residuals = residuals,
       Xvar = Xvar, Xtotvar = Xtotvar)
}

#'
#'
#' #' @title Kernel PLS (Dayal and MacGregor)
#' #'
#' #' @description Fits a PLSR model with the kernel algorithm.
#' #'
#' #' @details This function should not be called directly, but through the generic
#' #' functions \code{plsr} or \code{mvr} with the argument
#' #' \code{method="kernelpls"} (default).  Kernel PLS is particularly efficient
#' #' when the number of objects is (much) larger than the number of variables.
#' #' The results are equal to the NIPALS algorithm.  Several different forms of
#' #' kernel PLS have been described in literature, e.g.  by De Jong and Ter
#' #' Braak, and two algorithms by Dayal and MacGregor.  This function implements
#' #' the fastest of the latter, not calculating the crossproduct matrix of X.  In
#' #' the Dyal & MacGregor paper, this is \dQuote{algorithm 1}.
#' #'
#' #' @param X a matrix of observations.  \code{NA}s and \code{Inf}s are not
#' #' allowed.
#' #' @param Y a vector or matrix of responses.  \code{NA}s and \code{Inf}s are
#' #' not allowed.
#' #' @param ncomp the number of components to be used in the modelling.
#' #' @param center logical, determines if the \eqn{X} and \eqn{Y} matrices are
#' #' mean centered or not. Default is to perform mean centering.
#' #' @param stripped logical.  If \code{TRUE} the calculations are stripped as
#' #' much as possible for speed; this is meant for use with cross-validation or
#' #' simulations when only the coefficients are needed.  Defaults to
#' #' \code{FALSE}.
#' #' @param \dots other arguments.  Currently ignored.
#' #' @return A list containing the following components is returned:
#' #' \item{coefficients}{an array of regression coefficients for 1, \ldots{},
#' #' \code{ncomp} components.  The dimensions of \code{coefficients} are
#' #' \code{c(nvar, npred, ncomp)} with \code{nvar} the number of \code{X}
#' #' variables and \code{npred} the number of variables to be predicted in
#' #' \code{Y}.} \item{scores}{a matrix of scores.} \item{loadings}{a matrix of
#' #' loadings.} \item{loading.weights}{a matrix of loading weights.}
#' #' \item{Yscores}{a matrix of Y-scores.} \item{Yloadings}{a matrix of
#' #' Y-loadings.} \item{projection}{the projection matrix used to convert X to
#' #' scores.} \item{Xmeans}{a vector of means of the X variables.}
#' #' \item{Ymeans}{a vector of means of the Y variables.} \item{fitted.values}{an
#' #' array of fitted values.  The dimensions of \code{fitted.values} are
#' #' \code{c(nobj, npred, ncomp)} with \code{nobj} the number samples and
#' #' \code{npred} the number of Y variables.} \item{residuals}{an array of
#' #' regression residuals.  It has the same dimensions as \code{fitted.values}.}
#' #' \item{Xvar}{a vector with the amount of X-variance explained by each
#' #' component.} \item{Xtotvar}{Total variance in \code{X}.}
#' #'
#' #' If \code{stripped} is \code{TRUE}, only the components \code{coefficients},
#' #' \code{Xmeans} and \code{Ymeans} are returned.
#' #' @author Ron Wehrens and Bjørn-Helge Mevik
#' #' @seealso \code{\link{mvr}} \code{\link{plsr}} \code{\link{cppls}}
#' #' \code{\link{pcr}} \code{\link{widekernelpls.fit}} \code{\link{simpls.fit}}
#' #' \code{\link{oscorespls.fit}}
#' #' @references de Jong, S. and ter Braak, C. J. F. (1994) Comments on the PLS
#' #' kernel algorithm.  \emph{Journal of Chemometrics}, \bold{8}, 169--174.
#' #'
#' #' Dayal, B. S. and MacGregor, J. F. (1997) Improved PLS algorithms.
#' #' \emph{Journal of Chemometrics}, \bold{11}, 73--85.
#' #' @keywords regression multivariate
#' #' @export
#' nipals.fit <- function(X, Y, ncomp, center = TRUE,
#'                           stripped = FALSE, ...)
#' {
#'     Y <- as.matrix(Y)
#'     if (!stripped) {
#'         ## Save dimnames:
#'         dnX <- dimnames(X)
#'         dnY <- dimnames(Y)
#'     }
#'     ## Remove dimnames during calculation.  (Doesn't seem to make a
#'     ## difference here (2.3.0).)
#'     dimnames(X) <- dimnames(Y) <- NULL
#'
#'     nobj  <- dim(X)[1]
#'     npred <- dim(X)[2]
#'     nresp <- dim(Y)[2]
#'
#'     ## Center variables:
#'     if (center) {
#'         Xmeans <- colMeans(X)
#'         X <- X - rep(Xmeans, each = nobj)
#'         Ymeans <- colMeans(Y)
#'         Y <- Y - rep(Ymeans, each = nobj)
#'     } else {
#'         ## Set means to zero. Will ensure that predictions do not take the
#'         ## mean into account.
#'         Xmeans <- rep_len(0, npred)
#'         Ymeans <- rep_len(0, nresp)
#'     }
#'     X.orig <- X
#'
#'     ## Projection, loadings
#'     R <- P <- matrix(0, ncol = ncomp, nrow = npred)
#'     Q <- matrix(0, ncol = nresp, nrow = ncomp)# Y loadings; transposed
#'     B <- array(0, c(npred, nresp, ncomp))
#'
#'     if (!stripped) {
#'         W <- P                        # Loading weights
#'         U <- T <- matrix(0, ncol = ncomp, nrow = nobj)# scores
#'         fitted <- array(0, c(nobj, nresp, ncomp))
#'     }
#'
#'     for(a in 1:ncomp){
#'       v <- Y[,1]
#'       w <- crossprod(X,v);  w  <- w/sqrt(sum(w*w))
#'       u <- X %*% w;         u  <- u/sqrt(sum(u*u))
#'       z <- crossprod(Y,u);  z  <- z/sqrt(sum(z*z))
#'       v <- Y %*% z;         v  <- v/sqrt(sum(v*v))
#'       w1 <- crossprod(X,v); w1 <- w1/sqrt(sum(w1*w1))
#'       while(abs(crossprod(w1,w)-crossprod(w1)) > 0.0001){
#'         w <- w1
#'         u <- X %*% w;         u  <- u/sqrt(sum(u*u))
#'         z <- crossprod(Y,u);  z  <- z/sqrt(sum(z*z))
#'         v <- Y %*% z;         v  <- v/sqrt(sum(v*v))
#'         w1 <- crossprod(X,v); w1 <- w1/sqrt(sum(w1*w1))
#'       }
#'       W[,a] <- w1
#'       t1 <- X %*% w1
#'       T[,a] <- t1
#'       tt <- sum(t1*t1)
#'       Q[,a] <- crossprod(Y, t1)/tt
#'       P[,a] <- crossprod(X, t1)/tt
#'       X <- X - tcrossprod(t1, P[,a])
#'       Y <- Y - tcrossprod(t1, Q[,a])
#'
#'     }
#'     R <- W %*% solve(crossprod(P,W))
#'     for(a in 1:ncomp){
#'       B[,,a] <- tcrossprod(R[,1:a], Q[,1:a])
#'       fitted[,,a] <- X.orig %*% B[,,a]
#'     }
#'
#'     if (stripped) {
#'         ## Return as quickly as possible
#'         list(coefficients = B, Xmeans = Xmeans, Ymeans = Ymeans)
#'     } else {
#'         residuals <- - fitted + c(Y)
#'         if (center) {
#'             fitted <- fitted + rep(Ymeans, each = nobj) # Add mean
#'         }
#'
#'         ## Add dimnames:
#'         objnames <- dnX[[1]]
#'         if (is.null(objnames)) objnames <- dnY[[1]]
#'         prednames <- dnX[[2]]
#'         respnames <- dnY[[2]]
#'         compnames <- paste("Comp", 1:ncomp)
#'         nCompnames <- paste(1:ncomp, "comps")
#'         dimnames(TT) <- dimnames(U) <- list(objnames, compnames)
#'         dimnames(R) <- dimnames(W) <- dimnames(P) <-
#'             list(prednames, compnames)
#'         dimnames(tQ) <- list(compnames, respnames)
#'         dimnames(B) <- list(prednames, respnames, nCompnames)
#'         dimnames(fitted) <- dimnames(residuals) <-
#'             list(objnames, respnames, nCompnames)
#'         class(TT) <- class(U) <- "scores"
#'         class(P) <- class(W) <- class(tQ) <- "loadings"
#'
#'         list(coefficients = B,
#'              scores = TT, loadings = P,
#'              loading.weights = W,
#'              Yscores = U, Yloadings = Q,
#'              projection = R,
#'              Xmeans = Xmeans, Ymeans = Ymeans,
#'              fitted.values = fitted, residuals = residuals,
#'              Xvar = colSums(P * P) * tsqs,
#'              Xtotvar = sum(X * X))
#'     }
#' }

