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
