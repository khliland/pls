### jackknife.R: Jackknife variance estimation of regression coefficients.

## var.jack: Calculate jackknife variance (or covariance) estimates


#' @title Jackknife Variance Estimates of Regression Coefficients
#'
#' @description Calculates jackknife variance or covariance estimates of regression
#' coefficients.
#'
#' The original (Tukey) jackknife variance estimator is defined as \eqn{(g-1)/g
#' \sum_{i=1}^g(\tilde\beta_{-i} - \bar\beta)^2}, where \eqn{g} is the number
#' of segments, \eqn{\tilde\beta_{-i}} is the estimated coefficient when
#' segment \eqn{i} is left out (called the jackknife replicates), and
#' \eqn{\bar\beta} is the mean of the \eqn{\tilde\beta_{-i}}.  The most common
#' case is delete-one jackknife, with \eqn{g = n}, the number of observations.
#'
#' This is the definition \code{var.jack} uses by default.
#'
#' However, Martens and Martens (2000) defined the estimator as \eqn{(g-1)/g
#' \sum_{i=1}^g(\tilde\beta_{-i} - \hat\beta)^2}, where \eqn{\hat\beta} is the
#' coefficient estimate using the entire data set.  I.e., they use the original
#' fitted coefficients instead of the mean of the jackknife replicates.  Most
#' (all?) other jackknife implementations for PLSR use this estimator.
#' \code{var.jack} can be made to use this definition with \code{use.mean =
#' FALSE}.  In practice, the difference should be small if the number of
#' observations is sufficiently large.  Note, however, that all theoretical
#' results about the jackknife refer to the `proper' definition.  (Also note
#' that this option might disappear in a future version.)
#'
#' @param object an \code{mvr} object.  A cross-validated model fitted with
#' \code{jackknife = TRUE}.
#' @param ncomp the number of components to use for estimating the
#' (co)variances
#' @param covariance logical.  If \code{TRUE}, covariances are calculated;
#' otherwise only variances.  The default is \code{FALSE}.
#' @param use.mean logical.  If \code{TRUE} (default), the mean coefficients
#' are used when estimating the (co)variances; otherwise the coefficients from
#' a model fitted to the entire data set.  See Details.
#' @return If \code{covariance} is \code{FALSE}, an \eqn{p\times q \times c}
#' array of variance estimates, where \eqn{p} is the number of predictors,
#' \eqn{q} is the number of responses, and \eqn{c} is the number of components.
#'
#' If \code{covariance} id \code{TRUE}, an \eqn{pq\times pq \times c} array of
#' variance-covariance estimates.
#' @section Warning: Note that the Tukey jackknife variance estimator is not
#' unbiased for the variance of regression coefficients (Hinkley 1977).  The
#' bias depends on the \eqn{X} matrix.  For ordinary least squares regression
#' (OLSR), the bias can be calculated, and depends on the number of
#' observations \eqn{n} and the number of parameters \eqn{k} in the mode.  For
#' the common case of an orthogonal design matrix with \eqn{\pm 1}{?1} levels,
#' the delete-one jackknife estimate equals \eqn{(n-1)/(n-k)} times the
#' classical variance estimate for the regression coefficients in OLSR.
#' Similar expressions hold for delete-d estimates.  Modifications have been
#' proposed to reduce or eliminate the bias for the OLSR case, however, they
#' depend on the number of parameters used in the model.  See e.g. Hinkley
#' (1977) or Wu (1986).
#'
#' Thus, the results of \code{var.jack} should be used with caution.
#' @author Bjørn-Helge Mevik
#' @seealso \code{\link{mvrCv}}, \code{\link{jack.test}}
#' @references Tukey J.W. (1958) Bias and Confidence in Not-quite Large
#' Samples. (Abstract of Preliminary Report).  \emph{Annals of Mathematical
#' Statistics}, \bold{29}(2), 614.
#'
#' Martens H. and Martens M. (2000) Modified Jack-knife Estimation of Parameter
#' Uncertainty in Bilinear Modelling by Partial Least Squares Regression
#' (PLSR).  \emph{Food Quality and Preference}, \bold{11}, 5--16.
#'
#' Hinkley D.V. (1977), Jackknifing in Unbalanced Situations.
#' \emph{Technometrics}, \bold{19}(3), 285--292.
#'
#' Wu C.F.J. (1986) Jackknife, Bootstrap and Other Resampling Methods in
#' Regression Analysis.  \emph{Te Annals of Statistics}, \bold{14}(4),
#' 1261--1295.
#' @keywords univar
#' @examples
#'
#' data(oliveoil)
#' mod <- pcr(sensory ~ chemical, data = oliveoil, validation = "LOO",
#'            jackknife = TRUE)
#' var.jack(mod, ncomp = 2)
#'
#' @export
var.jack <- function(object, ncomp = object$ncomp, covariance = FALSE,
                     use.mean = TRUE)
{
    if (!inherits(object, "mvr"))
        stop("Not an 'mvr' object")
    if (is.null(object$validation) || is.null(object$validation$coefficients))
        stop("'object' was not fit with jackknifing enabled")

    seglengths <- sapply(object$validation$segments, length)
    if (any(diff(seglengths) != 0))
        warning("Unequal segment lengths.  Estimator currently ignores that")
    nseg <- length(seglengths)
    if (isTRUE(use.mean)) {
        ## The `proper' version of the jackknife
        cent <-
            rowMeans(object$validation$coefficients[,,ncomp,, drop=FALSE],
                     dims = 3)
    } else {
        ## The `sloppy' version, used by e.g. Westad FIXME: ref
        cent <- object$coefficients[,,ncomp, drop=FALSE]
    }
    dnB <- dimnames(object$validation$coefficients[,,ncomp,, drop=FALSE])
    Bdiff <- object$validation$coefficients[,,ncomp,, drop=FALSE] - c(cent)
    if (isTRUE(covariance)) {
        BdiffSq <- apply(Bdiff, 3:4, function(x) tcrossprod(c(x)))
        dims <- dim(Bdiff)
        dims[1:2] <- dims[1] * dims[2]
        dim(BdiffSq) <- dims
        est <- (nseg - 1) * rowMeans(BdiffSq, dims = 3)
        if (length(dnB[[2]]) == 1) {
            nxy <- dnB[[1]]
        } else if (length(dnB[[1]]) == 1) {
            nxy <- dnB[[2]]
        } else {
            nxy <- c(t(outer(dnB[[2]], dnB[[1]], paste, sep = ":")))
        }
        dimnames(est) <- list(nxy, nxy, dnB[[3]])
    } else {
        BdiffSq <- apply(Bdiff, 3:4, function(x) c(x)^2)
        est <- (nseg - 1) * rowMeans(BdiffSq, dims = 2)
        dim(est) <- dim(cent)
        dimnames(est) <- dnB[1:3]
    }
    return(est)
}

## jack.test: Use jackknife variance estimates to test B = 0


#' @name jack.test
#' @title Jackknife approximate t tests of regression coefficients
#'
#' @description Performes approximate t tests of regression coefficients based on jackknife
#' variance estimates.
#'
#' @details \code{jack.test} uses the variance estimates from \code{var.jack} to perform
#' \eqn{t} tests of the regression coefficients.  The resulting object has a
#' print method, \code{print.jacktest}, which uses \code{\link{printCoefmat}}
#' for the actual printing.
#'
#' @aliases jack.test print.jacktest
#' @param object an \code{mvr} object.  A cross-validated model fitted with
#' \code{jackknife = TRUE}.
#' @param ncomp the number of components to use for estimating the variances
#' @param use.mean logical.  If \code{TRUE} (default), the mean coefficients
#' are used when estimating the (co)variances; otherwise the coefficients from
#' a model fitted to the entire data set.  See \code{\link{var.jack}} for
#' details.
#' @param x an \code{jacktest} object, the result of \code{jack.test}.
#' @param P.values logical.  Whether to print \eqn{p} values (default).
#' @param \dots Further arguments sent to the underlying print function
#' \code{\link{printCoefmat}}.
#' @return \code{jack.test} returns an object of class \code{"jacktest"}, with
#' components \item{coefficients }{The estimated regression coefficients}
#' \item{sd}{The square root of the jackknife variance estimates}
#' \item{tvalues}{The \eqn{t} statistics} \item{df}{The `degrees of freedom'
#' used for calculating \eqn{p} values} \item{pvalues}{The calculated \eqn{p}
#' values}
#'
#' \code{print.jacktest} returns the \code{"jacktest"} object (invisibly).
#' @section Warning: The jackknife variance estimates are known to be biased
#' (see \code{\link{var.jack}}).  Also, the distribution of the regression
#' coefficient estimates and the jackknife variance estimates are unknown (at
#' least in PLSR/PCR).  Consequently, the distribution (and in particular, the
#' degrees of freedom) of the resulting \eqn{t} statistics is unknown.  The
#' present code simply assumes a \eqn{t} distribution with \eqn{m - 1} degrees
#' of freedom, where \eqn{m} is the number of cross-validation segments.
#'
#' Therefore, the resulting \eqn{p} values should not be used uncritically, and
#' should perhaps be regarded as mere indicator of (non-)significance.
#'
#' Finally, also keep in mind that as the number of predictor variables
#' increase, the problem of multiple tests increases correspondingly.
#' @author Bjørn-Helge Mevik
#' @seealso \code{\link{var.jack}}, \code{\link{mvrCv}}
#' @references Martens H. and Martens M. (2000) Modified Jack-knife Estimation
#' of Parameter Uncertainty in Bilinear Modelling by Partial Least Squares
#' Regression (PLSR).  \emph{Food Quality and Preference}, \bold{11}, 5--16.
#' @keywords htest
#' @examples
#'
#' data(oliveoil)
#' mod <- pcr(sensory ~ chemical, data = oliveoil, validation = "LOO", jackknife = TRUE)
#' jack.test(mod, ncomp = 2)
#'
#' @export
jack.test <- function(object, ncomp = object$ncomp, use.mean = TRUE) {
    nresp <- dim(object$coefficients)[2]
    sdjack <- sqrt(var.jack(object, ncomp = ncomp, covariance = FALSE,
                           use.mean = use.mean))
    B <- coef(object, ncomp = ncomp)
    ## FIXME: This is an approximation at best:
    df <- length(object$validation$segments) - 1
    tvals <- B / sdjack
    pvals <- 2 * pt(abs(tvals), df = df, lower.tail = FALSE)
    structure(list(coefficients = B, sd = sdjack,
                   tvalues = tvals, df = df, pvalues = pvals),
              class = "jacktest")
}

## print.jacktest: Print method for jacktest objects
#' @rdname jack.test
#' @export
print.jacktest <- function(x, P.values = TRUE, ...) {
    nresp <- dim(x$coefficients)[2]
    respnames <- dimnames(x$coefficients)[[2]]
    nmod <- dim(x$coefficients)[3]
    modnames <- dimnames(x$coefficients)[[3]]
    for (resp in 1:nresp) {
        for (mod in 1:nmod) {
            if (resp > 1 || mod > 1) cat("\n")
            cat("Response ", respnames[resp], " (", modnames[mod], "):\n",
                sep = "")
            coefmat <- cbind(Estimate = x$coefficients[,resp,mod],
                             "Std. Error" = x$sd[,resp,mod],
                             Df = x$df,
                             "t value" = x$tvalues[,resp,mod],
                             "Pr(>|t|)" = x$pvalues[,resp,mod])
            printCoefmat(coefmat, P.values = isTRUE(P.values),
                         cs.ind = 1:2, tst.ind = 4, ...)
        }
    }
    invisible(x)
}
