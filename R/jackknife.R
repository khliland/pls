### jackknife.R: Jackknife variance estimation of regression coefficients.

## var.jack: Calculate jackknife variance (or covariance) estimates
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
