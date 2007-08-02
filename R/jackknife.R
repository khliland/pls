### jackknife.R: Jackknife variance estimation of regression coefficients.
### $Id$

varJack <- function(object, ncomp = object$ncomp, covariance = FALSE,
                    use.mean = TRUE) {
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
        dim(est) <- c(dim(cent)[1:2], dim(cent))
        dimnames(est) <- c(dnB[1:2], dnB[1:3])
    } else {
        BdiffSq <- apply(Bdiff, 3:4, function(x) c(x)^2)
        est <- (nseg - 1) * rowMeans(BdiffSq, dims = 2)
        dim(est) <- dim(cent)
        dimnames(est) <- dnB[1:3]
    }
    return(est)
}

### FIXME: Perhaps model the function arguments and return values more after
### the standard hypothesis test functions (t.test, wilcox.test,
### kruskal.test, etc.)
jack.test <- function(object, ncomp = object$ncomp, use.mean = TRUE) {
    nresp <- dim(object$coefficients)[2]
    sdjack <- sqrt(varJack(object, ncomp = ncomp, covariance = FALSE,
                           use.mean = use.mean))
    B <- coef(object, ncomp = ncomp)
    df <- length(object$validation$segments) - 1
    tvals <- B / sdjack
    pvals <- 2 * pt(abs(tvals), df = df, lower.tail = FALSE)
    structure(list(coefficients = B, sd = sdjack,
                   tvalues = tvals, df = df, pvalues = pvals),
              class = "jacktest")
}

### FIXME: Se print.summary.mlm / print.summary.lm for en bedre løsning.
### Also: perhaps see print.htest
print.jacktest <- function(x, ...) {
    nresp <- dim(x$coefficients)[2]
    respnames <- dimnames(x$coefficients)[[2]]
    nmod <- dim(x$coefficients)[3]
    modnames <- dimnames(x$coefficients)[[3]]
    for (resp in 1:nresp) {
        for (mod in 1:nmod) {
            if (resp > 1 || mod > 1) cat("\n")
            cat("Response ", respnames[resp], " (", modnames[mod], "):\n",
                sep = "")
            anovatab <- cbind(Estimate = x$coefficients[,resp,mod],
                              "Std. Error" = x$sd[,resp,mod],
                              Df = x$df,
                              "t value" = x$tvalues[,resp,mod],
                              "Pr(>|t|)" = x$pvalues[,resp,mod])
            class(anovatab) <- "anova"
            print(anovatab)
        }
    }
    invisible(x)
}
