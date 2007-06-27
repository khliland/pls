### jackknife.R: Jackknife variance estimation of regression coefficients.
### $Id$

varJack <- function(object, covariance = TRUE, use.mean = TRUE) {
    if (!inherits(object, "mvr"))
        stop("Not an 'mvr' object")
    if (is.null(object$validation) || is.null(object$validation$coefficients))
        stop("'object' was not fit with jackknifing enabled")

    seglengths <- sapply(object$validation$segments, length)
    if (any(diff(seglengths) != 0))
        warning("Unequal segment lengths.  Estimate currently ignores that")
    nseg <- length(seglengths)
    if (isTRUE(use.mean)) {
        ## The `proper' version of the jackknife
        cent <- rowMeans(object$validation$coefficients, dims = 3)
    } else {
        ## The `sloppy' version, used by e.g. Westad FIXME: ref
        cent <- object$coefficients
    }
    Bdiff <- object$validation$coefficients - c(cent)
    BdiffSq <- apply(Bdiff, 3:4, function(x) tcrossprod(c(x)))
    dims <- dim(Bdiff)
    dims[1:2] <- dims[1] * dims[2]
    dim(BdiffSq) <- dims
    est <- (nseg - 1) * rowMeans(BdiffSq, dims = 3)
    if (!isTRUE(covariance))
        est <- apply(est, 3, diag)
    return(est)
}

### FIXME: Perhaps model the function arguments and return values more after
### the standard hypothesis test functions (t.test, wilcox.test,
### kruskal.test, etc.)
jack.test <- function(object, ncomp = object$ncomp, use.mean = TRUE) {
    nresp <- dim(object$coefficients)[2]
    sdjack <- sqrt(varJack(object, covariance = FALSE,
                           use.mean = use.mean)[,ncomp])
    B <- coef(object, ncomp = ncomp)
    df <- length(object$validation$segments) - 1
    tvals <- B / sdjack
    pvals <- 2 * pt(abs(tvals), df = df, lower.tail = FALSE)
    structure(list(coefficients = B, sd = matrix(sdjack, ncol = nresp),
                   tvalues = tvals, df = df, pvalues = pvals),
              class = "jacktest")
}

### FIXME: Se print.summary.mlm / print.summary.lm for en bedre løsning.
### Also: perhaps see print.htest
print.jacktest <- function(x, ...) {
    nresp <- dim(x$coefficients)[2]
    respnames <- dimnames(x$coefficients)[[2]]
    for (resp in 1:nresp) {
        if (resp > 1) cat("\n")
        cat("Response ", respnames[resp], ":\n", sep = "")
        anovatab <- cbind(Estimate = x$coefficients[,resp,],
                          "Std. Error" = x$sd[,resp],
                          Df = x$df,
                          "t value" = x$tvalues[,resp,],
                          "Pr(>|t|)" = x$pvalues[,resp,])
	class(anovatab) <- "anova"
	print(anovatab)
    }
    invisible(x)
}
