### summaries.R: print and summary methods.
### $Id$

## Print method for mvr objects:
print.mvr <- function(x, ...) {
    switch(x$method,
           kernelpls = {
               regr = "Partial least squares"
               alg = "kernel"
           },
           simpls = {
               regr = "Partial least squares"
               alg = "simpls"
           },
           oscorespls = {
               regr = "Partial least squares"
               alg = "orthogonal scores"
           },
           svdpc = {
               regr = "Principal components"
               alg = "singular value decomposition"
           },
           stop("Unknown fit method.")
           )
    cat(regr, "regression, fitted with the", alg, "algorithm.")
    if (!is.null(x$validation))
        cat("\nCross-validated using", length(x$validation$segments),
            attr(x$validation$segments, "type"), "segments.")
    cat("\nCall:\n", deparse(x$call), "\n", sep = "")
    invisible(x)
}

## Summary method for mvr objects
summary.mvr <- function(object, what = c("all", "validation", "training"),
                        digits = 4, print.gap = 2, ...)
{
    what <- match.arg(what)
    if (what == "all") what <- c("validation", "training")
    if (is.null(object$validation)) what <- "training"
  
    nobj <- nrow(object$scores)
    npred <- length(object$Ymeans)
    yvarnames <- dimnames(object$fitted.values)[[2]]
    cat("Data: \tX dimension:", nobj, length(object$Xmeans),
        "\n\tY dimension:", nobj, npred)
    cat("\nFit method:", object$method)
    cat("\nNumber of components considered:", object$ncomp)
  
    for (wh in what) {
        if (wh == "training") {
            cat("\nTRAINING: % variance explained\n")
            xve <- object$Xvar / object$Xtotvar
            yve <- drop(R2(object, estimate = "train", intercept = FALSE)$val)
            tbl <- 100 * rbind(cumsum(xve), yve)
            dimnames(tbl) <- list(c("X", yvarnames),
                                  paste(1:object$ncomp, "comps"))
            
            ## FIXME: What's the `format = "f"' supposed to do?
            print(tbl, format = "f", digits = digits,
                  print.gap = print.gap, ...)
        } else {
            cat("\n\nVALIDATION: RMSEP")
            ## FIXME: se(RMSEP) and cross-validated R2 not implemented yet!
            cat("\nCross-validated using", length(object$validation$segments),
                attr(object$validation$segments, "type"), "segments.\n")
            print(RMSEP(object), digits = digits, print.gap = print.gap, ...)
        }
    }
}

## Print method for mvrVal objects:
print.mvrVal <- function(x, digits = 4, print.gap = 2, ...) {
    npred <- dim(x$val)[2]
    yvarnames <- dimnames(x$val)[[2]]
    names(dimnames(x$val)) <- NULL
    for (i in 1:npred) {
        if (npred > 1) cat("\nResponse:", yvarnames[i], "\n")
        ## FIXME: What's the `format = "f"' for?
        print(x$val[,i,], format = "f", digits = digits,
              print.gap = print.gap, ...)
    }
    invisible(x)
}
