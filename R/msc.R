### msc.R: Multiplicative scatter/signal correction
###
###	$Id$	

msc <- function(X, reference = NULL) {
    if (is.null(reference)) reference <- colMeans(X)
    Z <- cbind(1, reference)
    ## The estimated regression coefficients (a_i, b_i), one pair per row:
    B <- t(solve(crossprod(Z), t(X %*% Z)))
    res <- (X - B[,1]) / B[,2]
    attr(res, "reference") <- reference
    class(res) <- c("msc", "matrix")
    return(res)
}

predict.msc <- function(object, newdata, ...) {
    if (missing(newdata)) return(object)
    msc(newdata, reference = attr(object, "reference"))
}

## This method makes things like
## `predict(plsr(y ~ msc(X), data = foo), newdata = bar)' work.
makepredictcall.msc <- function(var, call) {
    if (as.character(call)[1] != "msc") 
        return(call)
    call$reference <- attr(var, "reference")
    call
}
