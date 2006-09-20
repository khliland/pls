### stdize.R: Standardization by centering and scaling
###
###	$Id$

## This is a somewhat modified version of scale.default
stdize <- function(x, center = TRUE, scale = TRUE) {
    nc <- ncol(x)
    if (is.logical(center)) {
        if (center) {
            center <- colMeans(x, na.rm = TRUE)
            x <- sweep(x, 2, center)
        }
    } else if (is.numeric(center) && length(center) == nc)
        x <- sweep(x, 2, center)
    else stop("invalid 'center'")
    if (is.logical(scale)) {
        if (scale) {
            ## This is faster than sd(x), but cannot handle missing values:
            scale <- sqrt(colSums(sweep(x, 2, colMeans(x))^2) / (nrow(x) - 1))
            x <- sweep(x, 2, scale, "/")
        }
    } else if (is.numeric(scale) && length(scale) == nc)
        x <- sweep(x, 2, scale, "/")
    else stop("invalid 'scale'")
    if (is.numeric(center)) attr(x, "stdized:center") <- center
    if (is.numeric(scale)) attr(x, "stdized:scale") <- scale
    class(x) <- c("stdized", "matrix")
    return(x)
}

## This is not really needed for `stdize' to work with formulas, but might
## be nice to have for manually manipulating data:
predict.stdized <- function(object, newdata, ...) {
    if (missing(newdata)) return(object)
    if (is.null(center <- attr(object, "stdized:center")))
        center <- FALSE
    if (is.null(scale <- attr(object, "stdized:scale")))
        scale <- FALSE
    stdize(newdata, center = center, scale = scale)
}

## This method makes things like
## `predict(plsr(y ~ stdize(X), data = foo), newdata = bar)' work.
## This is a slightly modified version of makepredictcall.default.
makepredictcall.stdized <- function(var, call) {
    if (as.character(call)[1] != "stdize")
        return(call)
    if (!is.null(z <- attr(var, "stdized:center")))
        call$center <- z
    if (!is.null(z <- attr(var, "stdized:scale")))
        call$scale <- z
    call
}
