### stdize.R: Standardization by centering and scaling

## This is a somewhat modified version of scale.default


#' @name stdize
#' @title Standardization of Data Matrices
#'
#' @description Performs standardization (centering and scaling) of a data matrix.
#'
#' @details \code{makepredictcall.stdized} is an internal utility function; it is not
#' meant for interactive use.  See \code{\link{makepredictcall}} for details.
#'
#' If \code{center} is \code{TRUE}, \code{x} is centered by subtracting the
#' coloumn mean from each coloumn.  If \code{center} is a numeric vector, it is
#' used in place of the coloumn means.
#'
#' If \code{scale} is \code{TRUE}, \code{x} is scaled by dividing each coloumn
#' by its sample standard deviation.  If \code{scale} is a numeric vector, it
#' is used in place of the standard deviations.
#'
#' @aliases stdize predict.stdized makepredictcall.stdized
#' @param x,newdata numeric matrices.  The data to standardize.
#' @param center logical value or numeric vector of length equal to the number
#' of coloumns of \code{x}.
#' @param scale logical value or numeric vector of length equal to the number
#' of coloumns of \code{x}.
#' @param object an object inheriting from class \code{"stdized"}, normally the
#' result of a call to \code{stdize}.
#' @param var A variable.
#' @param call The term in the formula, as a call.
#' @param \dots other arguments.  Currently ignored.
#' @return Both \code{stdize} and \code{predict.stdized} return a scaled and/or
#' centered matrix, with attributes \code{"stdized:center"} and/or
#' \code{"stdized:scale"} the vector used for centering and/or scaling.  The
#' matrix is given class \code{c("stdized", "matrix")}.
#' @note \code{stdize} is very similar to \code{\link[base]{scale}}.  The
#' difference is that when \code{scale = TRUE}, \code{stdize} divides the
#' coloumns by their standard deviation, while \code{scale} uses the
#' root-mean-square of the coloumns.  If \code{center} is \code{TRUE}, this is
#' equivalent, but in general it is not.
#' @author Bjørn-Helge Mevik and Ron Wehrens
#' @seealso \code{\link{mvr}}, \code{\link{pcr}}, \code{\link{plsr}},
#' \code{\link{msc}}, \code{\link[base]{scale}}
#' @keywords regression multivariate
#' @examples
#'
#' data(yarn)
#' ## Direct standardization:
#' Ztrain <- stdize(yarn$NIR[yarn$train,])
#' Ztest <- predict(Ztrain, yarn$NIR[!yarn$train,])
#'
#' ## Used in formula:
#' mod <- plsr(density ~ stdize(NIR), ncomp = 6, data = yarn[yarn$train,])
#' pred <- predict(mod, newdata = yarn[!yarn$train,]) # Automatically standardized
#'
#' @export
stdize <- function(x, center = TRUE, scale = TRUE) {
    nc <- ncol(x)
    if (is.logical(center)) {
        if (isTRUE(center)) {
            center <- colMeans(x, na.rm = TRUE)
            x <- sweep(x, 2, center)
        }
    } else if (is.numeric(center) && length(center) == nc)
        x <- sweep(x, 2, center)
    else stop("invalid 'center'")
    if (is.logical(scale)) {
        if (isTRUE(scale)) {
            ## This is faster than sd(x), but cannot handle missing values:
            scale <- sqrt(colSums(sweep(x, 2, colMeans(x))^2) / (nrow(x) - 1))
            x <- sweep(x, 2, scale, "/")
        }
    } else if (is.numeric(scale) && length(scale) == nc)
        x <- sweep(x, 2, scale, "/")
    else stop("invalid 'scale'")
    if (is.numeric(center)) attr(x, "stdized:center") <- center
    if (is.numeric(scale))  attr(x, "stdized:scale")  <- scale
    class(x) <- c("stdized", "matrix")
    return(x)
}

## This is not really needed for `stdize' to work with formulas, but might
## be nice to have for manually manipulating data:
#' @rdname stdize
#' @export
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
#' @rdname stdize
#' @export
makepredictcall.stdized <- function(var, call) {
    if (as.character(call)[1] != "stdize")
        return(call)
    if (!is.null(z <- attr(var, "stdized:center")))
        call$center <- z
    if (!is.null(z <- attr(var, "stdized:scale")))
        call$scale <- z
    call
}
