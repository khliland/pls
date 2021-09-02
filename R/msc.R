### msc.R: Multiplicative scatter/signal correction



#' @name msc
#' @title Multiplicative Scatter Correction
#'
#' @description Performs multiplicative scatter/signal correction on a data matrix.
#'
#' @details \code{makepredictcall.msc} is an internal utility function; it is not meant
#' for interactive use.  See \code{\link{makepredictcall}} for details.
#'
#' @aliases msc predict.msc makepredictcall.msc
#' @param X,newdata numeric matrices.  The data to scatter correct.
#' @param reference numeric vector.  Spectre to use as reference.  If
#' \code{NULL}, the column means of \code{X} are used.
#' @param object an object inheriting from class \code{"msc"}, normally the
#' result of a call to \code{msc} with a single matrix argument.
#' @param var A variable.
#' @param call The term in the formula, as a call.
#' @param \dots other arguments.  Currently ignored.
#' @return Both \code{msc} and \code{predict.msc} return a multiplicative
#' scatter corrected matrix, with attribute \code{"reference"} the vector used
#' as reference spectre. The matrix is given class \code{c("msc", "matrix")}.
#' For \code{predict.msc}, the \code{"reference"} attribute of \code{object} is
#' used as reference spectre.
#' @author Bjørn-Helge Mevik and Ron Wehrens
#' @seealso \code{\link{mvr}}, \code{\link{pcr}}, \code{\link{plsr}},
#' \code{\link{stdize}}
#' @references Martens, H., Næs, T. (1989) \emph{Multivariate calibration.}
#' Chichester: Wiley.
#' @keywords regression multivariate
#' @examples
#'
#' data(yarn)
#' ## Direct correction:
#' Ztrain <- msc(yarn$NIR[yarn$train,])
#' Ztest <- predict(Ztrain, yarn$NIR[!yarn$train,])
#'
#' ## Used in formula:
#' mod <- plsr(density ~ msc(NIR), ncomp = 6, data = yarn[yarn$train,])
#' pred <- predict(mod, newdata = yarn[!yarn$train,]) # Automatically scatter corrected
#'
#' @export
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

#' @rdname msc
#' @export
predict.msc <- function(object, newdata, ...) {
    if (missing(newdata)) return(object)
    msc(newdata, reference = attr(object, "reference"))
}

## This method makes things like
## `predict(plsr(y ~ msc(X), data = foo), newdata = bar)' work.
#' @rdname msc
#' @export
makepredictcall.msc <- function(var, call) {
    if (as.character(call)[1] != "msc")
        return(call)
    call$reference <- attr(var, "reference")
    call
}
