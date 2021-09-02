### predict.mvr.R: A predict method



#' @title Predict Method for PLSR and PCR
#'
#' @description Prediction for mvr (PCR, PLSR) models.  New responses or scores are
#' predicted using a fitted model and a new matrix of observations.
#'
#' @details When \code{type} is \code{"response"} (default), predicted response values
#' are returned.  If \code{comps} is missing (or is \code{NULL}), predictions
#' for \code{length(ncomp)} models with \code{ncomp[1]} components,
#' \code{ncomp[2]} components, etc., are returned.  Otherwise, predictions for
#' a single model with the exact components in \code{comps} are returned.
#' (Note that in both cases, the intercept is always included in the
#' predictions.  It can be removed by subtracting the \code{Ymeans} component
#' of the fitted model.)
#'
#' When \code{type} is \code{"scores"}, predicted score values are returned for
#' the components given in \code{comps}.  If \code{comps} is missing or
#' \code{NULL}, \code{ncomps} is used instead.
#'
#' It is also possible to supply a matrix instead of a data frame as
#' \code{newdata}, which is then assumed to be the \eqn{X} data matrix.  Note
#' that the usual checks for the type of the data are then omitted.  Also note
#' that this is \emph{only} possible with \code{predict}; it will not work in
#' functions like \code{\link{predplot}}, \code{\link{RMSEP}} or
#' \code{\link{R2}}, because they also need the response variable of the new
#' data.
#'
#' @param object an \code{mvr} object.  The fitted model
#' @param newdata a data frame.  The new data.  If missing, the training data
#' is used.
#' @param ncomp,comps vector of positive integers.  The components to use in
#' the prediction.  See below.
#' @param type character.  Whether to predict scores or response values
#' @param na.action function determining what should be done with missing
#' values in \code{newdata}.  The default is to predict \code{NA}.  See
#' \code{\link{na.omit}} for alternatives.
#' @param \dots further arguments.  Currently not used
#' @return When \code{type} is \code{"response"}, a three dimensional array of
#' predicted response values is returned.  The dimensions correspond to the
#' observations, the response variables and the model sizes, respectively.
#'
#' When \code{type} is \code{"scores"}, a score matrix is returned.
#' @note A warning message like \samp{'newdata' had 10 rows but variable(s)
#' found have 106 rows} means that not all variables were found in the
#' \code{newdata} data frame.  This (usually) happens if the formula contains
#' terms like \code{yarn$NIR}.  Do not use such terms; use the \code{data}
#' argument instead.  See \code{\link{mvr}} for details.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{summary.mvr}},
#' \code{\link{coef.mvr}}, \code{\link{plot.mvr}}
#' @keywords regression multivariate
#' @examples
#'
#' data(yarn)
#' nir.mvr <- mvr(density ~ NIR, ncomp = 5, data = yarn[yarn$train,])
#'
#' ## Predicted responses for models with 1, 2, 3 and 4 components
#' pred.resp <- predict(nir.mvr, ncomp = 1:4, newdata = yarn[!yarn$train,])
#'
#' ## Predicted responses for a single model with components 1, 2, 3, 4
#' predict(nir.mvr, comps = 1:4, newdata = yarn[!yarn$train,])
#'
#' ## Predicted scores
#' predict(nir.mvr, comps = 1:3, type = "scores", newdata = yarn[!yarn$train,])
#'
#' @export
predict.mvr <- function(object, newdata, ncomp = 1:object$ncomp, comps,
                        type = c("response", "scores"),
                        na.action = na.pass, ...)
{
    if (missing(newdata) || is.null(newdata))
        newX <- model.matrix(object)
    else if (is.matrix(newdata)) {
        ## For matrices, simply check dimension:
        if (ncol(newdata) != length(object$Xmeans))
            stop("'newdata' does not have the correct number of columns")
        newX <- newdata
    } else {
        Terms <- delete.response(terms(object))
        m <- model.frame(Terms, newdata, na.action = na.action)
        if (!is.null(cl <- attr(Terms, "dataClasses")))
            .checkMFClasses(cl, m)
        newX <- delete.intercept(model.matrix(Terms, m))
    }

    nobs <- dim(newX)[1]

    ## Perform any scaling:
    if (!is.null(object$scale)) newX <- newX / rep(object$scale, each = nobs)
    type <- match.arg(type)
    if (type == "response") {
        if (missing(comps) || is.null(comps)) {
            ## Predict with models containing ncomp[1] components,
            ## ncomp[2] components, etc.
            if (missing(newdata)) return(fitted(object)[,,ncomp, drop=FALSE])
            B <- coef(object, ncomp = ncomp, intercept = TRUE)
            dPred <- dim(B)
            dPred[1] <- dim(newX)[1]
            dnPred <- dimnames(B)
            dnPred[1] <-
                if(is.null(dimnames(newX))) list(NULL) else dimnames(newX)[1]
            pred <- array(dim = dPred, dimnames = dnPred)
            for (i in seq(along = ncomp))
                pred[,,i] <- newX %*% B[-1,,i] + rep(B[1,,i], each = nobs)
            return(pred)
        } else {
            ## Predict with a model containing the components `comps'
            B <- rowSums(coef(object, comps = comps), dims = 2)
            B0 <- object$Ymeans - object$Xmeans %*% B
            pred <- newX %*% B + rep(B0, each = nobs)
            if (missing(newdata) && !is.null(object$na.action))
                pred <- napredict(object$na.action, pred)
            return(pred)
        }
    } else {
        ## Return predicted scores (for scores, `cumulative' has no meaning)
        ## When predicting scores, we allow ncomp as an alias for comps:
        if (missing(comps) || is.null(comps)) comps <- ncomp
        if (missing(newdata)) {
            TT <- object$scores[,comps]
            if (!is.null(object$na.action))
                TT <- napredict(object$na.action, TT)
        } else {
            if (is.null(object$projection))
                stop("`object' has no `projection' component.  Maybe it was fitted with `stripped = TRUE'.")
            TT <- (newX - rep(object$Xmeans, each = nobs)) %*%
                object$projection[,comps]
        }
        return(TT)
    }
}
