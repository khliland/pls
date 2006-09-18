### predict.mvr.R: A predict method
### $Id$

predict.mvr <- function(object, newdata, comps = 1:object$ncomp,
                        type = c("response", "scores"), cumulative = TRUE,
                        na.action = na.pass, ...) {
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
        if (cumulative) {
            ## Predict with models containing comps[1] components,
            ## comps[2] components, etc.
            if (missing(newdata)) return(fitted(object)[,,comps, drop=FALSE])
            B <- coef(object, comps = comps, intercept = TRUE)
            dPred <- dim(B)
            dPred[1] <- dim(newX)[1]
            dnPred <- dimnames(B)
            dnPred[1] <- dimnames(newX)[1]
            pred <- array(dim = dPred, dimnames = dnPred)
            for (i in seq(along = comps))
                pred[,,i] <- newX %*% B[-1,,i] + rep(B[1,,i], each = nobs)
            return(pred)
        } else {
            ## Predict with a model containing the components `comps'
            B <- rowSums(coef(object, comps = comps, cumulative = FALSE),
                         dims = 2)
            B0 <- object$Ymeans - object$Xmeans %*% B
            return(newX %*% B + rep(B0, each = nobs))
        }
    } else {
        ## Return predicted scores (for scores, `cumulative' has no meaning)
        if (missing(newdata)) {
            TT <- object$scores[,comps]
        } else {
            if (is.null(object$projection))
                stop("`object' has no `projection' component.  Maybe it was fitted with `stripped = TRUE'.")
            maxComp <- max(comps)
            TT <- (newX - rep(object$Xmeans, each = newX)) %*%
                object$projection[,comps]
        }
        return(TT)
    }
}
