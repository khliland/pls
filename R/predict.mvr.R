### predict.mvr.R: A predict method
### $Id$

predict.mvr <- function(object, newdata, comps = 1:object$ncomp,
                        type = c("response", "scores"), cumulative = TRUE,
                        na.action = na.pass, ...) {
    if (missing(newdata) || is.null(newdata))
        newX <- model.matrix(object)
    else if (is.matrix(newdata))
        newX <- newdata
    else {
        Terms <- delete.response(terms(object))
        m <- model.frame(Terms, newdata, na.action = na.action)
        if (!is.null(cl <- attr(Terms, "dataClasses")))
            .checkMFClasses(cl, m)
        newX <- model.matrix(Terms, m)
    }
    ## Perform any scaling:
    if (!is.null(object$scale)) newX <- sweep(newX, 2, object$scale, "/")
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
                pred[,,i] <- sweep(newX %*% B[-1,,i], 2, B[1,,i], "+")
            return(pred)
        } else {
            ## Predict with a model containing the components `comps'
            B <- rowSums(coef(object, comps = comps, cumulative = FALSE),
                         dims = 2)
            B0 <- object$Ymeans - object$Xmeans %*% B
            return(sweep(newX %*% B, 2, B0, "+"))
        }
    } else {
        ## Return predicted scores (for scores, `cumulative' has no meaning)
        if (missing(newdata)) {
            TT <- object$scores[,comps]
        } else {
            if (is.null(object$projection))
                stop("`object' has no `projection' component.  Maybe it was fitted with `stripped = TRUE'.")
            maxComp <- max(comps)
            TT <- sweep(newX, 2, object$Xmeans) %*% object$projection[,comps]
        }
        return(TT)
    }
}
