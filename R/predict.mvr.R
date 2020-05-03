### predict.mvr.R: A predict method

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
