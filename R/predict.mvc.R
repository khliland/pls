### predict.mvc.R: A predict method
### $Id$

predict.mvc <- function(object, newdata, ncomp = 1:object$ncomp, comps,
                        type = c("class", "posterior", "response", "scores"),
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
    predcall <- match.call(expand.dots = FALSE)
    predcall[[1]] <- as.name("predict.mvr")
    type <- match.arg(type)
    if (type == "response" || type == "scores") {
        return(eval(predcall, parent.frame()))
    } else if (type != "class" && type != "posterior") {
        stop("This cannot happen")
    }
    ## We have type "class" or "posterior"
    predcall$type <- object$classX      # "scores" or "response"
    predcall$ncomp <- seq_len(max(ncomp)) # to make it easier to subscript later
    print(predcall)                     # debug
    pls.pred <- eval(predcall, parent.frame())
    print(dim(pls.pred))                # debug
    if (object$classX == "scores") {
        pls.pred <- as.matrix(pls.pred) # in case ncomp == 1
    } else {
        pls.pred <- pls.pred[,-dim(pls.pred)[2],, drop=FALSE] # remove last response
    }
    print(dim(pls.pred))                # debug
    pred <- list()
    for (nc in ncomp) {
        print(nc)                       # debug
        X.class <- if (object$classX == "scores")
            pls.pred[,1:nc, drop=FALSE]
        else
            as.matrix(pls.pred[,,nc, drop=TRUE])
        ## FIXME: This will depend on the class method:
        pred[[as.character(nc)]] <- predict(object$classfit[[nc]], X.class, ...)[[type]]
    }
    return(pred)
}


## Temporary definitions
model.matrix.mvc <- model.matrix.mvr
