### mvr.R: plsr/pcr modelling functions
###
### $Id$

## The top level user function.  Implements a formula interface and calls the
## correct fit function to do the work.
## The function borrows heavily from lm().
mvr <- function(formula, ncomp, data, subset, na.action,
                method = c("kernelpls", "simpls", "oscorespls", "svdpc"),
                validation = c("none", "CV", "LOO"),
                model = TRUE, x = FALSE, y = FALSE, ...)
{
    ret.x <- x                          # More useful names
    ret.y <- y
    ## Get the model frame
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]                # Retain only the named arguments
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    method <- match.arg(method)
    if (method == "model.frame") return(mf) # FIXME: Remove?
    ## Get the terms
    mt <- attr(mf, "terms")        # This is to include the `predvars'
                                   # attribute of the terms
    attr(mt, "intercept") <- 0          # Remove the intercept term
    ## Get the data matrices
    Y <- model.response(mf, "numeric")
    if (is.matrix(Y)) {
        if (is.null(colnames(Y)))
            colnames(Y) <- paste("Y", 1:dim(Y)[2], sep = "")
    } else {
        Y <- as.matrix(Y)
        colnames(Y) <- deparse(formula[[2]])
    }
    X <- model.matrix(mt, mf)
    ## Set or check the number of components:
    if (missing(ncomp)) {
        ncomp <- min(nrow(X) - 1, ncol(X))
    } else {
        if (ncomp < 1 || ncomp > min(nrow(X) - 1, ncol(X)))
            stop("Invalid number of components, ncomp")
    }
    ## Select fit function:
    fitFunc <- switch(method,
                      simpls = simpls.fit,
                      kernelpls = kernelpls.fit,
                      oscorespls = oscorespls.fit,
                      svdpc = svdpc.fit)
    ## Fit the model:
    z <- fitFunc(X, Y, ncomp, ...)
    ## Optionally, perform validation:
    switch(match.arg(validation),
           CV = {
               z$validation <- mvrCv(X, Y, ncomp, method = method, ...)
           },
           LOO = {
               segments <- as.list(1:nrow(X))
               attr(segments, "type") <- "leave-one-out"
               z$validation <- mvrCv(X, Y, ncomp, method = method,
                                     segments = segments, ...)
           }
           )
    ## Build and return the object:
    class(z) <- "mvr"
    z$ncomp <- ncomp
    z$method <- method
    z$call <- match.call()
    z$terms <- mt
    if (model) z$model <- mf
    if (ret.x) z$x <- X
    if (ret.y) z$y <- Y
    z
}
