### mvr.R: plsr/pcr modelling functions
###
### The top level user function.  Implements a formula interface and calls the
### correct fit function to do the work.
### The function borrows heavily from lm().

mvr <- function(formula, ncomp, Y.add, data, subset, na.action,
                method = pls.options()$mvralg, scale = FALSE,
                center = TRUE, validation = c("none", "CV", "LOO"),
                model = TRUE, x = FALSE, y = FALSE, ...)
{
    ret.x  <- isTRUE(x)                 # More useful names
    ret.y  <- isTRUE(y)
    center <- isTRUE(center)        # Make sure it is a single logical

    ## Get the model frame
    mf <- match.call(expand.dots = FALSE)
    if (!missing(Y.add)) {
        ## Temporarily add Y.add to the formula
        Y.addname <- as.character(substitute(Y.add))
        mf$formula <- update(formula, paste("~ . +", Y.addname))
    }
    m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]                # Retain only the named arguments
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    method <- match.arg(method, c("kernelpls", "widekernelpls", "simpls",
                                  "oscorespls", "cppls", "svdpc", "model.frame"))
    if (method == "model.frame") return(mf)
    ## Get the terms
    mt <- attr(mf, "terms")        # This is to include the `predvars'
                                   # attribute of the terms
    ## Get the data matrices
    Y <- model.response(mf, "numeric")
    if (is.matrix(Y)) {
        if (is.null(colnames(Y)))
            colnames(Y) <- paste("Y", 1:dim(Y)[2], sep = "")
    } else {
        Y <- as.matrix(Y)
        colnames(Y) <- deparse(formula[[2]])
    }
    if (missing(Y.add)) {
        Y.add <- NULL
    } else {
        Y.add <- mf[,Y.addname]
        ## Remove Y.add from the formula again
        mt <- drop.terms(mt, which(attr(mt, "term.labels") == Y.addname),
                         keep.response = TRUE)
    }
    X <- delete.intercept(model.matrix(mt, mf))

    nobj  <- dim(X)[1]
    npred <- dim(X)[2]

    ## model.matrix prepends the term name to the colnames of matrices.
    ## If there is only one predictor term, and the corresponding matrix
    ## has colnames, remove the prepended term name:
    if (length(attr(mt, "term.labels")) == 1 &&
        !is.null(colnames(mf[[attr(mt, "term.labels")]])))
        colnames(X) <- sub(attr(mt, "term.labels"), "", colnames(X))

    ## Set or check the number of components:
    if (missing(ncomp)) {
        ncomp <- min(nobj - 1, npred)
        ncompWarn <- FALSE              # Don't warn about changed `ncomp'
    } else {
        if (ncomp < 1 || ncomp > min(nobj - 1, npred))
            stop("Invalid number of components, ncomp")
        ncompWarn <- TRUE
    }

    ## Handle any fixed scaling before the the validation
    sdscale <- isTRUE(scale)            # Signals scaling by sd
    if (is.numeric(scale))
        if (length(scale) == npred)
            X <- X / rep(scale, each = nobj)
        else stop("length of 'scale' must equal the number of x variables")

    ## Optionally, perform validation:
    switch(match.arg(validation),
           CV = {
               val <- mvrCv(X, Y, ncomp, Y.add = Y.add, method = method,
                            scale = sdscale, center = center, ...)
           },
           LOO = {
               segments <- as.list(1:nobj)
               attr(segments, "type") <- "leave-one-out"
               val <- mvrCv(X, Y, ncomp, Y.add = Y.add, method = method,
                            scale = sdscale, center = center,
                            segments = segments, ...)
           },
           none = {
               val <- NULL
           }
           )
    ## Check and possibly adjust ncomp:
    if (identical(TRUE, ncomp > val$ncomp)) {
        ncomp <- val$ncomp
        if (ncompWarn) warning("`ncomp' reduced to ", ncomp,
                               " due to cross-validation")
    }

    ## Select fit function:
    fitFunc <- switch(method,
                      kernelpls = kernelpls.fit,
                      widekernelpls = widekernelpls.fit,
                      simpls = simpls.fit,
                      oscorespls = oscorespls.fit,
                      cppls = cppls.fit,
                      svdpc = svdpc.fit)

    ## Perform any scaling by sd:
    if (sdscale) {
        ## This is faster than sd(X), but cannot handle missing values:
        scale <- sqrt(colSums((X - rep(colMeans(X), each = nobj))^2) /
                      (nobj - 1))
        if (any(abs(scale) < .Machine$double.eps^0.5))
            warning("Scaling with (near) zero standard deviation")
        X <- X / rep(scale, each = nobj)
    }

    ## Fit the model:
    start.time <- proc.time()[3]
    z <- fitFunc(X, Y, ncomp, Y.add = Y.add, center = center, ...)
    z$fit.time <- proc.time()[3] - start.time

    ## Build and return the object:
    class(z) <- "mvr"
    z$na.action <- attr(mf, "na.action")
    z$ncomp <- ncomp
    z$method <- method
    if (is.numeric(scale)) z$scale <- scale
    z$validation <- val
    z$call <- match.call()
    z$terms <- mt
    if (isTRUE(model)) z$model <- mf
    if (ret.x) z$x <- X
    if (ret.y) z$y <- Y
    z
}
