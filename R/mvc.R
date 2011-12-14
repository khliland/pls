### mvc.R: plsr/pcr discriminant analysis function
###
### $Id$
###
### The top level user function.  Implements a formula interface and calls the
### correct fit function(s) to do the work.
### The function borrows heavily from mvr().

mvc <- function(formula, ncomp, Y.add, data, subset, na.action,
                 ## FIXME:
                 regression = c("kernelpls", "widekernelpls", "simpls",
                                  "oscorespls", "cppls", "plsda", "svdpc", "model.frame")
                 classX = c("scores", "response"),
                 classifier = c("lda", "qda"),
                 scale = FALSE, validation = c("none", "CV", "LOO"),
                 model = TRUE, x = FALSE, y = FALSE, ...)
{
    ret.x <- x                          # More useful names
    ret.y <- y

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
    ## FIXME:
    regression <- match.arg(regression)
    classX <- match.arg(classX)
    classifier <- match.arg(classifier)
    if (regression == "model.frame") return(mf)
    ## Get the terms
    mt <- attr(mf, "terms")        # This is to include the `predvars'
                                   # attribute of the terms
    ## Get the data matrices
    cl <- model.response(mf)
    if (is.factor(cl))
        Y <- class.ind(cl)
    else if (is.matrix(cl)) {
        Y <- cl
        if (!all(rowSums(Y) == 1) || !(sum(Y == 1) == nrow(Y)))
            stop("The rows of the response matrix must be 0/1 and sum to 1")
        if (is.null(colnames(Y)))
            colnames(Y) <- paste("Y", 1:dim(Y)[2], sep = "")
        levels <- colnames(Y)
        cl <- factor(levels[apply(Y, 1, which.max)])
    } else {
        stop("The response must be a factor or a matrix")
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

    nobj <- dim(X)[1]
    npred <- dim(X)[2]
    nresp <- dim(Y)[2]

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
    sdscale <- identical(TRUE, scale)   # Signals scaling by sd
    if (is.numeric(scale))
        if (length(scale) == npred)
            X <- X / rep(scale, each = nobj)
        else stop("length of 'scale' must equal the number of x variables")

    ## Optionally, perform validation:
    switch(match.arg(validation),
           CV = {
               val <- NULL
               ##val <- mvrCv(X, Y, ncomp, Y.add = Y.add, method = method, scale = sdscale, ...)
           },
           LOO = {
               segments <- as.list(1:nobj)
               attr(segments, "type") <- "leave-one-out"
               val <- NULL
               ##val <- mvrCv(X, Y, ncomp, Y.add = Y.add, method = method, scale = sdscale,
               ##segments = segments, ...)
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

    ## Select fit functions:
    regFunc <- switch(regression,
                      kernelpls = kernelpls.fit,
                      widekernelpls = widekernelpls.fit,
                      simpls = simpls.fit,
                      oscorespls = oscorespls.fit,
                      cppls = cppls.fit,
                      plsda = plsda.fit,
                      svdpc = svdpc.fit)
    ## FIXME: These should probably be wrappers, to ensure similar interface:
    classFunc <- switch(classifier,
                        lda = lda,
                        qda = qda
                        )

    ## Perform any scaling by sd:
    if (sdscale) {
        ## This is faster than sd(X), but cannot handle missing values:
        scale <- sqrt(colSums((X - rep(colMeans(X), each = nobj))^2) /
                      (nobj - 1))
        if (any(abs(scale) < .Machine$double.eps^0.5))
            warning("Scaling with (near) zero standard deviation")
        X <- X / rep(scale, each = nobj)
    }

    ## Fit the regression model:
    z <- regFunc(X, Y, ncomp, Y.add = Y.add, ...)

    ## Fit the classifier model:
    ## FIXME: This is very brute-force.
    ## FIXME: Some classifiers might be able to estimate several models at
    ## once, or it might be possible to extract the "submodels" afterwards
    z$classfit <- list()
    ## FIXME: for "response", use nresp:ncomp instead of 1:ncomp
    for (nc in 1:ncomp) {
        X.class <- if (classX == "scores")
            z$scores[,1:nc, drop=FALSE]
        else
            as.matrix(z$fitted.values[,-nresp,nc, drop=TRUE])
        print(dim(X.class))
        z$classfit[[nc]] <- classFunc(X.class, cl, ...)
    }

    ## Build and return the object:
    class(z) <- "mvc"
    z$na.action <- attr(mf, "na.action")
    z$ncomp <- ncomp
    z$regression <- regression
    z$classX <- classX
    z$classifier <- classifier
    if (is.numeric(scale)) z$scale <- scale
    z$validation <- val
    z$call <- match.call()
    z$terms <- mt
    if (model) z$model <- mf
    if (ret.x) z$x <- X
    if (ret.y) z$y <- Y
    z
}


### Temporary help functions:
## from nnet.formula, via caret:
class.ind <- function(cl)
{
    n <- length(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (as.vector(unclass(cl)) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
}
