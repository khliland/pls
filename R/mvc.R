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
                               "oscorespls", "cppls", "plsda", "svdpc",
                               "model.frame"),
                ## FIXME: Perhaps default, and allowed values, of pred.class
                ## should be decided from the value of classifier?
                pred.class = c("scores", "response"),
                classifier = c("lda", "qda", "max"),
                prior,
                scale = FALSE, validation = c("none", "CV", "LOO"),
                model = TRUE, x = FALSE, y = FALSE,
                args.reg = list(), args.class = list(),
                ...)
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
    pred.class <- match.arg(pred.class)
    classifier <- match.arg(classifier)
    ## FIXME: Dirty hack:
    if (classifier == "max" && pred.class != "response")
        stop("Classifier 'max' can only be used with pred.class 'response'")
    if (regression == "model.frame") return(mf)
    ## Get the terms
    mt <- attr(mf, "terms")        # This is to include the `predvars'
                                   # attribute of the terms
    ## Get the data matrices
    group <- model.response(mf)
    if (is.factor(group))
        Y <- class.ind(group)
    else if (is.matrix(group)) {
        Y <- group
        if (!all(rowSums(Y) == 1) || !(sum(Y == 1) == nrow(Y)))
            stop("The rows of the response matrix must be 0/1 and sum to 1")
        if (is.null(colnames(Y)))
            colnames(Y) <- paste("Y", 1:dim(Y)[2], sep = "")
        levels <- colnames(Y)
        group <- factor(levels[apply(Y, 1, which.max)])
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

    ## Handle any 'prior' argument.  The allowable values are "proportions",
    ## a numeric vector, and NULL.  The heuristics are:
    ## If missing or NULL, no priors are used for regression, and the default
    ## (often "proportions") is used for the classifier.  If "proportions",
    ## proportions are used for regression, and the default for classifier.
    ## Otherwise it is used for both regression and classification, but can
    ## be overridden by the classifier's own argument.
    reg.prior <- FALSE
    if (missing(prior) || is.null(NULL)) {
        ## do nothing
    } else if (is.character(prior) &&
               pmatch(prior, "proportions", nomatch = FALSE)) {
        ## use proportions for reg
        reg.prior <- "proportions"
    } else {
        ## check that value is ok and use for both reg and class (unless
        ## overridden in args.class)
        if (!is.numeric(prior) || length(prior) != nresp ||
           any(prior < 0) || !isTRUE(all.equal(sum(prior), 1)))
            stop("Invalid value of 'prior'") # FIXME: separate messages!
        reg.prior <- "prior"
        if (! "prior" %in% names(args.class))
            args.class <- c(list(prior = prior), args.class)
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
                        qda = qda,
                        max = NULL,
                        stop("This cannot happen")
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

    ## Optionally use priors for the regression:
    if (reg.prior != FALSE) {
        if (reg.prior == "proportions") prior <- colMeans(Y)
        respWeights <- sqrt(prior) / colSums(Y)
        Y.orig <- Y
        Y <- sweep(Y.orig, 2, respWeights, "*")
    }

    ## Fit the regression model:
    z <- do.call(regFunc, c(list(X, Y, ncomp, Y.add = Y.add), args.reg))

    ## Optionally back-transform the model (so predict, etc. works):
    if (reg.prior) {
        z$Yloadings <- z$Yloadings / respWeights
        z$Ymeans <- z$Ymeans / respWeights
        ## FIXME: there might be faster ways:
        z$coefficients <- sweep(z$coefficients, 2, respWeights, "/")
        z$fitted.values <- sweep(z$fitted.values, 2, respWeights, "/")
        z$residuals <- sweep(z$residuals, 2, respWeights, "/")
        ## FIXME: this is ugly!  Can we skip it?
        TT <- z$scores
        tsqs <- colSums(TT*TT)
        Q <- z$Yloadings
        U <- Y.orig %*% Q %*% diag(1/colSums(Q*Q), ncol = ncol(Q))
        for (a in seq_len(ncol(U))[-1]) {
            U[,a] <- U[,a] - TT[,1:(a-1)] %*% (crossprod(TT[,1:(a-1), drop=FALSE], U[,a]) / tsqs[1:(a-1)])
        }
        Y <- Y.orig                     # for the return value
    }

    ## Fit the classifier model:
    ## FIXME: This is very brute-force.
    ## FIXME: Some classifiers might be able to estimate several models at
    ## once, or it might be possible to extract the "submodels" afterwards
    z$classfit <- list()
    ## FIXME: for "response", use nresp:ncomp instead of 1:ncomp
    for (nc in 1:ncomp) {
        X.class <- if (pred.class == "scores")
            z$scores[,1:nc, drop=FALSE]
        else
            as.matrix(z$fitted.values[,-nresp,nc, drop=TRUE])
        print(dim(X.class))
        if (!is.null(classFunc))        # Not all "classifiers" have a fit
                                        # function
            z$classfit[[nc]] <- do.call(classFunc, c(list(X.class, group), args.class))
    }

    ## Build and return the object:
    class(z) <- "mvc"
    z$na.action <- attr(mf, "na.action")
    z$ncomp <- ncomp
    z$regression <- regression
    z$pred.class <- pred.class
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
class.ind <- function(group)
{
    n <- length(group)
    x <- matrix(0, n, length(levels(group)))
    x[(1:n) + n * (as.vector(unclass(group)) - 1)] <- 1
    dimnames(x) <- list(names(group), levels(group))
    x
}
