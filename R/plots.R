### Plots for mvr objects.  Some of them also work for other
### objects, but that is not a priority.
###
### $Id$

###
### Plot method for mvr objects
###

plot.mvr <- function(x, plottype = c("prediction", "validation",
                        "coefficients", "scores", "loadings", "biplot"),
                     ...)
{
    plottype <- match.arg(plottype)
    plotFunc <- switch(plottype,
                       prediction = predplot.mvr,
                       validation = validationplot,
                       coefficients = coefplot,
                       scores = scoreplot,
                       loadings = loadingplot,
                       biplot = biplot.mvr)
    plotFunc(x, ...)
}


###
### Scoreplot
###

scoreplot <- function(object, comps = 1:2, labels, identify = FALSE,
                      type = "p", xlab, ylab, ...) {
    nComps <- length(comps)
    if (nComps == 0) stop("At least one component must be selected.")
    if (is.matrix(object)) {
        ## Assume this is already a score matrix
        S <- object[,comps, drop = FALSE]
        expl.var <- NULL
    } else {
        S <- scores(object)[,comps, drop = FALSE]
        if (is.null(S))
            stop("`", deparse(substitute(object)), "' has no scores")
        expl.var <- switch(class(object),
                           mvr = 100 * object$Xvar[comps] / object$Xtotvar,
                           princomp =, 
                           prcomp = 100*object$sdev[comps]^2/sum(object$sdev^2)
                           )
    }
    varlab <- colnames(S)               # Default variable labels
    if (!is.null(expl.var))             # Add explained variance
        varlab <- paste(varlab, " (", format(expl.var, digits = 2, trim = TRUE),
                        " %)", sep = "")
    if (!missing(labels)) {
        ## Set up point labels
        if (length(labels) == 1) {
            labels <- switch(match.arg(labels, c("names", "numbers")),
                             names = rownames(S),
                             numbers = 1:nrow(S)
                             )
        }
        labels <- as.character(labels)
        type <- "n"
    }
    if (nComps <= 2) {
        if (nComps == 1) {
            ## One component versus index
            if (missing(xlab)) xlab <- "observation"
            if (missing(ylab)) ylab <- varlab
        } else {
            ## Second component versus first
            if (missing(xlab)) xlab <- varlab[1]
            if (missing(ylab)) ylab <- varlab[2]
        }
        plot(S, xlab = xlab, ylab = ylab, type = type, ...)
        if (!missing(labels)) text(S, labels, ...)
        if (identify) {
            if (!is.null(rownames(S))) {
                identify(S, labels = rownames(S))
            } else {
                identify(S)
            }
        }
    } else {
        ## Pairwise scatterplots of several components
        panel <- if (missing(labels))
            function(x, y, ...) points(x, y, type = type, ...) else
            function(x, y, ...) text(x, y, labels = labels, ...)
        pairs(S, labels = varlab, panel = panel, ...)
    } 
}

## A plot method for scores:
plot.scores <- function(x, ...) scoreplot(x, ...)


###
### Loadingplot
###

loadingplot <- function(object, comps = 1:2, scatter = FALSE, labels,
                        identify = FALSE, type, lty, lwd = NULL, pch,
                        cex = NULL, col, legendpos, xlab, ylab, ...)
{
    nComps <- length(comps)
    if (nComps == 0) stop("At least one component must be selected.")
    if (!missing(type) && sum(nchar(type)) != 1) stop("invalid plot type")
    if (is.matrix(object)) {
        ## Assume this is already a loading matrix
        L <- object[,comps, drop = FALSE]
        expl.var <- NULL
    } else {
        L <- loadings(object)[,comps, drop = FALSE]
        if (is.null(L))
            stop("`", deparse(substitute(object)), "' has no loadings")
        expl.var <- switch(class(object),
                           mvr = 100 * object$Xvar[comps] / object$Xtotvar,
                           princomp =, 
                           prcomp = 100*object$sdev[comps]^2/sum(object$sdev^2)
                           )
    }
    varlab <- colnames(L)               # Default variable labels
    if (!is.null(expl.var))             # Add explained variance
        varlab <- paste(varlab, " (", format(expl.var, digits = 2, trim = TRUE),
                        " %)", sep = "")
    if (scatter) {
        ## Scatter plots
        if (missing(type)) type <- "p"
        if (missing(lty)) lty <- NULL
        if (missing(pch)) pch <- NULL
        if (missing(col)) col <- par("col") # `NULL' means `no colour'
        if (!missing(labels)) {
            ## Set up point labels
            if (length(labels) == 1) {
                labels <- switch(match.arg(labels, c("names", "numbers")),
                                 names = rownames(L),
                                 numbers = 1:nrow(L)
                                 )
            }
            labels <- as.character(labels)
            type <- "n"
        }
        if (nComps <= 2) {
            if (nComps == 1) {
                ## One component versus index
                if (missing(xlab)) xlab <- "variable"
                if (missing(ylab)) ylab <- varlab
            } else {
                ## Second component versus first
                if (missing(xlab)) xlab <- varlab[1]
                if (missing(ylab)) ylab <- varlab[2]
            }
            plot(L, xlab = xlab, ylab = ylab, type = type, lty = lty,
                 lwd = lwd, pch = pch, cex = cex, col = col, ...)
            if (!missing(labels)) text(L, labels, cex = cex, col = col, ...)
            if (identify)
                identify(L, labels = paste(1:nrow(L), rownames(L), sep = ": "))
        } else {
            ## Pairwise scatterplots of several components
            panel <- if (missing(labels)) {
                function(x, y, ...)
                    points(x, y, type = type, lty = lty, lwd = lwd,
                           pch = pch, col = col, ...)
            } else {
                function(x, y, ...)
                    text(x, y, labels = labels, col = col, ...)
            }
            pairs(L, labels = varlab, panel = panel, cex = cex, ...)
        }
    } else {                            # if (scatter)
        ## Line plots
        if (missing(type)) type <- "l"
        if (missing(lty)) lty <- 1:nComps
        if (missing(pch)) pch <- 1:nComps
        if (missing(col)) col <- 1:nComps
        if (missing(xlab)) xlab <- "variable"
        if (missing(ylab)) ylab <- "loading value"
        matplot(L, xlab = xlab, ylab = ylab, type = type,
                lty = lty, lwd = lwd, pch = pch, cex = cex, col = col,...)
        if (!missing(legendpos)) {
            ## Are we plotting lines?
            dolines <- type %in% c("l", "b", "c", "o", "s", "S", "h")
            ## Are we plotting points?
            dopoints <- type %in% c("p", "b", "o")
            if (length(lty) > nComps) lty <- lty[1:nComps]
            do.call("legend", c(list(legendpos, varlab, col = col),
                                if (dolines) list(lty = lty, lwd = lwd),
                                if (dopoints) list(pch = pch, pt.cex = cex,
                                                   pt.lwd = lwd)))
        }
        if (identify)
            identify(c(row(L)), c(L),
                     labels = paste(c(col(L)), rownames(L), sep = ": "))
    }                                   # if (scatter)
}

## A plot method for loadings (loadings, loading.weights or Yloadings):
plot.loadings <- function(x, ...) loadingplot(x, ...)


###
### prediction plot
###

## Generic:
predplot <- function(object, ...)
  UseMethod("predplot")

## Default method:
predplot.default <- function(object, ...) {
    measured <- model.response(model.frame(object))
    predicted <- predict(object)
    predplotXy(measured, predicted, ...)
}

## Method for mvr objects:
predplot.mvr <- function(object, ncomp = object$ncomp, which, newdata,
                         nCols, nRows, xlab = "measured", ylab = "predicted",
                         font.main = 1, cex.main = 1.1, ...)
{
    ## Select type(s) of prediction
    if (missing(which)) {
        ## Pick the «best» alternative.
        if (!missing(newdata)) {
            which <- "test"
        } else {
            if (!is.null(object$validation)) {
                which <- "validation"
            } else {
                which <- "train"
            }
        }
    } else {
        ## Check the supplied `which'
        allTypes <- c("train", "validation", "test")
        which <- allTypes[pmatch(which, allTypes)]
        if (length(which) == 0 || any(is.na(which)))
            stop("`which' should be a subset of ",
                 paste(allTypes, collapse = ", "))
    }
    
    ## Help variables
    nEst <- length(which)
    nSize <- length(ncomp)
    nResp <- dim(object$fitted.values)[2]

    ## Set plot parametres as needed:
    dims <- c(nEst, nSize, nResp)
    dims <- dims[dims > 1]
    if (length(dims) > 0) {
        ## Show the *labs in the margin:
        mXlab <- xlab
        mYlab <- ylab
        xlab <- ylab <- ""
        if(missing(nCols)) nCols <- min(c(3, dims[1]))
        if(missing(nRows))
            nRows <- min(c(3, ceiling(prod(dims[1:2], na.rm = TRUE) / nCols)))
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
        par(mfrow = c(nRows, nCols),
            oma = c(1,1,0,0) + 0.1, mar = c(3,3,3,1) + 0.1)
        if (nRows * nCols < prod(dims)) par(ask = TRUE)
    } else {
        nCols <- nRows <- 1
    }

    ## Set up measured and predicted for all model sizes, responses and
    ## estimates:
    if ("train" %in% which) {
        train.measured <- as.matrix(model.response(model.frame(object)))
        train.predicted <- object$fitted.values[,,ncomp, drop = FALSE]
    }
    if ("validation" %in% which) {
        if (is.null(object$validation)) stop("`object' has no `validation' component.")
        if(!exists("train.measured"))
            train.measured <- as.matrix(model.response(model.frame(object)))
        validation.predicted <- object$validation$pred[,,ncomp, drop = FALSE]
    }
    if ("test" %in% which) {
        if (missing(newdata)) stop("Missing `newdata'.")
        test.measured <- as.matrix(model.response(model.frame(formula(object),
                                                              data = newdata)))
        test.predicted <- predict(object, comps = ncomp, newdata = newdata)
    }
  
    ## Do the plots
    plotNo <- 0
    for (resp in 1:nResp) {
        for (size in 1:nSize) {
            for (est in 1:nEst) {
                plotNo <- plotNo + 1
                main <- paste(colnames(object$fitted)[resp], ncomp[size],
                              "comps,", which[est])
                sub <- which[est]
                switch(which[est],
                       train = {
                           measured <- train.measured[,resp]
                           predicted <- train.predicted[,resp,size]
                       },
                       validation = {
                           measured <- train.measured[,resp]
                           predicted <- validation.predicted[,resp,size]
                       },
                       test = {
                           measured <- test.measured[,resp]
                           predicted <- test.predicted[,resp,size]
                       }
                       )
                if (length(dims) > 0 &&
                    (plotNo %% (nCols * nRows) == 0 || plotNo == prod(dims))) {
                    ## Last plot on a page; add outer margin text:
                    mtext(mXlab, side = 1, outer = TRUE)
                    mtext(mYlab, side = 2, outer = TRUE)
                }
                predplotXy(measured, predicted, main = main,
                           font.main = font.main, cex.main = cex.main, 
                           xlab = xlab, ylab = ylab, ...)
            }
        }
    }
}

## The workhorse function:
predplotXy <- function(x, y, line = FALSE, main = "Prediction plot",
                       xlab = "measured response",
                       ylab = "predicted response", ...)
{
    plot(y ~ x, main = main, xlab = xlab, ylab = ylab, ...)
    if (line) abline(0,1)
    invisible(cbind(measured = x, predicted = as.vector(y)))
}


###
### Coefficient plot
###

coefplot <- function(object, ncomp = object$ncomp, separate = FALSE,
                     cumulative = TRUE, intercept = FALSE,
                     nCols, nRows, type = "l", lty = 1:nLines, lwd = NULL,
                     pch = 1:nLines, cex = NULL, col = 1:nLines, legendpos,
                     xlab = "variable", ylab = "regression coefficient", ...)
{
    ## Help variables
    nSize <- if (separate) length(ncomp) else 1
    nResp <- dim(object$fitted.values)[2]
    nLines <- length(ncomp)

    ## Set plot parametres as needed:
    dims <- c(nSize, nResp)
    dims <- dims[dims > 1]
    if (length(dims) > 0) {
        ## Show the *labs in the margin:
        mXlab <- xlab
        mYlab <- ylab
        xlab <- ylab <- ""
        if(missing(nCols)) nCols <- min(c(3, dims[1]))
        if(missing(nRows))
            nRows <- min(c(3, ceiling(prod(dims[1:2], na.rm = TRUE) / nCols)))
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
        par(mfrow = c(nRows, nCols),
            oma = c(1,1,0,0) + 0.1, mar = c(3,3,3,1) + 0.1)
        if (nRows * nCols < prod(dims)) par(ask = TRUE)
    } else {
        nCols <- nRows <- 1
    }
    if (length(lty) > nLines) lty <- lty[1:nLines] # otherwise legend chokes
    if (sum(nchar(type)) != 1) stop("invalid plot type")
    ## Are we plotting lines?
    dolines <- type %in% c("l", "b", "c", "o", "s", "S", "h")
    ## Are we plotting points?
    dopoints <- type %in% c("p", "b", "o")
  
    coefs <- coef(object, comps = ncomp, intercept = intercept,
                  cumulative = cumulative)
    linenames <- dimnames(coefs)[[3]]
  
    ## Do the plots
    plotNo <- 0
    for (resp in 1:nResp) {
        respname <- dimnames(coefs)[[2]][resp]
        for (size in 1:nSize) {
            plotNo <- plotNo + 1

            if (length(dims) > 0 &&
                (plotNo %% (nCols * nRows) == 0 || plotNo == prod(dims))) {
                ## Last plot on a page; add outer margin text:
                mtext(mXlab, side = 1, outer = TRUE)
                mtext(mYlab, side = 2, outer = TRUE)
            }

            if (separate) {
                plot(coefs[,resp,size],
                     main = paste(respname, linenames[size]), xlab = xlab,
                     ylab = ylab, type = type, lty = lty, lwd = lwd,
                     pch = pch, cex = cex, col = col, ...)
            } else {
                matplot(coefs[,resp,], main = respname, xlab = xlab,
                        ylab = ylab, type = type, lty = lty, lwd = lwd,
                        pch = pch, cex = cex, col = col, ...)
                if (!missing(legendpos)) {
                    do.call("legend", c(list(legendpos, linenames, col = col),
                                        if(dolines) list(lty = lty, lwd = lwd),
                                        if(dopoints) list(pch = pch,
                                                          pt.cex = cex,
                                                          pt.lwd = lwd)))
                }
            }
            abline(h = 0, col = "gray")
        }
    }
}


###
### Validation plot (MSEP/RMSEP/R2)
###

validationplot <- function(object, val.type = c("RMSEP", "MSEP", "R2"),
                           estimate, newdata, comps, intercept, ...)
{
    cl <- match.call(expand.dots = FALSE)
    cl[[1]] <- as.name(match.arg(val.type))
    cl$val.type <- NULL
    x <- eval(cl, parent.frame())
    plot(x, ...)
}

## A plot method for mvrVal objects:
plot.mvrVal <- function(x, nCols, nRows, type = "l", lty = 1:nEst, lwd = NULL,
                        pch = 1:nEst, cex = NULL, col = 1:nEst, legendpos,
                        xlab = "number of components", ylab = x$type, ...)
{
    if (!is.null(x$call$cumulative) && eval(x$call$cumulative) == FALSE)
        stop("`cumulative = FALSE' not supported.")
    ## Set plot parametres as needed:
    nResp <- dim(x$val)[2]              # Number of response variables
    if (nResp > 1) {
        ## Show the *labs in the margin:
        mXlab <- xlab
        mYlab <- ylab
        xlab <- ylab <- ""
        if(missing(nCols)) nCols <- min(c(3, nResp))
        if(missing(nRows)) nRows <- min(c(3, ceiling(nResp / nCols)))
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
        par(mfrow = c(nRows, nCols),
            oma = c(1,1,0,0) + 0.1, mar = c(3,3,3,1) + 0.1)
        if (nRows * nCols < nResp) par(ask = TRUE)
    } else {
        nCols <- nRows <- 1
    }
    ynames <- dimnames(x$val)[[2]]      # Names of response variables
    estnames <- dimnames(x$val)[[1]]    # Names of estimators
    nEst <- length(estnames)
    if (length(lty) > nEst) lty <- lty[1:nEst] # otherwise legend chokes
    if (sum(nchar(type)) != 1) stop("invalid plot type")
    ## Are we plotting lines?
    dolines <- type %in% c("l", "b", "c", "o", "s", "S", "h")
    ## Are we plotting points?
    dopoints <- type %in% c("p", "b", "o")

    for (resp in 1:nResp) {
        if (nResp > 1 && (resp %% (nCols * nRows) == 0 || resp == nResp)) {
            ## Last plot on a page; add outer margin text:
            mtext(mXlab, side = 1, outer = TRUE)
            mtext(mYlab, side = 2, outer = TRUE)
        }
        
        y <- x$val[,resp,]
        if (is.matrix(y)) y <- t(y)
        if (identical(all.equal(x$comps, min(x$comps):max(x$comps)),
                      TRUE)) {
            matplot(x$comps, y, xlab = xlab, ylab = ylab, main = ynames[resp],
                    type = type, lty = lty, lwd = lwd, pch = pch, cex = cex,
                    col = col, ...)
        } else {
            ## Handle irregular x$comps:
            matplot(y, xlab = xlab, ylab = ylab, main = ynames[resp],
                    xaxt = "n", type = type, lty = lty, lwd = lwd,
                    pch = pch, cex = cex, col = col, ...)
            axis(1, seq(along = x$comps), x$comps)
        }
        if (!missing(legendpos)) {
            do.call("legend", c(list(legendpos, estnames, col = col),
                                if (dolines) list(lty = lty, lwd = lwd),
                                if (dopoints) list(pch = pch, pt.cex = cex,
                                                   pt.lwd = lwd)))
        }
    }
}


###
### biplot
###

## FIXME: Check scaling.
## FIXME: The xlabs/ylabs fix is ugly (especially: I want circles!)
## FIXME: Maybe treat xlabs=T as missing xlabs?
## FIXME: The handling of defaults is fishy.  Also, wouldn't it be better to
## call biplot.default instead of biplot?  And is eval() the best choice?
biplot.mvr <- function(x, comps = 1:2,
                       which = c("x", "y", "scores", "loadings"),
                       var.axes = FALSE, xlabs, ylabs, main, ...) {
    if (length(comps) != 2) stop("Exactly 2 components must be selected")
    which <- match.arg(which)
    switch(which,
           x = {
               objects <- x$scores
               vars <- x$loadings
               title <- "X scores and X loadings"
           },
           y = {
               objects <- x$Yscores
               vars <- x$Yloadings
               title <- "Y scores and Y loadings"
           },
           scores = {
               objects <- x$scores
               vars <- x$Yscores
               title <- "X scores and Y scores"
           },
           loadings = {
               objects <- x$loadings
               vars <- x$Yloadings
               title <- "X loadings and Y loadings"
           }
           )
    if (is.null(objects) || is.null(vars))
        stop("'x' lacks the required scores/loadings")
    ## Build a call to `biplot'
    mc <- match.call()
    mc$comps <- mc$which <- NULL
    mc$x <- objects[,comps, drop = FALSE]
    mc$y <- vars[,comps, drop = FALSE]
    if (missing(main)) mc$main <- title
    if (missing(var.axes)) mc$var.axes = FALSE
    if (!missing(xlabs) && is.logical(xlabs) && !xlabs) # i.e. xlabs = FALSE
        mc$xlabs <- rep("o", nrow(objects))
    if (!missing(ylabs) && is.logical(ylabs) && !ylabs) # i.e. ylabs = FALSE
        mc$ylabs <- rep("o", nrow(vars))
    mc[[1]] <- as.name("biplot")
    ## Evaluate the call:
    eval(mc, parent.frame())
}
