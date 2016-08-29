### Plots for mvr objects.  Some of them also work for other
### objects, but that is not a priority.
###
### $Id$

###
### Plot method for mvr objects
###

plot.mvr <- function(x, plottype = c("prediction", "validation",
                        "coefficients", "scores", "loadings", "biplot",
                        "correlation"),
                     ...)
{
    plottype <- match.arg(plottype)
    plotFunc <- switch(plottype,
                       prediction = predplot.mvr,
                       validation = validationplot,
                       coefficients = coefplot,
                       scores = scoreplot,
                       loadings = loadingplot,
                       biplot = biplot.mvr,
                       correlation = corrplot)
    plotFunc(x, ...)
}


###
### Scoreplot
###

scoreplot <- function(object, ...) UseMethod("scoreplot")

scoreplot.default <- function(object, comps = 1:2, labels, identify = FALSE,
                              type = "p", xlab, ylab, ...) {
    ## Check arguments
    nComps <- length(comps)
    if (nComps == 0) stop("At least one component must be selected.")
    ## Get the scores
    if (is.matrix(object)) {
        ## Assume this is already a score matrix
        S <- object[,comps, drop = FALSE]
    } else {
        ## Try to get the scores
        S <- scores(object)[,comps, drop = FALSE]
        if (is.null(S))
            stop("`", deparse(substitute(object)), "' has no scores.")
    }
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
    varlab <- compnames(object, comps, explvar = TRUE)
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

loadingplot <- function(object, ...) UseMethod("loadingplot")

loadingplot.default <- function(object, comps = 1:2, scatter = FALSE, labels,
                                identify = FALSE, type, lty, lwd = NULL, pch,
                                cex = NULL, col, legendpos, xlab, ylab,
                                pretty.xlabels = TRUE, xlim, ...)
{
    ## Check arguments
    nComps <- length(comps)
    if (nComps == 0) stop("At least one component must be selected.")
    if (!missing(type) &&
        (length(type) != 1 || is.na(nchar(type, "c")) || nchar(type, "c") != 1))
        stop("Invalid plot type.")
    ## Get the loadings
    if (is.matrix(object)) {
        ## Assume this is already a loading matrix
        L <- object[,comps, drop = FALSE]
    } else {
        ## Try to get the loadings:
        L <- loadings(object)[,comps, drop = FALSE]
        if (is.null(L))
            stop("`", deparse(substitute(object)), "' has no loadings.")
    }
    varlab <- compnames(object, comps, explvar = TRUE)
    if (scatter) {
        ## Scatter plots
        if (missing(type)) type <- "p"
        if (!missing(labels)) {
            ## Set up point/tick mark labels
            if (length(labels) == 1) {
                labels <- switch(match.arg(labels, c("names", "numbers")),
                                 names = {
                                     if (is.null(rnames <- rownames(L))) {
                                         stop("The loadings have no row names.")
                                     } else {
                                         rnames
                                     }},
                                 numbers = 1:nrow(L)
                                 )
            }
            labels <- as.character(labels)
            type <- "n"
        }
        if (missing(lty)) lty <- NULL
        if (missing(pch)) pch <- NULL
        if (missing(col)) col <- par("col") # `NULL' means `no colour'
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
        xnum <- 1:nrow(L)
        if (missing(labels)) {
            xaxt <- par("xaxt")
        } else {
            xaxt <- "n"
            if (length(labels) == 1) {
                xnam <- rownames(L)
                switch(match.arg(labels, c("names", "numbers")),
                       names = {        # Simply use the names as is
                           labels <- xnam
                       },
                       numbers = {      # Try to use them as numbers
                           if (length(grep("^[-0-9.]+[^0-9]*$", xnam)) ==
                               length(xnam)) {
                               ## Labels are on "num+text" format
                               labels <- sub("[^0-9]*$", "", xnam)
                               if (isTRUE(pretty.xlabels)) {
                                   xnum <- as.numeric(labels)
                                   xaxt <- par("xaxt")
                               }
                           } else {
                               stop("Could not convert variable names to numbers.")
                           }
                       }
                       )
            } else {
                labels <- as.character(labels)
            }
        }
        if (missing(xlim)) xlim <- xnum[c(1, length(xnum))] # Needed for reverted scales
        matplot(xnum, L, xlab = xlab, ylab = ylab, type = type,
                lty = lty, lwd = lwd, pch = pch, cex = cex, col = col,
                xaxt = xaxt, xlim = xlim, ...)
        if (!missing(labels) && xaxt == "n") {
            if (isTRUE(pretty.xlabels)) {
                ticks <- axTicks(1)
                ticks <- ticks[ticks >= 1 & ticks <= length(labels)]
            } else {
                ticks <- 1:length(labels)
            }
            axis(1, ticks, labels[ticks], ...)
        }
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
### Correlation loadings plot
###

corrplot <- function(object, comps = 1:2, labels, radii = c(sqrt(1/2), 1),
                     identify = FALSE, type = "p", xlab, ylab, ...) {
    nComps <- length(comps)
    if (nComps < 2) stop("At least two components must be selected.")
    if (is.matrix(object)) {
        ## Assume this is already a correlation matrix
        cl <- object[,comps, drop = FALSE]
        varlab <- colnames(cl)
    } else {
        S <- scores(object)[,comps, drop = FALSE]
        if (is.null(S))
            stop("`", deparse(substitute(object)), "' has no scores.")
        cl <- cor(model.matrix(object), S)
        varlab <- compnames(object, comps, explvar = TRUE)
    }
    if (!missing(labels)) {
        ## Set up point labels
        if (length(labels) == 1) {
            labels <- switch(match.arg(labels, c("names", "numbers")),
                             names = rownames(cl),
                             numbers = 1:nrow(cl)
                             )
        }
        labels <- as.character(labels)
        type <- "n"
    }
    ## Build the expression to add circles:
    if (length(radii)) {
        addcircles <- substitute(symbols(cent, cent, circles = radii,
                                         inches = FALSE, add = TRUE),
                                 list(cent = rep(0, length(radii))))
    } else {
        addcircles <- expression()
    }
    if (nComps == 2) {
        ## Second component versus first
        if (missing(xlab)) xlab <- varlab[1]
        if (missing(ylab)) ylab <- varlab[2]
        plot(cl, xlim = c(-1,1), ylim = c(-1,1), asp = 1,
             xlab = xlab, ylab = ylab, type = type, ...)
        eval(addcircles)
        segments(x0 = c(-1, 0), y0 = c(0, -1), x1 = c(1, 0), y1 = c(0, 1))
        if (!missing(labels)) text(cl, labels, ...)
        if (identify) {
            if (!is.null(rownames(cl))) {
                identify(cl, labels = rownames(cl))
            } else {
                identify(cl)
            }
        }
    } else {
        ## Pairwise scatterplots of several components
        pointsOrText <- if (missing(labels)) {
            function(x, y, ...) points(x, y, type = type, ...)
        } else {
            function(x, y, ...) text(x, y, labels = labels, ...)
        }
        panel <- function(x, y, ...) {
            ## Ignore the leading `ghost points':
            pointsOrText(x[-(1:2)], y[-(1:2)], ...)
            eval(addcircles)
            segments(x0 = c(-1, 0), y0 = c(0, -1), x1 = c(1, 0),
                     y1 = c(0, 1))
        }
        ## Call `pairs' with two leading `ghost points', to get
        ## correct xlim and ylim:
        pairs(rbind(-1, 1, cl), labels = varlab, panel = panel, asp = 1, ...)
    }
}


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
                         main,
                         ask = nRows * nCols < nPlots && dev.interactive(),
                         ..., font.main, cex.main)
{
    ## Select type(s) of prediction
    if (missing(which)) {
        ## Pick the `best' alternative.
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
    nPlots <- prod(dims)
    if (nPlots > 1) {
        ## Set up default font.main and cex.main for individual titles:
        if (missing(font.main)) font.main <- 1
        if (missing(cex.main)) cex.main <- 1.1
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
            oma = c(1, 1, if(missing(main)) 0 else 2, 0) + 0.1,
            mar = c(3,3,3,1) + 0.1)
        if (isTRUE(ask)) {
            oask <- devAskNewPage(TRUE)
            on.exit(devAskNewPage(oask))
        }
    } else {
        ## Set up default font.main and cex.main for the main title:
        if (missing(font.main)) font.main <- par("font.main")
        if (missing(cex.main)) cex.main <- par("cex.main")
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
        test.predicted <- predict(object, ncomp = ncomp, newdata = newdata)
    }

    ## Do the plots
    plotNo <- 0
    for (resp in 1:nResp) {
        for (size in 1:nSize) {
            for (est in 1:nEst) {
                plotNo <- plotNo + 1
                if (nPlots == 1 && !missing(main)) {
                    lmain <- main
                } else {
                    lmain <- sprintf("%s, %d comps, %s",
                                     respnames(object)[resp],
                                     ncomp[size], which[est])
                }
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
                xy <- predplotXy(measured, predicted, main = lmain,
                                 font.main = font.main, cex.main = cex.main,
                                 xlab = xlab, ylab = ylab, ...)
                if (nPlots > 1 &&
                    (plotNo %% (nCols * nRows) == 0 || plotNo == nPlots)) {
                    ## Last plot on a page; add outer margin text and title:
                    mtext(mXlab, side = 1, outer = TRUE)
                    mtext(mYlab, side = 2, outer = TRUE)
                    if (!missing(main)) title(main = main, outer = TRUE)
                }
            }
        }
    }
    invisible(xy)
}

## The workhorse function:
predplotXy <- function(x, y, line = FALSE, labels, type = "p",
                       main = "Prediction plot", xlab = "measured response",
                       ylab = "predicted response", line.col = par("col"),
                       line.lty = NULL, line.lwd = NULL, ...)
{
    if (!missing(labels)) {
        ## Set up point labels
        if (length(labels) == 1) {
            labels <- switch(match.arg(labels, c("names", "numbers")),
                             names = names(y),
                             numbers = as.character(1:length(y))
                             )
        }
        ## Override plot type:
        type <- "n"
    }
    plot(y ~ x, type = type, main = main, xlab = xlab, ylab = ylab, ...)
    if (!missing(labels)) text(x, y, labels, ...)
    if (line) abline(0, 1, col = line.col, lty = line.lty, lwd = line.lwd)
    invisible(cbind(measured = x, predicted = as.vector(y)))
}


###
### Coefficient plot
###

coefplot <- function(object, ncomp = object$ncomp, comps, intercept = FALSE,
                     separate = FALSE, se.whiskers = FALSE,
                     nCols, nRows, labels,
                     type = "l", lty, lwd = NULL,
                     pch, cex = NULL, col, legendpos,
                     xlab = "variable", ylab = "regression coefficient",
                     main, pretty.xlabels = TRUE, xlim, ylim,
                     ask = nRows * nCols < nPlots && dev.interactive(), ...)
{
    ## This simplifies code below:
    if (missing(comps)) comps <- NULL

    ## Help variables
    nLines <- if (is.null(comps)) length(ncomp) else length(comps)
    nSize <- if (separate) nLines else 1
    nResp <- dim(object$fitted.values)[2]

    ## Set plot parametres as needed:
    dims <- c(nSize, nResp)
    dims <- dims[dims > 1]
    nPlots <- prod(dims)
    if (nPlots > 1) {
        ## Show the *labs in the margin:
        mXlab <- xlab
        mYlab <- ylab
        xlab <- ylab <- ""
        if (missing(nCols)) nCols <- min(c(3, dims[1]))
        if (missing(nRows))
            nRows <- min(c(3, ceiling(prod(dims[1:2], na.rm = TRUE) / nCols)))
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
        par(mfrow = c(nRows, nCols),
            oma = c(1, 1, if(missing(main)) 0 else 2, 0) + 0.1,
            mar = c(3,3,3,1) + 0.1)
        if (isTRUE(ask)) {
            oask <- devAskNewPage(TRUE)
            on.exit(devAskNewPage(oask))
        }
    } else {
        nCols <- nRows <- 1
    }
    if (missing(col)) col <- if (separate) par("col") else 1:nLines
    if (missing(pch)) pch <- if (separate) par("pch") else 1:nLines
    if (missing(lty)) lty <- if (separate) par("lty") else 1:nLines
    if (length(lty) > nLines) lty <- lty[1:nLines] # otherwise legend chokes
    if (length(type) != 1 || is.na(nchar(type, "c")) || nchar(type, "c") != 1)
        stop("Invalid plot type.")
    ## Are we plotting lines?
    dolines <- type %in% c("l", "b", "c", "o", "s", "S", "h")
    ## Are we plotting points?
    dopoints <- type %in% c("p", "b", "o")

    ## Get the coefficients:
    coefs <- coef(object, ncomp = ncomp, comps = comps, intercept = intercept)
    complabs <- dimnames(coefs)[[3]]

    ## Optionally, get the standard errors:
    if (isTRUE(se.whiskers)) {
        if (isTRUE(intercept)) stop(sQuote("se.whiskers"),
                                    " not supported when ",
                                    sQuote("intercept"), " is TRUE")
        if (!is.null(comps))
            stop(sQuote("se.whiskers"), " not supported when ",
                 sQuote("comps"), " is specified")
        if (dim(coefs)[3] > 1 && !isTRUE(separate))
            stop(sQuote("se.whiskers"), " not supported when ",
                 sQuote("separate"), " is FALSE and length(ncomp) > 1")
        SEs <- sqrt(var.jack(object, ncomp = ncomp))
        npred <- dim(SEs)[1]
        if (!hasArg("ylim")) {
            ## Calculate new ylims:
            miny <- apply(coefs - SEs, 2:3, min)
            maxy <- apply(coefs + SEs, 2:3, max)
        }
    }

    ## Set up the x labels:
    xnum <- 1:dim(coefs)[1]
    if (missing(labels)) {
        xaxt <- par("xaxt")
    } else {
        xaxt <- "n"
        if (length(labels) == 1) {
            xnam <- prednames(object, intercept = intercept)
            switch(match.arg(labels, c("names", "numbers")),
                   names = {            # Simply use the names as is
                       labels <- xnam
                   },
                   numbers = {          # Try to use them as numbers
                       if (length(grep("^[-0-9.]+[^0-9]*$", xnam)) ==
                           length(xnam)) {
                           ## Labels are on "num+text" format
                           labels <- sub("[^0-9]*$", "", xnam)
                           if (isTRUE(pretty.xlabels)) {
                               xnum <- as.numeric(labels)
                               xaxt <- par("xaxt")
                           }
                       } else {
                           stop("Could not convert variable names to numbers.")
                       }
                   }
                   )
        } else {
            labels <- as.character(labels)
        }
    }
    if (missing(xlim)) xlim <- xnum[c(1, length(xnum))] # Needed for reverted scales
    ## Do the plots
    plotNo <- 0
    for (resp in 1:nResp) {
        respname <- respnames(object)[resp]
        for (size in 1:nSize) {
            plotNo <- plotNo + 1

            if (nPlots == 1 && !missing(main)) {
                lmain <- main
            } else if (separate) {
                lmain <- paste(respname, complabs[size], sep = ", ")
            } else {
                lmain <- respname
            }
            if (separate) {
                if (missing(ylim)) {
                    if (isTRUE(se.whiskers)) {
                        ylims <- c(miny[resp,size], maxy[resp,size])
                    } else {
                        ylims <- range(coefs[,resp,size])
                    }
                } else {
                    ylims <- ylim
                }
                plot(xnum, coefs[,resp,size],
                     main = lmain, xlab = xlab, ylab = ylab, type = type,
                     lty = lty, lwd = lwd, pch = pch, cex = cex,
                     col = col, xaxt = xaxt, xlim = xlim, ylim = ylims, ...)
                if (isTRUE(se.whiskers)) {
                    arrows(1:npred, (coefs - SEs)[,resp,size],
                           1:npred, (coefs + SEs)[,resp,size], length = 0.05,
                           angle = 90, code = 3, col = 2)
                }
            } else {
                if (missing(ylim)) {
                    if (isTRUE(se.whiskers)) {
                        ylims <- c(miny[resp,], maxy[resp,])
                    } else {
                        ylims <- range(coefs[,resp,])
                    }
                } else {
                    ylims <- ylim
                }
                matplot(xnum, coefs[,resp,], main = lmain, xlab = xlab,
                        ylab = ylab, type = type, lty = lty, lwd = lwd,
                        pch = pch, cex = cex, col = col, xaxt = xaxt,
                        xlim = xlim, ylim = ylims, ...)
                if (isTRUE(se.whiskers)) {
                    arrows(1:npred, (coefs - SEs)[,resp,],
                           1:npred, (coefs + SEs)[,resp,], length = 0.05,
                           angle = 90, code = 3, col = 2)
                }
                if (!missing(legendpos)) {
                    do.call("legend", c(list(legendpos, complabs, col = col),
                                        if(dolines) list(lty = lty, lwd = lwd),
                                        if(dopoints) list(pch = pch,
                                                          pt.cex = cex,
                                                          pt.lwd = lwd)))
                }
            }
            if (!missing(labels) && xaxt == "n") {
                if (isTRUE(pretty.xlabels)) {
                    ticks <- axTicks(1)
                    ticks <- ticks[ticks >= 1 & ticks <= length(labels)]
                } else {
                    ticks <- 1:length(labels)
                }
                axis(1, ticks, labels[ticks], ...)
            }
            abline(h = 0, col = "gray")

            if (nPlots > 1 &&
                (plotNo %% (nCols * nRows) == 0 || plotNo == nPlots)) {
                ## Last plot on a page; add outer margin text and title:
                mtext(mXlab, side = 1, outer = TRUE)
                mtext(mYlab, side = 2, outer = TRUE)
                if (!missing(main)) title(main, outer = TRUE)
            }
        }
    }
}


###
### Validation plot (MSEP/RMSEP/R2)
###

validationplot <- function(object, val.type = c("RMSEP", "MSEP", "R2"),
                           estimate, newdata, ncomp, comps, intercept, ...)
{
    cl <- match.call(expand.dots = FALSE)
    cl[[1]] <- as.name(match.arg(val.type))
    cl$val.type <- NULL
    x <- eval(cl, parent.frame())
    plot(x, ...)
}

## A plot method for mvrVal objects:
plot.mvrVal <- function(x, nCols, nRows, type = "l", lty = 1:nEst,
                        lwd = par("lwd"), pch = 1:nEst, cex = 1, col = 1:nEst,
                        legendpos, xlab = "number of components",
                        ylab = x$type, main,
                        ask = nRows * nCols < nResp && dev.interactive(), ...)
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
            oma = c(1, 1, if(missing(main)) 0 else 2, 0) + 0.1,
            mar = c(3,3,3,1) + 0.1)
        if (isTRUE(ask)) {
            oask <- devAskNewPage(TRUE)
            on.exit(devAskNewPage(oask))
        }
    } else {
        nCols <- nRows <- 1
    }
    ynames <- dimnames(x$val)[[2]]      # Names of response variables
    estnames <- dimnames(x$val)[[1]]    # Names of estimators
    nEst <- length(estnames)
    if (length(lty) > nEst) lty <- lty[1:nEst] # otherwise legend chokes
    if (length(type) != 1 || is.na(nchar(type, "c")) || nchar(type, "c") != 1)
        stop("Invalid plot type.")
    ## Are we plotting lines?
    dolines <- type %in% c("l", "b", "c", "o", "s", "S", "h")
    ## Are we plotting points?
    dopoints <- type %in% c("p", "b", "o")

    for (resp in 1:nResp) {
        if (nResp == 1 && !missing(main)) {
            lmain <- main
        } else {
            lmain <- ynames[resp]
        }
        y <- x$val[,resp,]
        if (is.matrix(y)) y <- t(y)
        if (isTRUE(all.equal(x$comps, min(x$comps):max(x$comps)))) {
            matplot(x$comps, y, xlab = xlab, ylab = ylab, main = lmain,
                    type = type, lty = lty, lwd = lwd, pch = pch, cex = cex,
                    col = col, ...)
        } else {
            ## Handle irregular x$comps:
            matplot(y, xlab = xlab, ylab = ylab, main = lmain,
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
        if (nResp > 1 && (resp %% (nCols * nRows) == 0 || resp == nResp)) {
            ## Last plot on a page; add outer margin text and title:
            mtext(mXlab, side = 1, outer = TRUE)
            mtext(mYlab, side = 2, outer = TRUE)
            if (!missing(main)) title(main, outer = TRUE)
        }
    }
}


###
### biplot
###

biplot.mvr <- function(x, comps = 1:2,
                       which = c("x", "y", "scores", "loadings"),
                       var.axes = FALSE, xlabs, ylabs, main, ...) {
    if (length(comps) != 2) stop("Exactly 2 components must be selected.")
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
        stop("'x' lacks the required scores/loadings.")
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
