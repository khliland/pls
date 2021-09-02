### Plots for mvr objects.  Some of them also work for other
### objects, but that is not a priority.

###
### Plot method for mvr objects
###



#' @title Plot Method for MVR objects
#'
#' @description \code{plot.mvr} plots predictions, coefficients, scores, loadings, biplots,
#' correlation loadings or validation plots (RMSEP curves, etc.).
#'
#' @details The function is simply a wrapper for the underlying plot functions used to
#' make the selected plots.  See \code{\link{predplot.mvr}},
#' \code{\link{validationplot}}, \code{\link{coefplot}},
#' \code{\link{scoreplot}}, \code{\link{loadingplot}}, \code{\link{biplot.mvr}}
#' or \code{\link{corrplot}} for details.  Note that all arguments except
#' \code{x} and \code{plottype} must be named.
#'
#' @param x an object of class \code{mvr}.  The fitted model to plot.
#' @param plottype character.  What kind of plot to plot.
#' @param \dots further arguments, sent to the underlying plot functions.
#' @return \code{plot.mvr} returns whatever the underlying plot function
#' returns.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{predplot.mvr}},
#' \code{\link{validationplot}}, \code{\link{coefplot}},
#' \code{\link{scoreplot}}, \code{\link{loadingplot}},
#' \code{\link{biplot.mvr}}, \code{\link{corrplot}}
#' @keywords regression multivariate hplot
#' @examples
#'
#' data(yarn)
#' nir.pcr <- pcr(density ~ NIR, ncomp = 9, data = yarn, validation = "CV")
#' \dontrun{
#' plot(nir.pcr, ncomp = 5) # Plot of cross-validated predictions
#' plot(nir.pcr, "scores") # Score plot
#' plot(nir.pcr, "loadings", comps = 1:3) # The three first loadings
#' plot(nir.pcr, "coef", ncomp = 5) # Coefficients
#' plot(nir.pcr, "val") # RMSEP curves
#' plot(nir.pcr, "val", val.type = "MSEP", estimate = "CV") # CV MSEP
#' }
#'
#' @export
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



#' @name scoreplot
#' @title Plots of Scores, Loadings and Correlation Loadings
#'
#' @description  Functions to make scatter plots of scores or correlation loadings, and
#' scatter or line plots of loadings.
#'
#' @details \code{plot.scores} is simply a wrapper calling \code{scoreplot}, passing all
#' arguments.  Similarly for \code{plot.loadings}.
#'
#' \code{scoreplot} is generic, currently with a default method that works for
#' matrices and any object for which \code{\link{scores}} returns a matrix.
#' The default \code{scoreplot} method makes one or more scatter plots of the
#' scores, depending on how many components are selected.  If one or two
#' components are selected, and \code{identify} is \code{TRUE}, the function
#' \code{\link{identify}} is used to interactively identify points.
#'
#' Also \code{loadingplot} is generic, with a default method that works for
#' matrices and any object where \code{\link{loadings}} returns a matrix.  If
#' \code{scatter} is \code{TRUE}, the default method works exactly like the
#' default \code{scoreplot} method.  Otherwise, it makes a lineplot of the
#' selected loading vectors, and if \code{identify} is \code{TRUE}, uses
#' \code{\link{identify}} to interactively identify points.  Also, if
#' \code{legendpos} is given, a legend is drawn at the position indicated.
#'
#' \code{corrplot} works exactly like the default \code{scoreplot} method,
#' except that at least two components must be selected.  The
#' \dQuote{correlation loadings}, i.e. the correlations between each variable
#' and the selected components (see References), are plotted as pairwise
#' scatter plots, with concentric circles of radii given by \code{radii}.  Each
#' point corresponds to a variable.  The squared distance between the point and
#' origin equals the fraction of the variance of the variable explained by the
#' components in the panel.  The default \code{radii} corresponds to 50\% and
#' 100\% explained variance.  By default, only the correlation loadings of the
#' \eqn{X} variables are plotted, but if \code{ploty} is \code{TRUE}, also the
#' \eqn{Y} correlation loadings are plotted.
#'
#' \code{scoreplot}, \code{loadingplot} and \code{corrplot} can also be called
#' through the plot method for \code{mvr} objects, by specifying
#' \code{plottype} as \code{"scores"}, \code{"loadings"} or
#' \code{"correlation"}, respectively.  See \code{\link{plot.mvr}}.
#'
#' The argument \code{labels} can be a vector of labels or one of
#' \code{"names"} and \code{"numbers"}.
#'
#' If a scatter plot is produced (i.e., \code{scoreplot}, \code{corrplot}, or
#' \code{loadingplot} with \code{scatter = TRUE}), the labels are used instead
#' of plot symbols for the points plotted.  If \code{labels} is \code{"names"}
#' or \code{"numbers"}, the row names or row numbers of the matrix (scores,
#' loadings or correlation loadings) are used.
#'
#' If a line plot is produced (i.e., \code{loadingplot}), the labels are used
#' as \eqn{x} axis labels.  If \code{labels} is \code{"names"} or
#' \code{"numbers"}, the variable names are used as labels, the difference
#' being that with \code{"numbers"}, the variable names are converted to
#' numbers, if possible.  Variable names of the forms \samp{"number"} or
#' \samp{"number text"} (where the space is optional), are handled.
#'
#' The argument \code{pretty.xlabels} is only used when \code{labels} is
#' specified for a line plot.  If \code{TRUE} (default), the code tries to use
#' a \sQuote{pretty} selection of labels.  If \code{labels} is
#' \code{"numbers"}, it also uses the numerical values of the labels for
#' horisontal spacing.  If one has excluded parts of the spectral region, one
#' might therefore want to use \code{pretty.xlabels = FALSE}.
#'
#' @aliases scoreplot scoreplot.default plot.scores loadingplot
#' loadingplot.default plot.loadings corrplot
#' @param object an object.  The fitted model.
#' @param comps integer vector.  The components to plot.
#' @param scatter logical.  Whether the loadings should be plotted as a scatter
#' instead of as lines.
#' @param labels optional.  Alternative plot labels or \eqn{x} axis labels.
#' See Details.
#' @param plotx locical.  Whether to plot the \eqn{X} correlation loadings.
#' Defaults to \code{TRUE}.
#' @param ploty locical.  Whether to plot the \eqn{Y} correlation loadings.
#' Defaults to \code{FALSE}.
#' @param radii numeric vector, giving the radii of the circles drawn in
#' \code{corrplot}.  The default radii represent 50\% and 100\% explained
#' variance of the \eqn{X} variables by the chosen components.
#' @param identify logical.  Whether to use \code{identify} to interactively
#' identify points.  See below.
#' @param type character.  What type of plot to make.  Defaults to \code{"p"}
#' (points) for scatter plots and \code{"l"} (lines) for line plots.  See
#' \code{\link{plot}} for a complete list of types (not all types are
#' possible/meaningful for all plots).
#' @param lty vector of line types (recycled as neccessary).  Line types can be
#' specified as integers or character strings (see \code{\link{par}} for the
#' details).
#' @param lwd vector of positive numbers (recycled as neccessary), giving the
#' width of the lines.
#' @param pch plot character.  A character string or a vector of single
#' characters or integers (recycled as neccessary).  See \code{\link{points}}
#' for all alternatives.
#' @param cex numeric vector of character expansion sizes (recycled as
#' neccessary) for the plotted symbols.
#' @param col character or integer vector of colors for plotted lines and
#' symbols (recycled as neccessary).  See \code{\link{par}} for the details.
#' @param legendpos Legend position.  Optional.  Ignored if \code{scatter} is
#' \code{TRUE}.  If present, a legend is drawn at the given position.  The
#' position can be specified symbolically (e.g., \code{legendpos =
#' "topright"}).  This requires >= 2.1.0.  Alternatively, the position can be
#' specified explicitly (\code{legendpos = t(c(x,y))}) or interactively
#' (\code{legendpos = \link{locator}()}).
#' @param xlab,ylab titles for \eqn{x} and \eqn{y} axes.  Typically character
#' strings, but can be expressions or lists.  See \code{\link{title}} for
#' details.
#' @param pretty.xlabels logical.  If \code{TRUE}, \code{loadingplot} tries to
#' plot the \eqn{x} labels more nicely.  See Details.
#' @param xlim optional vector of length two, with the \eqn{x} limits of the
#' plot.
#' @param x a \code{scores} or \code{loadings} object.  The scores or loadings
#' to plot.
#' @param \dots further arguments sent to the underlying plot function(s).
#' @return The functions return whatever the underlying plot function (or
#' \code{identify}) returns.
#' @note \code{\link{legend}} has many options.  If you want greater control
#' over the appearance of the legend, omit the \code{legendpos} argument and
#' call \code{legend} manually.
#'
#' Graphical parametres (such as \code{pch} and \code{cex}) can also be used
#' with \code{scoreplot} and \code{corrplot}.  They are not listed in the
#' argument list simply because they are not handled specifically in the
#' function (unlike in \code{loadingplot}), but passed directly to the
#' underlying plot functions by \code{\dots{}}.
#'
#' Tip: If the labels specified with \code{labels} are too long, they get
#' clipped at the border of the plot region.  This can be avoided by supplying
#' the graphical parameter \code{xpd = TRUE} in the plot call.
#'
#' The handling of \code{labels} and \code{pretty.xlabels} in \code{coefplot}
#' is experimental.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{plot.mvr}}, \code{\link{scores}},
#' \code{\link{loadings}}, \code{\link{identify}}, \code{\link{legend}}
#' @references Martens, H., Martens, M. (2000) Modified Jack-knife Estimation
#' of Parameter Uncertainty in Bilinear Modelling by Partial Least Squares
#' Regression (PLSR).  \emph{Food Quality and Preference}, \bold{11}(1--2),
#' 5--16.
#' @keywords regression multivariate hplot
#' @examples
#'
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 10, data = yarn)
#' ## These three are equivalent:
#' \dontrun{
#' scoreplot(mod, comps = 1:5)
#' plot(scores(mod), comps = 1:5)
#' plot(mod, plottype = "scores", comps = 1:5)
#'
#' loadingplot(mod, comps = 1:5)
#' loadingplot(mod, comps = 1:5, legendpos = "topright") # With legend
#' loadingplot(mod, comps = 1:5, scatter = TRUE) # Plot as scatterplots
#'
#' corrplot(mod, comps = 1:2)
#' corrplot(mod, comps = 1:3)
#' }
#'
#' @export
scoreplot <- function(object, ...) UseMethod("scoreplot")

#' @rdname scoreplot
#' @export
scoreplot.default <- function(object, comps = 1:2, labels, identify = FALSE,
                              type = "p", xlab, ylab, ...)
{
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
        if (isTRUE(identify)) {
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
#' @rdname scoreplot
#' @export
plot.scores <- function(x, ...) scoreplot(x, ...)


###
### Loadingplot
###

#' @rdname scoreplot
#' @export
loadingplot <- function(object, ...) UseMethod("loadingplot")

#' @rdname scoreplot
#' @export
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
    if (isTRUE(scatter)) {
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
            if (isTRUE(identify))
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
    } else {                            # if (isTRUE(scatter))
        ## Line plots
        if (missing(type)) type <- "l"
        if (missing(lty))  lty  <- 1:nComps
        if (missing(pch))  pch  <- 1:nComps
        if (missing(col))  col  <- 1:nComps
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
        if (isTRUE(identify))
            identify(c(row(L)), c(L),
                     labels = paste(c(col(L)), rownames(L), sep = ": "))
    }                                   # if (isTRUE(scatter))
}

## A plot method for loadings (loadings, loading.weights or Yloadings):
#' @rdname scoreplot
#' @export
plot.loadings <- function(x, ...) loadingplot(x, ...)


###
### Correlation loadings plot
###

#' @rdname scoreplot
#' @export
corrplot <- function(object, comps = 1:2, labels, plotx = TRUE, ploty = FALSE,
                     radii = c(sqrt(1/2), 1), identify = FALSE,
                     type = "p", xlab, ylab, col, ...)
{
    nComps <- length(comps)
    if (nComps < 2) stop("At least two components must be selected.")
    if (is.matrix(object)) {
        ## Assume this is already a correlation matrix
        cl <- object[,comps, drop = FALSE]
        numX <- nrow(cl)
        numY <- 0
        varlab <- colnames(cl)
    } else {
        S <- scores(object)[,comps, drop = FALSE]
        if (is.null(S))
            stop("`", deparse(substitute(object)), "' has no scores.")

        if (isTRUE(plotx)) {
            clX <- cor(model.matrix(object), S)
            numX <- nrow(clX)
        } else {
            clX <- NULL
            numX <- 0
        }
        if (isTRUE(ploty)) {
            clY <- cor(model.response(model.frame(object)), S)
            numY <- nrow(clY)
            if (numY == 1) {
                ## Add response name for single response models
                rownames(clY) <- all.vars(formula(object))[1]
            }
        } else {
            clY <- NULL
            numY <- 0
        }
        cl <- rbind(clX, clY)
        varlab <- compnames(object, comps, explvar = TRUE)
    }
    if (missing(col)) {
        ## Set up default colours:
        col <- c(rep(1, numX), rep(2, numY))
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
             xlab = xlab, ylab = ylab, type = type, col = col, ...)
        eval(addcircles)
        segments(x0 = c(-1, 0), y0 = c(0, -1), x1 = c(1, 0), y1 = c(0, 1))
        if (!missing(labels)) text(cl, labels, col = col, ...)
        if (isTRUE(identify)) {
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
        pairs(rbind(-1, 1, cl), labels = varlab, panel = panel, asp = 1,
              col = col, ...)
    }
}


###
### prediction plot
###

## Generic:


#' @name predplot
#' @title Prediction Plots
#'
#' @description Functions to plot predicted values against measured values for a fitted
#' model.
#'
#' @details \code{predplot} is a generic function for plotting predicted versus measured
#' response values, with default and \code{mvr} methods currently implemented.
#' The default method is very simple, and doesn't handle multiple responses or
#' new data.
#'
#' The \code{mvr} method, handles multiple responses, model sizes and types of
#' predictions by making one plot for each combination.  It can also be called
#' through the plot method for \code{mvr}, by specifying \code{plottype =
#' "prediction"} (the default).
#'
#' The argument \code{main} can be used to specify the main title of the plot.
#' It is handled in a non-standard way.  If there is only on (sub) plot,
#' \code{main} will be used as the main title of the plot.  If there is
#' \emph{more} than one (sub) plot, however, the presence of \code{main} will
#' produce a corresponding \sQuote{global} title on the page.  Any graphical
#' parametres, e.g., \code{cex.main}, supplied to \code{coefplot} will only
#' affect the \sQuote{ordinary} plot titles, not the \sQuote{global} one.  Its
#' appearance can be changed by setting the parameters with \code{\link{par}},
#' which will affect \emph{both} titles (with the exception of \code{font.main}
#' and \code{cex.main}, which will only affect the \sQuote{global} title when
#' there is more than one plot).  (To have different settings for the two
#' titles, one can override the \code{par} settings with arguments to
#' \code{predplot}.)
#'
#' \code{predplotXy} is an internal function and is not meant for interactive
#' use.  It is called by the \code{predplot} methods, and its arguments, e.g,
#' \code{line}, can be given in the \code{predplot} call.
#'
#' @aliases predplot predplot.default predplot.mvr predplotXy
#' @param object a fitted model.
#' @param ncomp integer vector.  The model sizes (numbers of components) to use
#' for prediction.
#' @param which character vector.  Which types of predictions to plot.  Should
#' be a subset of \code{c("train", "validation", "test")}.  If not specified,
#' \code{plot.mvr} selects test set predictions if \code{newdata} is supplied,
#' otherwise cross-validated predictions if the model has been cross-validated,
#' otherwise fitted values from the calibration data.
#' @param newdata data frame.  New data to predict.
#' @param nCols,nRows integer.  The number of coloumns and rows the plots will
#' be laid out in.  If not specified, \code{plot.mvr} tries to be intelligent.
#' @param xlab,ylab titles for \eqn{x} and \eqn{y} axes.  Typically character
#' strings, but can be expressions or lists.  See \code{\link{title}} for
#' details.
#' @param labels optional.  Alternative plot labels to use.  Either a vector of
#' labels, or \code{"names"} or \code{"numbers"} to use the row names or row
#' numbers of the data as labels.
#' @param type character.  What type of plot to make.  Defaults to \code{"p"}
#' (points).  See \code{\link{plot}} for a complete list of types.  The
#' argument is ignored if \code{labels} is specified.
#' @param main optional main title for the plot.  See Details.
#' @param ask logical.  Whether to ask the user before each page of a plot.
#' @param font.main font to use for main titles.  See \code{\link{par}} for
#' details.  Also see Details below.
#' @param cex.main numeric.  The magnification to be used for main titles
#' relative to the current size.  Also see Details below.
#' @param x numeric vector.  The observed response values.
#' @param y numeric vector.  The predicted response values.
#' @param line logical.  Whether a target line should be drawn.
#' @param line.col,line.lty,line.lwd character or numeric.  The \code{col},
#' \code{lty} and \code{lwd} parametres for the target line.  See
#' \code{\link{par}} for details.
#' @param \dots further arguments sent to underlying plot functions.
#' @return The functions invisibly return a matrix with the (last) plotted
#' data.
#' @note The \code{font.main} and \code{cex.main} must be (completely) named.
#' This is to avoid that any argument \code{cex} or \code{font} matches them.
#'
#' Tip: If the labels specified with \code{labels} are too long, they get
#' clipped at the border of the plot region.  This can be avoided by supplying
#' the graphical parameter \code{xpd = TRUE} in the plot call.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{plot.mvr}}
#' @keywords regression multivariate hplot
#' @examples
#'
#' data(yarn)
#' mod <- plsr(density ~ NIR, ncomp = 10, data = yarn[yarn$train,], validation = "CV")
#' \dontrun{
#' predplot(mod, ncomp = 1:6)
#' plot(mod, ncomp = 1:6) # Equivalent to the previous
#' ## Both cross-validated and test set predictions:
#' predplot(mod, ncomp = 4:6, which = c("validation", "test"),
#'          newdata = yarn[!yarn$train,])
#' }
#'
#' data(oliveoil)
#' mod.sens <- plsr(sensory ~ chemical, ncomp = 4, data = oliveoil)
#' \dontrun{plot(mod.sens, ncomp = 2:4) # Several responses gives several plots}
#'
#' @export
predplot <- function(object, ...)
  UseMethod("predplot")

## Default method:
#' @rdname predplot
#' @export
predplot.default <- function(object, ...) {
    measured <- model.response(model.frame(object))
    predicted <- predict(object)
    predplotXy(measured, predicted, ...)
}

## Method for mvr objects:
#' @rdname predplot
#' @export
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
    nEst  <- length(which)
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
#' @rdname predplot
#' @export
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
    if (isTRUE(line)) abline(0, 1, col = line.col, lty = line.lty, lwd = line.lwd)
    invisible(cbind(measured = x, predicted = as.vector(y)))
}


###
### Coefficient plot
###



#' @title Plot Regression Coefficients of PLSR and PCR models
#'
#' @description  Function to plot the regression coefficients of an \code{mvr} object.
#'
#' @details \code{coefplot} handles multiple responses by making one plot for each
#' response.  If \code{separate} is \code{TRUE}, separate plots are made for
#' each combination of model size and response.  The plots are laid out in a
#' rectangular fashion.
#'
#' If \code{legendpos} is given, a legend is drawn at the given position
#' (unless \code{separate} is \code{TRUE}).
#'
#' The argument \code{labels} can be a vector of labels or one of
#' \code{"names"} and \code{"numbers"}.  The labels are used as \eqn{x} axis
#' labels.  If \code{labels} is \code{"names"} or \code{"numbers"}, the
#' variable names are used as labels, the difference being that with
#' \code{"numbers"}, the variable names are converted to numbers, if possible.
#' Variable names of the forms \samp{"number"} or \samp{"number text"} (where
#' the space is optional), are handled.
#'
#' The argument \code{main} can be used to specify the main title of the plot.
#' It is handled in a non-standard way.  If there is only on (sub) plot,
#' \code{main} will be used as the main title of the plot.  If there is
#' \emph{more} than one (sub) plot, however, the presence of \code{main} will
#' produce a corresponding \sQuote{global} title on the page.  Any graphical
#' parametres, e.g., \code{cex.main}, supplied to \code{coefplot} will only
#' affect the \sQuote{ordinary} plot titles, not the \sQuote{global} one.  Its
#' appearance can be changed by setting the parameters with \code{\link{par}},
#' which will affect \emph{both} titles.  (To have different settings for the
#' two titles, one can override the \code{par} settings with arguments to
#' \code{coefplot}.)
#'
#' The argument \code{pretty.xlabels} is only used when \code{labels} is
#' specified.  If \code{TRUE} (default), the code tries to use a
#' \sQuote{pretty} selection of labels.  If \code{labels} is \code{"numbers"},
#' it also uses the numerical values of the labels for horisontal spacing.  If
#' one has excluded parts of the spectral region, one might therefore want to
#' use \code{pretty.xlabels = FALSE}.
#'
#' When \code{separate} is \code{TRUE}, the arguments \code{lty}, \code{col},
#' and \code{pch} default to their \code{par()} setting.  Otherwise, the
#' default for all of them is \code{1:nLines}, where \code{nLines} is the
#' number of model sizes specified, i.e., the length of \code{ncomp} or
#' \code{comps}.
#'
#' The function can also be called through the \code{mvr} plot method by
#' specifying \code{plottype = "coefficients"}.
#'
#' @param object an \code{mvr} object.  The fitted model.
#' @param ncomp,comps vector of positive integers.  The components to plot.
#' See \code{\link{coef.mvr}} for details.
#' @param separate logical.  If \code{TRUE}, coefficients for different model
#' sizes are blotted in separate plots.
#' @param se.whiskers logical.  If \code{TRUE}, whiskers at plus/minus 1
#' estimated standard error are added to the plot.  This is only available if
#' the model was cross-validated with \code{jackknife = TRUE}.  Also, in the
#' current implementation, \code{intercept} must be \code{FALSE}, and
#' \code{separate} must be \code{TRUE} if \code{length(ncomp) > 1}.
#' @param intercept logical.  Whether coefficients for the intercept should be
#' plotted.  Ignored if \code{comps} is specified.  Defaults to \code{FALSE}.
#' See \code{\link{coef.mvr}} for details.
#' @param nCols,nRows integer.  The number of coloumns and rows the plots will
#' be laid out in.  If not specified, \code{coefplot} tries to be intelligent.
#' @param labels optional.  Alternative \eqn{x} axis labels.  See Details.
#' @param type character.  What type of plot to make.  Defaults to \code{"l"}
#' (lines).  Alternative types include \code{"p"} (points) and \code{"b"}
#' (both).  See \code{\link{plot}} for a complete list of types.
#' @param lty vector of line types (recycled as neccessary).  Line types can be
#' specified as integers or character strings (see \code{\link{par}} for the
#' details).
#' @param lwd vector of positive numbers (recycled as neccessary), giving the
#' width of the lines.
#' @param pch plot character.  A character string or a vector of single
#' characters or integers (recycled as neccessary).  See \code{\link{points}}
#' for all alternatives.
#' @param cex numeric vector of character expansion sizes (recycled as
#' neccessary) for the plotted symbols.
#' @param col character or integer vector of colors for plotted lines and
#' symbols (recycled as neccessary).  See \code{\link{par}} for the details.
#' @param legendpos Legend position.  Optional.  Ignored if \code{separate} is
#' \code{TRUE}.  If present, a legend is drawn at the given position.  The
#' position can be specified symbolically (e.g., \code{legendpos =
#' "topright"}).  This requires >= 2.1.0.  Alternatively, the position can be
#' specified explicitly (\code{legendpos = t(c(x,y))}) or interactively
#' (\code{legendpos = \link{locator}()}).  This only works well for plots of
#' single-response models.
#' @param xlab,ylab titles for \eqn{x} and \eqn{y} axes.  Typically character
#' strings, but can be expressions (e.g., \code{expression(R^2)} or lists.  See
#' \code{\link{title}} for details.
#' @param main optional main title for the plot.  See Details.
#' @param pretty.xlabels logical.  If \code{TRUE}, \code{coefplot} tries to
#' plot the \eqn{x} labels more nicely.  See Details.
#' @param xlim,ylim optional vector of length two, with the \eqn{x} or \eqn{y}
#' limits of the plot.
#' @param ask logical.  Whether to ask the user before each page of a plot.
#' @param \dots Further arguments sent to the underlying plot functions.
#' @note \code{\link{legend}} has many options.  If you want greater control
#' over the appearance of the legend, omit the \code{legendpos} argument and
#' call \code{legend} manually.
#'
#' The handling of \code{labels} and \code{pretty.xlabels} is experimental.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{plot.mvr}}, \code{\link{coef.mvr}},
#' \code{\link{plot}}, \code{\link{legend}}
#' @keywords regression multivariate hplot
#' @examples
#'
#' data(yarn)
#' mod.nir <- plsr(density ~ NIR, ncomp = 8, data = yarn)
#' \dontrun{
#' coefplot(mod.nir, ncomp = 1:6)
#' plot(mod.nir, plottype = "coefficients", ncomp = 1:6) # Equivalent to the previous
#' ## Plot with legend:
#' coefplot(mod.nir, ncom = 1:6, legendpos = "bottomright")
#' }
#'
#' data(oliveoil)
#' mod.sens <- plsr(sensory ~ chemical, ncomp = 4, data = oliveoil)
#' \dontrun{coefplot(mod.sens, ncomp = 2:4, separate = TRUE)}
#'
#' @export
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
    separate <- isTRUE(separate)
    se.whiskers <- isTRUE(se.whiskers)

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
    if (se.whiskers) {
        if (isTRUE(intercept))
            stop(sQuote("se.whiskers"), " not supported when ",
                 sQuote("intercept"), " is TRUE")
        if (!is.null(comps))
            stop(sQuote("se.whiskers"), " not supported when ",
                 sQuote("comps"), " is specified")
        if (dim(coefs)[3] > 1 && !separate)
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
                    if (se.whiskers) {
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
                if (se.whiskers) {
                    arrows(1:npred, (coefs - SEs)[,resp,size],
                           1:npred, (coefs + SEs)[,resp,size], length = 0.05,
                           angle = 90, code = 3, col = 2)
                }
            } else {
                if (missing(ylim)) {
                    if (se.whiskers) {
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
                if (se.whiskers) {
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



#' @name validationplot
#' @title Validation Plots
#'
#' @description Functions to plot validation statistics, such as RMSEP or \eqn{R^2}, as a
#' function of the number of components.
#'
#' @details \code{validationplot} calls the proper validation function (currently
#' \code{\link{MSEP}}, \code{\link{RMSEP}} or \code{\link{R2}}) and plots the
#' results with \code{plot.mvrVal}.  \code{validationplot} can be called
#' through the \code{mvr} plot method, by specifying \code{plottype =
#' "validation"}.
#'
#' \code{plot.mvrVal} creates one plot for each response variable in the model,
#' laid out in a rectangle.  It uses \code{\link{matplot}} for performing the
#' actual plotting.  If \code{legendpos} is given, a legend is drawn at the
#' given position.
#'
#' The argument \code{main} can be used to specify the main title of the plot.
#' It is handled in a non-standard way.  If there is only on (sub) plot,
#' \code{main} will be used as the main title of the plot.  If there is
#' \emph{more} than one (sub) plot, however, the presence of \code{main} will
#' produce a corresponding \sQuote{global} title on the page.  Any graphical
#' parametres, e.g., \code{cex.main}, supplied to \code{coefplot} will only
#' affect the \sQuote{ordinary} plot titles, not the \sQuote{global} one.  Its
#' appearance can be changed by setting the parameters with \code{\link{par}},
#' which will affect \emph{both} titles.  (To have different settings for the
#' two titles, one can override the \code{par} settings with arguments to the
#' plot function.)
#'
#' @aliases validationplot plot.mvrVal
#' @param object an \code{mvr} object.
#' @param val.type character.  What type of validation statistic to plot.
#' @param estimate character.  Which estimates of the statistic to calculate.
#' See \code{\link{RMSEP}}.
#' @param newdata data frame.  Optional new data used to calculate statistic.
#' @param ncomp,comps integer vector.  The model sizes to compute the statistic
#' for.  See \code{\link{RMSEP}}.
#' @param intercept logical.  Whether estimates for a model with zero
#' components should be calculated as well.
#' @param x an \code{mvrVal} object.  Usually the result of a
#' \code{\link{RMSEP}}, \code{\link{MSEP}} or \code{\link{R2}} call.
#' @param nCols,nRows integers.  The number of coloumns and rows the plots will
#' be laid out in.  If not specified, \code{plot.mvrVal} tries to be
#' intelligent.
#' @param type character.  What type of plots to create.  Defaults to
#' \code{"l"} (lines).  Alternative types include \code{"p"} (points) and
#' \code{"b"} (both).  See \code{\link{plot}} for a complete list of types.
#' @param lty vector of line types (recycled as neccessary).  Line types can be
#' specified as integers or character strings (see \code{\link{par}} for the
#' details).
#' @param lwd vector of positive numbers (recycled as neccessary), giving the
#' width of the lines.
#' @param pch plot character.  A character string or a vector of single
#' characters or integers (recycled as neccessary).  See \code{\link{points}}
#' for all alternatives.
#' @param cex numeric vector of character expansion sizes (recycled as
#' neccessary) for the plotted symbols.
#' @param col character or integer vector of colors for plotted lines and
#' symbols (recycled as neccessary).  See \code{\link{par}} for the details.
#' @param legendpos Legend position.  Optional.  If present, a legend is drawn
#' at the given position.  The position can be specified symbolically (e.g.,
#' \code{legendpos = "topright"}).  This requires >= 2.1.0.  Alternatively, the
#' position can be specified explicitly (\code{legendpos = t(c(x,y))}) or
#' interactively (\code{legendpos = \link{locator}()}).  This only works well
#' for plots of single-response models.
#' @param xlab,ylab titles for \eqn{x} and \eqn{y} axes.  Typically character
#' strings, but can be expressions (e.g., \code{expression(R^2)} or lists.  See
#' \code{\link{title}} for details.
#' @param main optional main title for the plot.  See Details.
#' @param ask logical.  Whether to ask the user before each page of a plot.
#' @param \dots Further arguments sent to underlying plot functions.
#' @note \code{\link{legend}} has many options.  If you want greater control
#' over the appearance of the legend, omit the \code{legendpos} argument and
#' call \code{legend} manually.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{plot.mvr}}, \code{\link{RMSEP}},
#' \code{\link{MSEP}}, \code{\link{R2}}, \code{\link{matplot}},
#' \code{\link{legend}}
#' @keywords regression multivariate hplot
#' @examples
#'
#' data(oliveoil)
#' mod <- plsr(sensory ~ chemical, data = oliveoil, validation = "LOO")
#' \dontrun{
#' ## These three are equivalent:
#' validationplot(mod, estimate = "all")
#' plot(mod, "validation", estimate = "all")
#' plot(RMSEP(mod, estimate = "all"))
#' ## Plot R2:
#' plot(mod, "validation", val.type = "R2")
#' ## Plot R2, with a legend:
#' plot(mod, "validation", val.type = "MSEP", legendpos = "top") # R >= 2.1.0
#' }
#'
#' @export
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
#' @rdname validationplot
#' @export
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



#' @name biplot.mvr
#' @title Biplots of PLSR and PCR Models.
#'
#' @description Biplot method for \code{mvr} objects.
#'
#' @details \code{biplot.mvr} can also be called through the \code{mvr} plot method by
#' specifying \code{plottype = "biplot"}.
#'
#' @param x an \code{mvr} object.
#' @param comps integer vector of length two.  The components to plot.
#' @param which character.  Which matrices to plot.  One of \code{"x"} (X
#' scores and loadings), \code{"y"} (Y scores and loadings), \code{"scores"} (X
#' and Y scores) and \code{"loadings"} (X and Y loadings).
#' @param var.axes logical.  If \code{TRUE}, the second set of points have
#' arrows representing them.
#' @param xlabs either a character vector of labels for the first set of
#' points, or \code{FALSE} for no labels.  If missing, the row names of the
#' first matrix is used as labels.
#' @param ylabs either a character vector of labels for the second set of
#' points, or \code{FALSE} for no labels.  If missing, the row names of the
#' second matrix is used as labels.
#' @param main character.  Title of plot.  If missing, a title is constructed
#' by \code{biplot.mvr}.
#' @param \dots Further arguments passed on to \code{biplot.default}.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{plot.mvr}},
#' \code{\link{biplot.default}}
#' @keywords regression multivariate hplot
#' @examples
#'
#' data(oliveoil)
#' mod <- plsr(sensory ~ chemical, data = oliveoil)
#' \dontrun{
#' ## These are equivalent
#' biplot(mod)
#' plot(mod, plottype = "biplot")
#'
#' ## The four combinations of x and y points:
#' par(mfrow = c(2,2))
#' biplot(mod, which = "x") # Default
#' biplot(mod, which = "y")
#' biplot(mod, which = "scores")
#' biplot(mod, which = "loadings")
#' }
#'
#' @export
biplot.mvr <- function(x, comps = 1:2,
                       which = c("x", "y", "scores", "loadings"),
                       var.axes = FALSE, xlabs, ylabs, main, ...)
{
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
    if (!missing(xlabs) && isFALSE(xlabs))
        mc$xlabs <- rep("o", nrow(objects))
    if (!missing(ylabs) && isFALSE(ylabs))
        mc$ylabs <- rep("o", nrow(vars))
    mc[[1]] <- as.name("biplot")
    ## Evaluate the call:
    eval(mc, parent.frame())
}
