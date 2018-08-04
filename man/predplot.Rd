%% $Id$
\encoding{UTF-8}
\name{predplot}
\alias{predplot}
\alias{predplot.default}
\alias{predplot.mvr}
\alias{predplotXy}
\title{Prediction Plots}
\description{
  Functions to plot predicted values against measured values for a
  fitted model.
}
\usage{
predplot(object, \dots)
\method{predplot}{default}(object, \dots)
\method{predplot}{mvr}(object, ncomp = object$ncomp, which, newdata, nCols, 
         nRows, xlab = "measured", ylab = "predicted", main,
         ask = nRows * nCols < nPlots && dev.interactive(),
         \dots, font.main, cex.main)
predplotXy(x, y, line = FALSE, labels, type = "p", main = "Prediction plot",
           xlab = "measured response", ylab = "predicted response",
           line.col = par("col"), line.lty = NULL, line.lwd = NULL, \dots)
}
\arguments{
  \item{object}{a fitted model.}
  \item{ncomp}{integer vector.  The model sizes (numbers of components)
    to use for prediction.}
  \item{which}{character vector.  Which types of predictions to plot.
    Should be a subset of \code{c("train", "validation", "test")}.  If not
    specified, \code{plot.mvr} selects test set predictions if
    \code{newdata} is supplied, otherwise cross-validated predictions if
    the model has been cross-validated, otherwise fitted values from the
    calibration data.}
  \item{newdata}{data frame.  New data to predict.}
  \item{nCols, nRows}{integer.  The number of coloumns and rows the
    plots will be laid out in.  If not specified, \code{plot.mvr} tries
    to be intelligent.}
  \item{xlab,ylab}{titles for \eqn{x} and \eqn{y} axes.  Typically
    character strings, but can be expressions or lists.  See
    \code{\link{title}} for details.}
  \item{labels}{optional.  Alternative plot labels to use.  Either a
    vector of labels, or \code{"names"} or \code{"numbers"} to use the
    row names or row numbers of the data as labels.}
  \item{type}{character.  What type of plot to make.  Defaults to
    \code{"p"} (points).  See \code{\link{plot}} for a complete list
    of types.  The argument is ignored if \code{labels} is specified.}
  \item{main}{optional main title for the plot.  See Details.}
  \item{ask}{logical.  Whether to ask the user before each page of a plot.}
  \item{font.main}{font to use for main titles.  See \code{\link{par}}
    for details.  Also see Details below.}
  \item{cex.main}{numeric.  The magnification to be used for main titles
    relative to the current size.  Also see Details below.}
  \item{x}{numeric vector.  The observed response values.}
  \item{y}{numeric vector.  The predicted response values.}
  \item{line}{logical.  Whether a target line should be drawn.}
  \item{line.col, line.lty, line.lwd}{character or numeric.  The
    \code{col}, \code{lty} and \code{lwd} parametres for the target
    line.  See \code{\link{par}} for details.}
  \item{\dots}{further arguments sent to underlying plot functions.}
}
\details{
  \code{predplot} is a generic function for plotting predicted versus
  measured response values, with default and \code{mvr} methods
  currently implemented.  The default method is very simple, and doesn't
  handle multiple responses or new data.

  The \code{mvr} method, handles multiple responses,
  model sizes and types of predictions by making one plot for each
  combination.  It can also be called through the plot method for
  \code{mvr}, by specifying \code{plottype = "prediction"} (the
  default).

  The argument \code{main} can be used to specify the main title of the
  plot.  It is handled in a non-standard way.  If there is only on (sub)
  plot, \code{main} will be used as the main title of the plot.  If
  there is \emph{more} than one (sub) plot, however, the presence of
  \code{main} will produce a corresponding \sQuote{global} title on the
  page.  Any graphical parametres, e.g., \code{cex.main}, supplied to
  \code{coefplot} will only affect the \sQuote{ordinary} plot titles,
  not the \sQuote{global} one.  Its appearance can be changed by setting
  the parameters with \code{\link{par}}, which will affect \emph{both}
  titles (with the exception of \code{font.main} and \code{cex.main},
  which will only affect the \sQuote{global} title when there is more
  than one plot).  (To have different settings for the two titles, one can
  override the \code{par} settings with arguments to \code{predplot}.)

  \code{predplotXy} is an internal function and is not meant for
  interactive use.  It is called by the \code{predplot} methods, and its
  arguments, e.g, \code{line}, can be given in the \code{predplot} call.
}
\value{
  The functions invisibly return a matrix with the (last) plotted data.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\note{
  The \code{font.main} and \code{cex.main} must be (completely) named.
  This is to avoid that any argument \code{cex} or \code{font} matches
  them.

  Tip: If the labels specified with \code{labels} are too long, they get
  clipped at the border of the plot region.  This can be avoided by
  supplying the graphical parameter \code{xpd = TRUE} in the plot call.
}
\seealso{\code{\link{mvr}}, \code{\link{plot.mvr}}}
\examples{
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 10, data = yarn[yarn$train,], validation = "CV")
\dontrun{
predplot(mod, ncomp = 1:6)
plot(mod, ncomp = 1:6) # Equivalent to the previous
## Both cross-validated and test set predictions:
predplot(mod, ncomp = 4:6, which = c("validation", "test"),
         newdata = yarn[!yarn$train,])
}

data(oliveoil)
mod.sens <- plsr(sensory ~ chemical, ncomp = 4, data = oliveoil)
\dontrun{plot(mod.sens, ncomp = 2:4) # Several responses gives several plots}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
