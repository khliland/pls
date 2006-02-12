%% $Id$
\encoding{latin1}
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
         nRows, xlab = "measured", ylab = "predicted",
         font.main = 1, cex.main = 1.1, \dots)
predplotXy(x, y, line = FALSE, main = "Prediction plot",
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
  \item{font.main}{font to use for main title.  See \code{\link{par}}
    for details.}
  \item{cex.main}{numeric.  The magnification to be used for main titles
    relative to the current size.}
  \item{x}{numeric vector.  The observed response values.}
  \item{y}{numeric vector.  The predicted response values.}
  \item{line}{logical.  Whether a target line should be drawn.}
  \item{main}{character.  Main title of plot.}
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

  \code{predplotXy} is an internal function and is not meant for
  interactive use.  It is called by the \code{predplot} methods, and its
  arguments, e.g, \code{line}, can be given in the \code{predplot} call.
}
\value{
  The functions invisibly returns a matrix with the (last) plotted data.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{plot.mvr}}}
\examples{
data(NIR)
mod <- plsr(y ~ X, ncomp = 10, data = NIR[NIR$train,], validation = "CV")
\dontrun{
predplot(mod, ncomp = 1:6)
plot(mod, ncomp = 1:6) # Equivalent to the previous
## Both cross-validated and test set predictions:
predplot(mod, ncomp = 4:6, which = c("validation", "test"),
         newdata = NIR[!NIR$train,])
}

data(sensory)
mod.sens <- plsr(Quality ~ Panel, ncomp = 4, data = sensory)
\dontrun{plot(mod.sens, ncomp = 2:4) # Several responses gives several plots}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
