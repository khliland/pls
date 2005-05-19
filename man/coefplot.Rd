%% $Id$
\encoding{latin1}
\name{coefplot}
\alias{coefplot}
\title{Plot Regression Coefficients of PLSR and PCR models}
\description{
  Function to plot the regression coefficients of an \code{mvr} object.
}
\usage{
coefplot(object, ncomp = object$ncomp, separate = FALSE, cumulative = TRUE,
         intercept = FALSE, nCols, nRows, type = "l", lty = 1:nLines, lwd = 1,
         pch = 1:nLines, cex = 1, col = 1:nLines, legendpos,
         xlab = "variable", ylab = "regression coefficient", \dots)
}
\arguments{
  \item{object}{an \code{mvr} object.  The fitted model.}
  \item{ncomp}{integer vector.  The number of components to include.  If
    \code{length(ncomp) > 1}, coefficients for each model size is plotted.}
  \item{separate}{logical.  If \code{TRUE}, coefficients for different model
    sizes are blotted in separate plots.}
  \item{cumulative}{logical.  Whether cumulative (the default) or
    individual coefficients for each component should be plotted.  See
    \code{\link{coef.mvr}} for details.}
  \item{intercept}{logical.  Whether coefficients for the intercept should
    be plotted.  Ignored if \code{cumulative = FALSE}.  Defaults to
    \code{FALSE}.  See \code{\link{coef.mvr}} for details.}
  \item{nCols, nRows}{integer.  The number of coloumns and rows the
    plots will be laid out in.  If not specified, \code{coefplot} tries
    to be intelligent.}
  \item{type}{character.  What type of plot to make.  Defaults to
    \code{"l"} (lines).  Alternative types include \code{"p"} (points)
    and \code{"b"} (both).  See \code{\link{plot}} for a complete list
    of types.}
  \item{lty}{vector of line types (recycled as neccessary).  Line types can be
    specified as integers or character strings (see \code{\link{par}}
    for the details).}
  \item{lwd}{vector of positive numbers (recycled as neccessary), giving
    the width of the lines.}
  \item{pch}{plot character.  A character string or a vector of
    single characters or integers (recycled as neccessary).  See
    \code{\link{points}} for all alternatives.}
  \item{cex}{numeric vector of character expansion sizes (recycled as
    neccessary) for the plotted symbols.}
  \item{col}{character or integer vector of colors for plotted lines and
    symbols (recycled as neccessary).  See \code{\link{par}} for the details.}
  \item{legendpos}{Legend position.  Optional.  Ignored if \code{separate} is
    \code{TRUE}.  If present, a legend is drawn at the given position.
    The position can be specified symbolically (e.g., \code{legendpos =
      "topright"}).  This requires \R >= 2.1.0.  Alternatively, the
    position can be specified explicitly (\code{legendpos = t(c(x,y))})
    or interactively (\code{legendpos = \link{locator}()}).  This only
    works well for plots of single-response models.}
  \item{xlab,ylab}{titles for \eqn{x} and \eqn{y} axes.  Typically
    character strings, but can be expressions (e.g.,
    \code{expression(R^2)} or lists.  See \code{\link{title}} for details.}
  \item{\dots}{Further arguments sent to the underlying plot functions.}
}
\details{
  \code{coefplot} handles multiple responses by making one plot for each
  response.  If \code{separate} is \code{TRUE}, separate plots are made
  for each combination of model size and response.  The plots are laid
  out in a rectangular fashion.

  If \code{legendpos} is given, a legend is drawn at the given position
  (unless \code{separate} is \code{TRUE}).

  The function can also be called through the \code{mvr} plot method by
  specifying \code{plottype = "coefficients"}.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\note{\code{\link{legend}} has many options.  If you want greater
  control over the appearance of the legend, omit the \code{legendpos}
  argument and call \code{legend} manually.
}
\seealso{\code{\link{mvr}}, \code{\link{plot.mvr}}, \code{\link{coef.mvr}},
  \code{\link{plot}}, \code{\link{legend}}}
\examples{
data(NIR)
mod.nir <- plsr(y ~ X, ncomp = 8, data = NIR)
\dontrun{
coefplot(mod.nir, ncomp = 1:6)
plot(mod.nir, plottype = "coefficients", ncomp = 1:6) # Equivalent to the previous
## Plot with legend:
coefplot(mod.nir, ncom = 1:6, legendpos = "bottomright")
}

data(sensory)
mod.sens <- plsr(Quality ~ Panel, ncomp = 4, data = sensory)
\dontrun{coefplot(mod.sens, ncomp = 2:4, separate = TRUE)}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
