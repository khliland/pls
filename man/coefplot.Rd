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
         intercept = FALSE, nCols, nRows, type = "l", \dots)
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
    \samp{FALSE}.  See \code{\link{coef.mvr}} for details.}
  \item{nCols, nRows}{integer.  The number of coloumns and rows the
    plots will be laid out in.  If not specified, \code{coefplot} tries
    to be intelligent.}
  \item{type}{character.  What type of plot to make.  Defaults to
    \samp{"l"} (lines).  See \code{\link{plot}} for details.}
  \item{\dots}{Further arguments sent to the underlying plot functions.}
}
\details{
  \code{coefplot} handles multiple responses by making one plot for each
  response.  If \code{separate} is \code{TRUE}, separate plots are made
  for each combination of model size and response.  The plots are laid
  out in a rectangular fashion.

  The function can also be called through the \code{mvr} plot method by
  specifying \code{plottype = "coefficients"}.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{plot.mvr}},
  \code{\link{coef.mvr}}, \code{\link{plot}}}
\examples{
data(NIR)
mod.nir <- plsr(y ~ X, ncomp = 8, data = NIR)
\dontrun{
coefplot(mod.nir, ncomp = 1:6)
plot(mod.nir, plottype = "coefficients", ncomp = 1:6) # Equivalent to the previous
}

data(sensory)
mod.sens <- plsr(Quality ~ Panel, ncomp = 4, data = sensory)
\dontrun{coefplot(mod.sens, ncomp = 2:4, separate = TRUE)}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
