%% $Id$
\encoding{UTF-8}
\name{plot.mvr}
\alias{plot.mvr}
\title{Plot Method for MVR objects}
\description{
  \code{plot.mvr} plots predictions, coefficients, scores, loadings,
  biplots, correlation loadings or validation plots (RMSEP curves, etc.).
}
\usage{
\method{plot}{mvr}(x, plottype = c("prediction", "validation", "coefficients",
                     "scores", "loadings", "biplot", "correlation"), \dots)
}
\arguments{
  \item{x}{an object of class \code{mvr}.  The fitted model to plot.}
  \item{plottype}{character.  What kind of plot to plot.}
  \item{\dots}{further arguments, sent to the underlying plot functions.}
}
\details{
  The function is simply a wrapper for the underlying plot functions
  used to make the selected plots.  See \code{\link{predplot.mvr}},
  \code{\link{validationplot}}, \code{\link{coefplot}},
  \code{\link{scoreplot}}, \code{\link{loadingplot}},
  \code{\link{biplot.mvr}} or \code{\link{corrplot}} for details.  Note
  that all arguments except \code{x} and \code{plottype} must be named.
}
\value{
  \code{plot.mvr} returns whatever the underlying plot function returns.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{predplot.mvr}},
  \code{\link{validationplot}}, \code{\link{coefplot}},
  \code{\link{scoreplot}}, \code{\link{loadingplot}},
  \code{\link{biplot.mvr}}, \code{\link{corrplot}}}
\examples{
data(yarn)
nir.pcr <- pcr(density ~ NIR, ncomp = 9, data = yarn, validation = "CV")
\dontrun{
plot(nir.pcr, ncomp = 5) # Plot of cross-validated predictions
plot(nir.pcr, "scores") # Score plot
plot(nir.pcr, "loadings", comps = 1:3) # The three first loadings
plot(nir.pcr, "coef", ncomp = 5) # Coefficients
plot(nir.pcr, "val") # RMSEP curves
plot(nir.pcr, "val", val.type = "MSEP", estimate = "CV") # CV MSEP
}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
