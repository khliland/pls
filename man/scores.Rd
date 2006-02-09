%% $Id$
\encoding{latin1}
\name{scores}
\alias{scores}
\alias{scores.default}
\alias{loading.weights}
\alias{Yscores}
\alias{Yloadings}
\title{Extract Scores and Loadings from PLSR and PCR Models}
\description{
  These functions extract score and loading matrices from fitted
  \code{mvr} models.
}
\usage{
scores(object, \dots)
\method{scores}{default}(object, \dots)

loading.weights(object)

Yscores(object)

Yloadings(object)
}
\arguments{
  \item{object}{a fitted model to extract from.}
  \item{\dots}{extra arguments, currently not used.}
}
\details{
  All functions extract the indicated matrix from the fitted model, and
  will work with any object having a suitably named component.

  The default \code{scores} method also handles \code{prcomp} objects (their
  score component is called \code{x}), and adds an attribute
  \code{"explvar"} with the variance explained by each component, if
  this is available.  (See \code{\link{explvar}} for details.)

  \code{\link[stats]{loadings}} is in package \pkg{stats}, and is
  documented there.
}
\value{
  A matrix with scores or loadings.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{coef.mvr}},
  \code{\link[stats]{loadings}}}
\examples{
data(NIR)
plsmod <- plsr(y ~ X, 6, data = NIR)
scores(plsmod)
loadings(plsmod)[,1:4]
}
\keyword{regression}
\keyword{multivariate}
