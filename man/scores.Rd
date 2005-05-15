%% $Id$
\encoding{latin1}
\name{scores}
\alias{scores}
\alias{scores.prcomp}
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
\method{scores}{prcomp}(object, \dots)
\method{scores}{default}(object, \dots)
loading.weights(object)
Yscores(object)
Yloadings(object)
}
\arguments{
  \item{object}{a fitted \code{mvr} model to extract from.}
  \item{\dots}{extra arguments, currently not used.}
}
\details{
  All functions extract the indicated matrix from the fitted model, and
  will work with any object having a suitably named component.
  \code{scores} also has a method for \code{prcomp} objects (their
  score component is called \code{x}).
  \code{loadings} is in package \code{stats}, and is documented there.
}
\value{
  A matrix with scores or loadings.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{coef.mvr}}}
\examples{
data(NIR)
plsmod <- plsr(y ~ X, 6, data = NIR)
scores(plsmod)
loadings(plsmod)[,1:4]
}
\keyword{regression}
\keyword{multivariate}
