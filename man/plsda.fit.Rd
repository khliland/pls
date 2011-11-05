%% $Id$
\encoding{latin1}
\name{plsda.fit}
\alias{plsda.fit}
\title{PLS discriminant analysis}
\description{Fits a PLS-DA model with maximisation of the between
groups sum of squares and cross products matrix.}
\usage{plsda.fit(X, Y, ncomp, stripped = FALSE,
               weights = NULL, \dots)}
\arguments{
  \item{X}{a matrix of observations.  \code{NA}s and \code{Inf}s are not
    allowed.} 
  \item{Y}{a vector or matrix of responses.  \code{NA}s and \code{Inf}s
    are not allowed.} 
  \item{ncomp}{the number of components to be used in the
    modelling.}
  \item{stripped}{logical.  If \code{TRUE} the calculations are stripped
    as much as possible for speed; this is meant for use with
    cross-validation or simulations when only the coefficients are
    needed.  Defaults to \code{FALSE}.}
  \item{weights}{a vector of individual weights for the observations. (Optional)}
  \item{\dots}{other arguments.  Currently ignored.}
}
\details{This function should not be called directly, but through
  the generic functions \code{plsda} or \code{mvr} with the argument
  \code{method="plsda"}.  It implements the ordinary PLS-DA algorithm
  with possible observation weights described in \cite{Indahl, Martens and Næs (2007)}.}
\value{A list containing the following components is returned:
  \item{coefficients}{an array of regression coefficients for 1, \ldots,
    \code{ncomp} components.  The dimensions of \code{coefficients} are
    \code{c(nvar, npred, ncomp)} with \code{nvar} the number
    of \code{X} variables and \code{npred} the number of variables to be
    predicted in \code{Y}.}
  \item{scores}{a matrix of scores.}
  \item{loadings}{a matrix of loadings.}
  \item{loading.weights}{a matrix of loading weights.}
  \item{Yscores}{a matrix of Y-scores.}
  \item{Yloadings}{a matrix of Y-loadings.}
  \item{projection}{the projection matrix used to convert X to scores.}
  \item{Xmeans}{a vector of means of the X variables.}
  \item{Ymeans}{a vector of means of the Y variables.}
  \item{fitted.values}{an array of fitted values.  The dimensions of
    \code{fitted.values} are \code{c(nobj, npred, ncomp)} with
    \code{nobj} the number samples and \code{npred} the number of
    Y variables.}
  \item{residuals}{an array of regression residuals.  It has the same
    dimensions as \code{fitted.values}.}
  \item{Xvar}{a vector with the amount of X-variance explained by each
    number of components.}
  \item{Xtotvar}{Total variance in \code{X}.}

  If \code{stripped} is \code{TRUE}, only the components
  \code{coefficients}, \code{Xmeans} and \code{Ymeans} are returned.
}
\references{
  Indahl, U.G., Martens, H., Næs, T. 2007. From dummy regression to 
  prior probabilities in PLS-DA. Journal of Chemometrics, Vol 21, 12, pp 529-536.
}
\author{Kristian Hovde Liland and Bjørn-Helge Mevik}
\seealso{
  \code{\link{mvr}}
  \code{\link{plsda}}
  \code{\link{cppls}}
  \code{\link{plsr}}
  \code{\link{pcr}}
  \code{\link{kernelpls.fit}}
  \code{\link{widekernelpls.fit}}
  \code{\link{simpls.fit}}
}
\keyword{regression}
\keyword{multivariate}
