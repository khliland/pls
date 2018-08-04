\encoding{UTF-8}
\name{svdpc.fit}
\alias{svdpc.fit}
\title{Principal Component Regression}
\description{Fits a PCR model using the singular value decomposition.}
\usage{svdpc.fit(X, Y, ncomp, center = TRUE, stripped = FALSE, \dots)}
\arguments{
  \item{X}{a matrix of observations.  \code{NA}s and \code{Inf}s are not
    allowed.} 
  \item{Y}{a vector or matrix of responses.  \code{NA}s and \code{Inf}s
    are not allowed.} 
  \item{ncomp}{the number of components to be used in the
    modelling.}
  \item{center}{logical, determines if the \eqn{X} and \eqn{Y} matrices
    are mean centered or not. Default is to perform mean centering.}
  \item{stripped}{logical.  If \code{TRUE} the calculations are stripped
    as much as possible for speed; this is meant for use with
    cross-validation or simulations when only the coefficients are
    needed.  Defaults to \code{FALSE}.}
  \item{\dots}{other arguments.  Currently ignored.}
}
\details{This function should not be called directly, but through
  the generic functions \code{pcr} or \code{mvr} with the argument
  \code{method="svdpc"}.  The singular value decomposition is
  used to calculate the principal components.
}
\value{A list containing the following components is returned:
  \item{coefficients}{an array of regression coefficients for 1, \ldots,
    \code{ncomp} components.  The dimensions of \code{coefficients} are
    \code{c(nvar, npred, ncomp)} with \code{nvar} the number
    of \code{X} variables and \code{npred} the number of variables to be
    predicted in \code{Y}.}
  \item{scores}{a matrix of scores.}
  \item{loadings}{a matrix of loadings.}
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
    component.}
  \item{Xtotvar}{Total variance in \code{X}.}

  If \code{stripped} is \code{TRUE}, only the components
  \code{coefficients}, \code{Xmeans} and \code{Ymeans} are returned.
}
\references{
  Martens, H., Næs, T. (1989) \emph{Multivariate calibration.}
  Chichester: Wiley.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{
  \code{\link{mvr}}
  \code{\link{plsr}}
  \code{\link{pcr}}
  \code{\link{cppls}}
}
\keyword{regression}
\keyword{multivariate}
