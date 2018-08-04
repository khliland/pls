\encoding{UTF-8}
\name{oscorespls.fit}
\alias{oscorespls.fit}
\title{Orthogonal scores PLSR}
\description{Fits a PLSR model with the orthogonal scores algorithm
(aka the NIPALS algorithm).}
\usage{oscorespls.fit(X, Y, ncomp, center = TRUE, stripped = FALSE,
               tol = .Machine$double.eps^0.5, maxit = 100, \dots)}
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
  \item{tol}{numeric.  The tolerance used for determining convergence in
    multi-response models.}
  \item{maxit}{positive integer.  The maximal number of iterations used
    in the internal Eigenvector calculation.}
  \item{\dots}{other arguments.  Currently ignored.}
}
\details{This function should not be called directly, but through
  the generic functions \code{plsr} or \code{mvr} with the argument
  \code{method="oscorespls"}.  It implements the orthogonal scores
  algorithm, as described in \cite{Martens and Næs (1989)}.  This is one
  of the two \dQuote{classical}
  PLSR algorithms, the other being the orthogonal loadings algorithm.}
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
  \code{\link{kernelpls.fit}}
  \code{\link{widekernelpls.fit}}
  \code{\link{simpls.fit}}
}
\keyword{regression}
\keyword{multivariate}
