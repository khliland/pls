%% $Id$
\encoding{latin1}
\name{widekernelpls.fit}
\alias{widekernelpls.fit}
\title{Wide Kernel PLS (Rännar et al.)}
\description{Fits a PLSR model with the wide kernel algorithm.}
\usage{widekernelpls.fit(X, Y, ncomp, stripped = FALSE,
                  tol = .Machine$double.eps^0.5, maxit = 100, \dots)}
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
  \item{tol}{numeric.  The tolerance used for determining convergence in
    the algorithm.}
  \item{maxit}{positive integer.  The maximal number of iterations used
    in the internal Eigenvector calculation.}
  \item{\dots}{other arguments.  Currently ignored.}
}
\details{This function should not be called directly, but through
  the generic functions \code{plsr} or \code{mvr} with the argument
  \code{method="widekernelpls"}.  The wide kernel PLS algorithm is
  particularly efficient when the number of variables is (much) larger
  than the number of observations.  It is especially fast compared to
  the default kernel algorithm when the number of responses is also
  larger than the number of observations.  The results are equal to the
  results of the NIPALS algorithm.
}
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
  Rännar, S., Lindgren, F., Geladi, P. and Wold, S. (1994) A PLS
  Kernel Algorithm for Data Sets with Many Variables and Fewer
  Objects.  Part 1: Theory and Algorithm.
  \emph{Journal of Chemometrics}, \bold{8}, 111--125.
}
\author{Bjørn-Helge Mevik}
\seealso{
  \code{\link{mvr}}
  \code{\link{plsr}}
  \code{\link{pcr}}
  \code{\link{kernelpls.fit}}
  \code{\link{simpls.fit}}
  \code{\link{oscorespls.fit}}
}
\keyword{regression}
\keyword{multivariate}
