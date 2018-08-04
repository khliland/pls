\encoding{UTF-8}
\name{kernelpls.fit}
\alias{kernelpls.fit}
\title{Kernel PLS (Dayal and MacGregor)}
\description{Fits a PLSR model with the kernel algorithm.}
\usage{kernelpls.fit(X, Y, ncomp, center = TRUE, stripped = FALSE, \dots)}
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
  the generic functions \code{plsr} or \code{mvr} with the argument
  \code{method="kernelpls"} (default).  Kernel PLS is particularly efficient 
  when the number of objects is (much) larger than the number of
  variables.  The results are equal to the NIPALS algorithm.  Several
  different forms of kernel PLS have been described in literature, e.g.
  by De Jong and Ter Braak, and two algorithms by Dayal and
  MacGregor.  This function implements the
  fastest of the latter, not calculating the crossproduct matrix of
  X.  In the Dyal & MacGregor paper, this is \dQuote{algorithm 1}.
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
    component.}
  \item{Xtotvar}{Total variance in \code{X}.}

  If \code{stripped} is \code{TRUE}, only the components
  \code{coefficients}, \code{Xmeans} and \code{Ymeans} are returned.
}
\references{
  de Jong, S. and ter Braak,  C. J. F. (1994) Comments on the PLS kernel
  algorithm.  \emph{Journal of Chemometrics}, \bold{8}, 169--174.

  Dayal, B. S. and MacGregor, J. F. (1997) Improved PLS algorithms.
  \emph{Journal of Chemometrics}, \bold{11}, 73--85.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{
  \code{\link{mvr}}
  \code{\link{plsr}}
  \code{\link{cppls}}
  \code{\link{pcr}}
  \code{\link{widekernelpls.fit}}
  \code{\link{simpls.fit}}
  \code{\link{oscorespls.fit}}
}
\keyword{regression}
\keyword{multivariate}
