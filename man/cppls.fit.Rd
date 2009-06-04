%% $Id$
\encoding{latin1}
\name{cppls.fit}
\alias{cppls}
\title{CPPLS (Indahl et al.)}
\description{Fits a PLS model using the CPPLS algorithm.}
\usage{cppls.fit(X, Y, ncomp, Y2 = NULL, stripped = FALSE, lower = 0.5, upper = 0.5, wt = NULL, \dots)}
\arguments{
  \item{X}{a matrix of observations.  \code{NA}s and \code{Inf}s are not
    allowed.} 
  \item{Y}{a vector or matrix of responses.  \code{NA}s and \code{Inf}s
    are not allowed.} 
  \item{ncomp}{the number of components to be used in the
    modelling.}
  \item{Y2}{a vector or matrix of additional responses containing
    relevant information about the observations.}
  \item{stripped}{logical.  If \code{TRUE} the calculations are stripped
    as much as possible for speed; this is meant for use with
    cross-validation or simulations when only the coefficients are
    needed.  Defaults to \code{FALSE}.}
  \item{lower}{a vector of lower limits for power optimisation. Defaults to \code{0.5}.}
  \item{upper}{a vector of upper limits for power optimisation. Defaults to \code{0.5}.}
  \item{wt}{a vector of individual weights for the observations. (Optional)}
  \item{\dots}{other arguments.  Currently ignored.}
}
\details{This function should not be called directly, but through
  the generic functions \code{cppls} or \code{mvr} with the argument
  \code{method="cppls"}. Canonical Powered PLS (CPPLS)
  is a generalisation of PLS incorporating discrete and continuous
  responses (also simultaneously), additional responses, individual
  weighting of observations and power methodology for sharpening
  focus on groups of variables. Depending on the input to \code{cppls} it
  can produce the following special cases:
  \itemize{
  \item{PLS}{uni-response continuous \code{Y}}
  \item{PPLS}{uni-response continuous \code{Y}, \code{(lower || upper) != 0.5}}
  \item{PLS-DA (using correlation maximisation - B/W)}{dummy-coded descrete response \code{Y}}
  \item{PPLS-DA}{dummy-coded descrete response \code{Y}, \code{(lower || upper) != 0.5}}
  \item{CPLS}{multi-response \code{Y} (continuous, discrete or combination)}
  \item{CPPLS}{multi-response \code{Y} (continuous, discrete or combination), \code{(lower || upper) != 0.5}}
  }
  The name "canonical" comes from canonical correlation analysis which
  is used when calculating vectors of loading weights, while "powered"
  refers to a reparameterisation of the vectors of loading weights
  which can be optimised over a given interval.
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
  \item{gammas}{gamma-values obtained in power optimisation.}
  \item{canonical.correlations}{Canonical correlation values from the
    calculations of loading weights.}
  \item{smallNorms}{Vector of indices of explanatory variables
    of length close to or equal to 0.}

  If \code{stripped} is \code{TRUE}, only the components
  \code{coefficients}, \code{Xmeans}, \code{Ymeans} and \code{gammas} are returned.
}
\references{
  Indahl, U. (2005) A twist to partial least squares regression.
  \emph{Journal of Chemometrics}, \bold{19}, 32--44.
  
  Indahl, U.G., Liland, K.H. and Næs, T. (2009) Canonical partial least
  squares - a unified PLS approach to classification and regression problems.
  \emph{Journal of Chemometrics}, In press.
}
\author{Kristian Hovde Liland}
\seealso{
  \code{\link{mvr}}
  \code{\link{plsr}}
  \code{\link{pcr}}
  \code{\link{widekernelpls.fit}}
  \code{\link{simpls.fit}}
  \code{\link{oscorespls.fit}}
}
\keyword{regression}
\keyword{classification}
\keyword{multivariate}
