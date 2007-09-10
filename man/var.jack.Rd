%% $Id$
\encoding{latin1}
\name{var.jack}
\alias{var.jack}
\title{Jackknife Variance Estimates of Regression Coefficients}
\description{
  Calculates jackknife variance or covariance estimates of regression
  coefficients.
}
\usage{
var.jack(object, ncomp = object$ncomp, covariance = FALSE, use.mean = TRUE)
}
\arguments{
  \item{object}{an \code{mvr} object.  A cross-validated model fitted
    with \code{jackknife = TRUE}.}
  \item{ncomp}{the number of components to use for estimating the (co)variances}
  \item{covariance}{logical.  If \code{TRUE}, covariances are
    calculated; otherwise only variances.  The default is \code{FALSE}.}
  \item{use.mean}{logical.  If \code{TRUE} (default), the mean
    coefficients are used when estimating the (co)variances; otherwise
    the coefficients from a model fitted to the entire data set.  See Details.}
}
\details{
  The original (Tukey) jackknife variance estimator is defined as
  \eqn{1/(g-1)\sum_{i=1}^g(\tilde\beta_{-i} - \bar\beta)^2}, where
  \eqn{g} is the number of segments, \eqn{\tilde\beta_{-i}} is the
  estimated coefficient when segment \eqn{i} is left out (called the
  jackknife replicates), and
  \eqn{\bar\beta} is the mean of the \eqn{\tilde\beta_{-i}}.  The most
  common case is delete-one jackknife, with \eqn{g = n}, the number of
  observations.  

  This is the definition \code{var.jack} uses by default.

  However, Martens FIXME defined the estimator as
  \eqn{1/(g-1)\sum_{i=1}^g(\tilde\beta_{-i} - \hat\beta)^2}, where
  \eqn{\hat\beta} is the coefficient estimate using the entire data set.
  I.e., they use the original fitted coefficients instead of the
  mean of the jackknife replicates.  Most (all?) jackknife
  implementations for PLSR use this estimator.  \code{var.jack} can be
  made to use this definition with \code{use.mean = FALSE}.  In
  practice, the difference should not be large if the number of
  observations is sufficiently large.  Note, however, that all
  theoretical results about the jackknife refer to the `proper'
  definition.
}
\value{
  If \code{covariance} is \code{FALSE}, an \eqn{p\times q \times c}
  array of variance estimates, where \eqn{p} is the number of
  predictors, \eqn{q} is the number of responses, and \eqn{c} is the
  number of components.

  If \code{covariance} id \code{TRUE}, an \eqn{pq\times pq \times c}
  array of variance-covariance estimates.
}
\references{ FIXME: Tukey, Martens, Hinkley, Wu etal }
\author{Bjørn-Helge Mevik}
\section{Warning}{
  Note that the Tukey jackknife variance estimator is not unbiased.
  The bias depends on the \eqn{X} matrix.  For ordinary least squares
  regression (OLSR), the (FIXME: expected?) bias can be calculated, and depends
  on the number of observations \eqn{n} and the number of parameters
  \eqn{k} in the mode.  For the common case of an orthogonal design
  matrix with \eqn{\pm 1}{±1} levels, the delete-one jackknife estimate
  equals \eqn{(n-1)/(n-k)} times the classical variance estimate for the
  regression coefficients in OLSR.  Similar expressions hold for
  delete-d estimates.  Modifications have been proposed to reduce or
  eliminate the bias (for the OLSR case), however, they depend on the
  number of parameters used in the model FIXME:ref.

  Thus, the results of \code{var.jack} should be used with caution.
}
\seealso{\code{\link{mvrCv}}, \code{\link{jack.test}}}
\examples{
data(oliveoil)
mod <- pcr(sensory ~ chemical, data = oliveoil, validation = "LOO", jackknife = TRUE)
var.jack(mod, ncomp = 2)
}
\keyword{FIXME}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
