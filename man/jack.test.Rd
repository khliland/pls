\name{jack.test}
\alias{jack.test}
\alias{print.jacktest}
\title{Jackknife approximate t tests of regression coefficients}
\description{
  Performes approximate t tests of regression coefficients
  based on jackknife variance estimates.
}
\usage{
jack.test(object, ncomp = object$ncomp, use.mean = TRUE)
\method{print}{jacktest}(x, P.values = TRUE, \dots)
}
\arguments{
  \item{object}{an \code{mvr} object.  A cross-validated model fitted
    with \code{jackknife = TRUE}.}
  \item{ncomp}{the number of components to use for estimating the variances}
  \item{use.mean}{logical.  If \code{TRUE} (default), the mean
    coefficients are used when estimating the (co)variances; otherwise
    the coefficients from a model fitted to the entire data set.  See
    \code{\link{var.jack}} for details.}
  \item{x}{an \code{jacktest} object, the result of \code{jack.test}.}
  \item{P.values}{logical.  Whether to print \eqn{p} values (default).}
  \item{\dots}{Further arguments sent to the underlying print function
    \code{\link{printCoefmat}}.}
}
\details{
  \code{jack.test} uses the variance estimates from \code{var.jack} to
  perform \eqn{t} tests of the regression coefficients.  The resulting object
  has a print method, \code{print.jacktest}, which uses
  \code{\link{printCoefmat}} for the actual printing.
}
\value{
  \code{jack.test} returns an object of class \code{"jacktest"}, with components
  \item{coefficients }{The estimated regression coefficients}
  \item{sd}{The square root of the jackknife variance estimates}
  \item{tvalues}{The \eqn{t} statistics}
  \item{df}{The `degrees of freedom' used for calculating \eqn{p}
    values}
  \item{pvalues}{The calculated \eqn{p} values}

  \code{print.jacktest} returns the \code{"jacktest"} object (invisibly).
}
\section{Warning}{
  The jackknife variance estimates are known to be biased (see
  \code{\link{var.jack}}).
  Also, the distribution of the regression coefficient estimates and the
  jackknife variance estimates are unknown (at least in PLSR/PCR).
  Consequently, the distribution (and in particular, the degrees of
  freedom) of the resulting \eqn{t} statistics is unknown.  The present code
  simply assumes a \eqn{t} distribution with \eqn{m - 1} degrees of
  freedom, where \eqn{m} is the number of cross-validation segments.

  Therefore, the resulting \eqn{p} values should not be used
  uncritically, and should perhaps be regarded as mere indicator of
  (non-)significance.

  Finally, also keep in mind that as the number of predictor variables
  increase, the problem of multiple tests increases correspondingly.
}
\references{
  Martens H. and Martens M. (2000) Modified Jack-knife Estimation of
  Parameter Uncertainty in Bilinear Modelling by Partial Least Squares
  Regression (PLSR).  \emph{Food Quality and Preference}, \bold{11}, 5--16.
}
\author{Bjørn-Helge Mevik}
\seealso{\code{\link{var.jack}}, \code{\link{mvrCv}}}
\examples{
data(oliveoil)
mod <- pcr(sensory ~ chemical, data = oliveoil, validation = "LOO", jackknife = TRUE)
jack.test(mod, ncomp = 2)
}
\keyword{htest}
