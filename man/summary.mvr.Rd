%% $Id$
\encoding{latin1}
\name{summary.mvr}
\alias{summary.mvr}
\alias{print.mvr}
\alias{print.mvrVal}
\title{Summary and Print Methods for PLSR and PCR objects}
\description{
  Summary and print methods for \code{mvr} and \code{mvrVal} objects.
}
\usage{
\method{summary}{mvr}(object, what = c("all", "validation", "training"),
        digits = 4, print.gap = 2, \dots)
\method{print}{mvr}(x, \dots)
\method{print}{mvrVal}(x, digits = 4, print.gap = 2, \dots)
}
\arguments{
  \item{x, object}{an \code{mvr} object}
  \item{what}{one of \samp{"all"}, \samp{"validation"} or \samp{"training"}}
  \item{digits}{integer.  Minimum number of significant digits in the
    output.  Default is 4.}
  \item{print.gap}{Integer.  Gap between coloumns of the printed tables.}
  \item{\dots}{Other arguments sent to underlying methods.}
}
\details{
  If \code{what} equals \samp{"training"}, the explained variances are
  given; if it equals "validation", the cross-validated RMSEPs (if
  available) are given; if it equals \samp{"all"}, both are given.
}
\value{
  \code{print.mvr} and \code{print.mvrVal} return the object invisibly.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{pcr}},
  \code{\link{plsr}}, \code{\link{RMSEP}}, \code{\link{MSEP}}}
\examples{
data(NIR)
nir.mvr <- mvr(y ~ X, ncomp = 8, validation = "LOO", data = NIR)
nir.mvr
summary(nir.mvr)
RMSEP(nir.mvr)
}
\keyword{regression}
\keyword{multivariate}
