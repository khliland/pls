%% $Id$
\encoding{UTF-8}
\name{mvrCv}
\alias{mvrCv}
\title{Cross-validation}
\description{
  Performs the cross-validation calculations for \code{mvr}.
}
\usage{
mvrCv(X, Y, ncomp, Y.add = NULL, weights = NULL,
      method = pls.options()$mvralg, scale = FALSE,
      segments = 10, segment.type = c("random", "consecutive", "interleaved"),
      length.seg, jackknife = FALSE, trace = FALSE, \dots)
}
\arguments{
  \item{X}{a matrix of observations.  \code{NA}s and \code{Inf}s are not
    allowed.}
  \item{Y}{a vector or matrix of responses.  \code{NA}s and \code{Inf}s
    are not allowed.}
  \item{ncomp}{the number of components to be used in the
    modelling.}
  \item{Y.add}{a vector or matrix of additional responses containing
    relevant information about the observations.  Only used for \code{cppls}.}
  \item{weights}{a vector of individual weights for the observations.
    Only used for \code{cppls}.  (Optional)}
  \item{method}{the multivariate regression method to be used.}
  \item{scale}{logical.  If \code{TRUE}, the learning \eqn{X} data for each
    segment is scaled by dividing each variable by its sample standard
    deviation.  The prediction data is scaled by the same amount.}
  \item{segments}{the number of segments to use, or a list with segments
    (see below).}
  \item{segment.type}{the type of segments to use.  Ignored if
    \code{segments} is a list.}
  \item{length.seg}{Positive integer.  The length of the segments to
    use.  If specified, it overrides \code{segments} unless
    \code{segments} is a list.}
  \item{jackknife}{logical.  Whether jackknifing of regression
    coefficients should be performed.}
  \item{trace}{logical; if \code{TRUE}, the segment number is printed
    for each segment.}
  \item{\dots}{additional arguments, sent to the underlying fit function.}
}
\details{
  This function is not meant to be called directly, but through
  the generic functions \code{pcr}, \code{plsr}, \code{cppls}
  or \code{mvr} with the argument \code{validation} set to \code{"CV"} or
  \code{"LOO"}.  All arguments to \code{mvrCv} can be specified in the
  generic function call.

  If \code{segments} is a list, the arguments \code{segment.type} and
  \code{length.seg} are ignored.  The elements of the list should be
  integer vectors specifying the indices of the segments.  See
  \code{\link{cvsegments}} for details.

  Otherwise, segments of type \code{segment.type} are generated.  How
  many segments to generate is selected by specifying the number of
  segments in \code{segments}, or giving the segment length in
  \code{length.seg}.  If both are specified, \code{segments} is
  ignored.

  If \code{jackknife} is \code{TRUE}, jackknifed regression coefficients
  are returned, which can be used for for variance estimation
  (\code{\link{var.jack}}) or hypothesis testing (\code{\link{jack.test}}).

  \code{X} and \code{Y} do not need to be centered.
  
  Note that this function cannot be used in situations where \eqn{X}
  needs to be recalculated for each segment (except for scaling by the
  standard deviation), for instance with
  \code{msc} or other preprocessing.  For such models, use the more
  general (but slower) function \code{\link{crossval}}.

  Also note that if needed, the function will silently(!) reduce
  \code{ncomp} to the maximal number of components that can be
  cross-validated, which is \eqn{n - l -  1}, where \eqn{n} is the
  number of observations and \eqn{l} is the length of the longest
  segment.  The (possibly reduced) number of components is returned as
  the component \code{ncomp}.

  By default, the cross-validation will be performed serially.  However,
  it can be done in parallel using functionality in the
  \code{\link{parallel}} package by setting the option \code{parallel} in
  \code{\link{pls.options}}.  See \code{\link{pls.options}} for the
  different ways to specify the parallelism.
}
\value{
  A list with the following components:
  \item{method}{equals \code{"CV"} for cross-validation.}
  \item{pred}{an array with the cross-validated predictions.}
  \item{coefficients}{(only if \code{jackknife} is \code{TRUE}) an array
    with the jackknifed regression coefficients.  The dimensions
    correspond to the predictors, responses, number of components, and
    segments, respectively.}
  \item{PRESS0}{a vector of PRESS values (one for each response variable)
    for a model with zero components, i.e., only the intercept.}
  \item{PRESS}{a matrix of PRESS values for models with 1, \ldots,
    \code{ncomp} components.  Each row corresponds to one response variable.}
  \item{adj}{a matrix of adjustment values for calculating bias
    corrected MSEP.  \code{MSEP} uses this.}
  \item{segments}{the list of segments used in the cross-validation.}
  \item{ncomp}{the actual number of components used.}
  \item{gamma}{if method \code{cppls} is used, gamma values for the
    powers of each CV segment are returned.}
}
\references{
  Mevik, B.-H., Cederkvist, H. R. (2004) Mean Squared Error of
  Prediction (MSEP) Estimates for Principal Component Regression (PCR)
  and Partial Least Squares Regression (PLSR).
  \emph{Journal of Chemometrics}, \bold{18}(9), 422--429.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\note{
  The \code{PRESS0} is always cross-validated using leave-one-out
  cross-validation.  This usually makes little difference in practice,
  but should be fixed for correctness.

  The current implementation of the jackknife stores all
  jackknife-replicates of the regression coefficients, which can be very
  costly for large matrices.  This might change in a future version.
}
\seealso{
  \code{\link{mvr}}
  \code{\link{crossval}}
  \code{\link{cvsegments}}
  \code{\link{MSEP}}
  \code{\link{var.jack}}
  \code{\link{jack.test}}
}
\examples{
data(yarn)
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV", segments = 10)
\dontrun{plot(MSEP(yarn.pcr))}
}
\keyword{regression}
\keyword{multivariate}
