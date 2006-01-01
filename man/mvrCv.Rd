%% $Id$
\encoding{latin1}
\name{mvrCv}
\alias{mvrCv}
\title{Cross-validation}
\description{
  Performs the cross-validation calculations for \code{mvr}.
}
\usage{
mvrCv(X, Y, ncomp,
      method = c("kernelpls", "simpls", "oscorespls", "svdpc"), scale = FALSE,
      segments = 10, segment.type = c("random", "consecutive", "interleaved"),
      length.seg, trace = FALSE, \dots)
}
\arguments{
  \item{X}{a matrix of observations.  \code{NA}s and \code{Inf}s are not
    allowed.}
  \item{Y}{a vector or matrix of responses.  \code{NA}s and \code{Inf}s
    are not allowed.}
  \item{ncomp}{the number of components to be used in the
    modelling.}
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
  \item{trace}{logical; if \code{TRUE}, the segment number is printed
    for each segment.}
  \item{\dots}{additional arguments, sent to the underlying fit function.}
}
\details{
  This function is not meant to be called directly, but through
  the generic functions \code{pcr}, \code{plsr} or \code{mvr} with the
  argument \code{validation} set to \code{"CV"} or \code{"LOO"}.  All
  arguments to \code{mvrCv} can be specified in the generic function call.

  If \code{segments} is a list, the arguments \code{segment.type} and
  \code{length.seg} are ignored.  The elements of the list should be
  integer vectors specifying the indices of the segments.  See
  \code{\link{cvsegments}} for details.

  Otherwise, segments of type \code{segment.type} are generated.  How
  many segments to generate is selected by specifying the number of
  segments in \code{segments}, or giving the segment length in
  \code{length.seg}.  If both are specified, \code{segments} is
  ignored.

  \code{X} and \code{Y} do not need to be centered.

  The R2 component returned is calculated as the squared correlation
  between the cross-validated predictions and the responses.
  
  Note that this function cannot be used in situations where \eqn{X}
  needs to be recalculated for each segment (except for scaling by the
  standard deviation), for instance with
  \code{msc} or other preprocessing.  For such models, use the more
  general (but slower) function \code{\link{crossval}}.
}
\value{
  A list with the following components:
  \item{method}{euqals \code{"CV"} for cross-validation.}
  \item{pred}{an array with the cross-validated predictions.}
  \item{MSEP0}{a vector of MSEP values (one for each response variable)
    for a model with zero components, i.e., only the intercept.}
  \item{MSEP}{a matrix of MSEP values for models with 1, \ldots,
    \code{ncomp} components.  Each row corresponds to one response variable.}
  \item{adj}{a matrix of adjustment values for calculating bias
    corrected MSEP.  \code{MSEP} uses this.}
  \item{R2}{a matrix of R2 values for models with 1, \ldots,
    \code{ncomp} components.  Each row corresponds to one response variable.}
  \item{segments}{the list of segments used in the cross-validation.}
}
\references{
  Mevik, B.-H., Cederkvist, H. R. (2004) Mean Squared Error of
  Prediction (MSEP) Estimates for Principal Component Regression (PCR)
  and Partial Least Squares Regression (PLSR).
  \emph{Journal of Chemometrics}, \bold{18}(9), 422--429.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\note{
  The \code{MSEP0} is always cross-validated using leave-one-out
  cross-validation.  This usually makes little difference in practice,
  but should be fixed for correctness.
}
\seealso{
  \code{\link{mvr}}
  \code{\link{crossval}}
  \code{\link{cvsegments}}
  \code{\link{MSEP}}
}
\examples{
data(NIR)
NIR.pcr <- pcr(y ~ X, 6, data = NIR, validation = "CV", segments = 10)
\dontrun{plot(MSEP(NIR.pcr))}
}
\keyword{regression}
\keyword{multivariate}
