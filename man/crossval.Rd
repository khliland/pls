%% $Id$
\encoding{latin1}
\name{crossval}
\alias{crossval}
\title{Cross-validation of PLSR and PCR models}
\description{
  A \dQuote{stand alone} cross-validation function for \code{mvr} objects.
}
\usage{
crossval(object, segments = 10,
         segment.type = c("random", "consecutive", "interleaved"),
         length.seg, trace = 15, \dots)
}
\arguments{
  \item{object}{a \code{mvr} object; the regression to cross-validate.}
  \item{segments}{the number of segments to use, or a list with segments
    (see below).  Ignored if \code{loo = TRUE}.}
  \item{segment.type}{the type of segments to use.  Ignored if
    \code{segments} is a list.}
  \item{length.seg}{Positive integer.  The length of the segments to
    use.  If specified, it overrides \code{segments}.}
  \item{trace}{if \code{TRUE}, tracing is turned on.  If numeric, it
    denotes a time limit (in seconds).  If the estimated total time of
    the cross-validation exceeds this limit, tracing is turned on.}
  \item{\dots}{additional arguments, sent to the underlying fit function.}
}
\details{
  This function performs cross-validation on a model fit by \code{mvr}.
  It can handle models such as \code{plsr(y ~ msc(X), \dots)} or other
  models where the predictor variables need to be recalculated for each
  segment.  When recalculation is not needed, the result of
  \code{crossval(mvr(\dots))} is identical to \code{mvr(\dots, CV = TRUE)},
  but slower.

  If \code{length.seg} is specified, segments of the requested length
  are used.  Otherwise:
  If \code{segments} is a number, it specifies the number of segments to
  use, and \code{segment.type} is used to select the type of segments.
  If \code{segments} is a list, the elements of the list should be
  integer vectors specifying the indices of the segments.  See
  \code{\link{cvsegments}} for details.

  The R2 component returned is calculated as the squared correlation
  between the cross-validated predictions and the responses.

  When tracing is turned on, the segment number is printed for each segment.
}
\value{
  The supplied \code{object} is returned, with an additional component
  \code{validation}, which is a list with components
  \item{method}{euqals \samp{"CV"} for cross-validation.}
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
  \code{\link{mvrCv}}
  \code{\link{cvsegments}}
  \code{\link{MSEP}}
}
\examples{
data(NIR)
NIR.pcr <- pcr(y ~ msc(X), 6, data = NIR)
NIR.cv <- crossval(NIR.pcr, CV = TRUE, segments = 10)
plot(MSEP(NIR.cv))
}
\keyword{regression}
\keyword{multivariate}
