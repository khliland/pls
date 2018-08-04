%% $Id$
\encoding{UTF-8}
\name{crossval}
\alias{crossval}
\title{Cross-validation of PLSR and PCR models}
\description{
  A \dQuote{stand alone} cross-validation function for \code{mvr} objects.
}
\usage{
crossval(object, segments = 10,
         segment.type = c("random", "consecutive", "interleaved"),
         length.seg, jackknife = FALSE, trace = 15, \dots)
}
\arguments{
  \item{object}{an \code{mvr} object; the regression to cross-validate.}
  \item{segments}{the number of segments to use, or a list with segments
    (see below).}
  \item{segment.type}{the type of segments to use.  Ignored if
    \code{segments} is a list.}
  \item{length.seg}{Positive integer.  The length of the segments to
    use.  If specified, it overrides \code{segments} unless
    \code{segments} is a list.}
  \item{jackknife}{logical.  Whether jackknifing of regression
    coefficients should be performed.}
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
  \code{crossval(mvr(\dots))} is identical to \code{mvr(\dots,
    validation = "CV")}, but slower.

  Note that to use \code{crossval}, the data \emph{must} be specified
  with a \code{data} argument when fitting \code{object}.

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

  When tracing is turned on, the segment number is printed for each segment.

  By default, the cross-validation will be performed serially.  However,
  it can be done in parallel using functionality in the
  \code{\link{parallel}} package by setting the option \code{parallel} in
  \code{\link{pls.options}}.  See \code{\link{pls.options}} for the
  different ways to specify the parallelism.  See also Examples below.
}
\value{
  The supplied \code{object} is returned, with an additional component
  \code{validation}, which is a list with components
  \item{method}{euqals \code{"CV"} for cross-validation.}
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
  \item{ncomp}{the number of components.}
  \item{gammas}{if method \code{cppls} is used, gamma values for the
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
  \code{\link{mvrCv}}
  \code{\link{cvsegments}}
  \code{\link{MSEP}}
  \code{\link{var.jack}}
  \code{\link{jack.test}}
}
\examples{
data(yarn)
yarn.pcr <- pcr(density ~ msc(NIR), 6, data = yarn)
yarn.cv <- crossval(yarn.pcr, segments = 10)
\dontrun{plot(MSEP(yarn.cv))}

\dontrun{
## Parallelised cross-validation, using transient cluster:
pls.options(parallel = 4) # use mclapply (not available on Windows)
pls.options(parallel = quote(parallel::makeCluster(4, type = "PSOCK"))) # parLapply
## A new cluster is created and stopped for each cross-validation:
yarn.cv <- crossval(yarn.pcr)
yarn.loocv <- crossval(yarn.pcr, length.seg = 1)

## Parallelised cross-validation, using persistent cluster:
library(parallel)
## This creates the cluster:
pls.options(parallel = makeCluster(4, type = "FORK")) # not available on Windows
pls.options(parallel = makeCluster(4, type = "PSOCK"))
## The cluster can be used several times:
yarn.cv <- crossval(yarn.pcr)
yarn.loocv <- crossval(yarn.pcr, length.seg = 1)
## The cluster should be stopped manually afterwards:
stopCluster(pls.options()$parallel)

## Parallelised cross-validation, using persistent MPI cluster:
## This requires the packages snow and Rmpi to be installed
library(parallel)
## This creates the cluster:
pls.options(parallel = makeCluster(4, type = "MPI"))
## The cluster can be used several times:
yarn.cv <- crossval(yarn.pcr)
yarn.loocv <- crossval(yarn.pcr, length.seg = 1)
## The cluster should be stopped manually afterwards:
stopCluster(pls.options()$parallel)
## It is good practice to call mpi.exit() or mpi.quit() afterwards:
mpi.exit()
}
}
\keyword{regression}
\keyword{multivariate}
