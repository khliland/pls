%% $Id$
\name{cvsegments}
\alias{cvsegments}
\title{Generate segments for cross-validation}
\description{
  The function generates a list of segments for cross-validation.
  Random, consecutive and interleaved segments can be produced.
}
\usage{
cvsegments(N, k, length.seg = ceiling(N/k),
           type = c("random", "consecutive", "interleaved"))
}
\arguments{
  \item{N}{Integer.  The number of objects in the data set.}
  \item{k}{Integer.  The number of segments to return.}
  \item{length.seg}{Integer.  The length of the segments.  If
    given, it overrides \code{k}.}
  \item{type}{One of \samp{"random"}, \samp{"consecutive"} and
    \samp{"interleaved"}.  The type of segments to generate.  Default is
    \samp{"random"}.}
}
\details{
  If \code{length.seg} is specified, it is used to calculate the number of
  segments to generate.  Otherwise \code{k} must be specified.  If
  \eqn{k*length.seg \ne N}{k*length.seg <> N}, the
  \eqn{k*length.seg - N} last segments will contain only
  \eqn{length.seg - 1} indices.

  If \code{type} is \samp{"random"}, the indices are allocated to
  segments in random order.  If it is \samp{"consecutive"}, the first
  segment will contain the first \eqn{length.seg} indices, and so on.
  If \code{type} is \samp{"interleaved"}, the first segment will contain
  the indices \eqn{1, length.seg+1, 2*lenght.seg+1, \ldots,
    (k-1)*length.seg+1}, and so on.
}
\value{
  A list of vectors.  Each vector contains the indices for one segment.
  The attribute \code{"incomplete"} contains the number of incomplete
  segments, and the attribute \code{"type"} contains the type of segments.
}
\author{Bjørn-Helge Mevik and Ron Wehrens}

\examples{
## Segments for 10-fold randomised cross-validation:
cvsegments(100, 10)

## Segments with four objects, taken consecutive:
cvsegments(60, length.seg = 4, type = "cons")

## Incomplete segments
segs <- cvsegments(50, length.seg = 3)
attr(segs, "incomplete")

## Leave-one-out cross-validation:
cvsegments(100, 100)
## Leave-one-out with variable/unknown data set size n:
n <- 50
cvsegments(n, length.seg = 1)
}
\keyword{models}
