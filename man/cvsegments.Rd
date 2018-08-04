%% $Id$
\encoding{UTF-8}
\name{cvsegments}
\alias{cvsegments}
\title{Generate segments for cross-validation}
\description{
  The function generates a list of segments for cross-validation.
  It can generate random, consecutive and interleaved segments, and
  supports keeping replicates in the same segment.
}
\usage{
cvsegments(N, k, length.seg = ceiling(N / k), nrep = 1,
           type = c("random", "consecutive", "interleaved"))
}
\arguments{
  \item{N}{Integer.  The number of rows in the data set.}
  \item{k}{Integer.  The number of segments to return.}
  \item{length.seg}{Integer.  The length of the segments.  If
    given, it overrides \code{k}.}
  \item{nrep}{Integer.  The number of (consecutive) rows that are
    replicates of the same object.  Replicates will always be kept in
    the same segment.}
  \item{type}{One of \code{"random"}, \code{"consecutive"} and
    \code{"interleaved"}.  The type of segments to generate.  Default is
    \code{"random"}.}
}
\details{
  If \code{length.seg} is specified, it is used to calculate the number of
  segments to generate.  Otherwise \code{k} must be specified.  If
  \eqn{k*length.seg \ne N}{k*length.seg <> N}, the
  \eqn{k*length.seg - N} last segments will contain only
  \eqn{length.seg - 1} indices.

  If \code{type} is \code{"random"}, the indices are allocated to
  segments in random order.  If it is \code{"consecutive"}, the first
  segment will contain the first \eqn{length.seg} indices, and so on.
  If \code{type} is \code{"interleaved"}, the first segment will contain
  the indices \eqn{1, length.seg+1, 2*lenght.seg+1, \ldots,
    (k-1)*length.seg+1}, and so on.

  If \eqn{nrep > }, it is assumed that each \code{nrep} consecutive rows
  are replicates (repeated measurements) of the same object, and care is
  taken that replicates are never put in different segments.

  Warning: If \code{k} does not divide \code{N}, a specified
  \code{length.seg} does not divide \code{N}, or \code{nrep} does not
  divide \code{length.seg}, the number of segments and/or the segment
  length will be adjusted as needed.  Warnings are printed for some of
  these cases, and one should always inspect the resulting segments to
  make sure they are as expected.
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

## Data set with replicates
cvsegments(100, 25, nrep = 2)
## Note that rows 1 and 2 are in the same segment, rows 3 and 4 in the
## same segment, and so on.
}
\keyword{models}
