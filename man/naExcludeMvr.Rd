\name{naExcludeMvr}
\alias{naExcludeMvr}
\title{Adjust for Missing Values}
\description{
  Use missing value information to adjust residuals and predictions.
  This is the \sQuote{mvr equivalent} of the \code{naresid.exclude} and
  \code{napredict.exclude} functions.
}
\usage{
naExcludeMvr(omit, x, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{omit}{an object produced by an \code{na.action} function,
    typically the \code{"na.action"} attribute of the result of
    \code{na.omit} or \code{na.exclude}.}
  \item{x}{a three-dimensional array to be adjusted based upon the
    missing value information in \code{omit}.}
  \item{\dots}{further arguments.  Currently not used.}
}
\details{
  This is a utility function used to allow \code{predict.mvr} and
  \code{residuals.mvr} to compensate for the removal of \code{NA}s in
  the fitting process.

  It is called only when the \code{na.action} is \code{na.exclude}, and
  pads \code{x} with \code{NA}s in the correct positions to have the
  same number of rows as the original data frame.
}
\value{
  \code{x}, padded with \code{NA}s along the first dimension
  (\sQuote{rows}).
}
\author{Bjørn-Helge Mevik and Ron Wehrens}
\seealso{\code{\link{predict.mvr}}, \code{\link{residuals.mvr}},
  \code{\link{napredict}}, \code{\link{naresid}}}
\keyword{regression}
\keyword{multivariate}
\keyword{internal}
