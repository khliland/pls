%% $Id$
\encoding{latin1}
\name{residuals.mvr}
\alias{residuals.mvr}
\title{Residuals method for mvr objects}
\description{
  Extracts residuals from PLSR and PCR models.
}
\usage{
\S3method{residuals}{mvr}(object, \dots)
}
\arguments{
  \item{object}{\code{mvr} object.  Fitted model.}
  \item{\dots}{other arguments.  Currently not used.}
}
\details{
  If the model was fitted with \code{na.action = na.exclude} (or after
  setting the default \code{na.action} to \code{"na.exclude"} with
  \code{\link{options}}), the residuals values to excluded
  observations are returned as \code{NA}.  Otherwise, they are omitted.
}
\value{
  Array (nobservations x nresponses x ncomponets) with residuals.
}
\author{Bjørn-Helge Mevik and Ron Wehrens}
\seealso{\code{\link{residuals}}, \code{\link{na.omit}}, \code{\link{mvr}}}
\keyword{regression}
\keyword{multivariate}
