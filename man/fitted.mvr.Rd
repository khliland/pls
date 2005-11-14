%% $Id$
\encoding{latin1}
\name{fitted.mvr}
\alias{fitted.mvr}
\title{Fitted method for Mvr Objects}
\description{
  Extracts fitted values from PLSR and PCR models.
}
\usage{
\S3method{fitted}{mvr}(object, \dots)
}
\arguments{
  \item{object}{\code{mvr} object.  Fitted model.}
  \item{\dots}{other arguments.  Currently not used.}
}
\details{
  If the model was fitted with \code{na.action = na.exclude} (or after
  setting the default \code{na.action} to \code{"na.exclude"} with
  \code{\link{options}}), the fitted values corresponding to excluded
  observations are returned as \code{NA}.  Otherwise, they are omitted.
}
\value{
  Array (nobservations x nresponses x ncomponets) with fitted values.
}
\author{Bjørn-Helge Mevik and Ron Wehrens}
\seealso{\code{\link{fitted}}, \code{\link{na.omit}}, \code{\link{mvr}}}
\keyword{regression}
\keyword{multivariate}
