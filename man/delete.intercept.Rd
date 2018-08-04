%% $Id$
\encoding{UTF-8}
\name{delete.intercept}
\alias{delete.intercept}
\title{Delete intercept from model matrix}
\description{
  A utility function to delete any intercept column from a model matrix,
  and adjust the \code{"assign"} attribute correspondingly.  It is used
  by formula handling functions like \code{mvr} and \code{model.matrix.mvr}.
}
\usage{
delete.intercept(mm)
}
\arguments{
  \item{mm}{Model matrix.}
}
\value{
  A model matrix without intercept column.
}
\author{Bjørn-Helge Mevik and Ron Wehrens}
\seealso{\code{\link{mvr}}, \code{\link{model.matrix.mvr}}}
\keyword{internal}
