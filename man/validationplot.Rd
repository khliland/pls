%% $Id$
\encoding{latin1}
\name{validationplot}
\alias{validationplot}
\alias{plot.mvrVal}
\title{Validation Plots}
\description{
  Functions to plot validation statistics, such as RMSEP or R2, as a
  function of the number of components.
}
\usage{
validationplot(object, val.type = c("RMSEP", "MSEP", "R2"), estimate,
               newdata, comps, intercept, \dots)
\method{plot}{mvrVal}(x, nCols, nRows, type = "l", \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an \code{mvr} object.}
  \item{val.type}{character.  What type of validation statistic to plot.}
  \item{estimate}{character.  Which estimates of the statistic to
    calculate.  See \code{\link{RMSEP}}.}
  \item{newdata}{data frame.  Optional new data used to calculate statistic.}
  \item{comps}{integer vector.  The model sizes to compute the statistic
    for.  See \code{\link{RMSEP}}.}
  \item{intercept}{logical.  Whether estimates for a model with zero
    components should be calculated as well.}
  \item{x}{an \code{mvrVal} object.  Usually the result of a
    \code{\link{RMSEP}}, \code{\link{MSEP}} or \code{\link{R2}} call.}
  \item{nCols, nRows}{integers.  The number of coloumns and rows the
    plots will be laid out in.  If not specified, \code{plot.mvrVal} tries
    to be intelligent.}
  \item{type}{character.  What type of plots to create.  Defaults to
    \code{"l"} (lines).}
  \item{\dots}{Further arguments sent to underlying plot functions.}
}
\details{
  \code{validationplot} calls the proper validation function (currently
  \code{\link{MSEP}}, \code{\link{RMSEP}} or \code{\link{R2}}) and plots
  the results with \code{plot.mvrVal}.  \code{validationplot} can be
  called through the \code{mvr} plot method, by specifying
  \code{plottype = "validation"}.

  \code{plot.mvrVal} creates one plot for each response variable in the
  model, laid out in a rectangle.
}
\value{
  The functions do not return any values.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\note{The handling of optional arguments (\dots) between these functions
  is a bit rough, and not thoroughly tested.}
\seealso{\code{\link{mvr}}, \code{\link{plot.mvr}}, \code{\link{RMSEP}},
  \code{\link{MSEP}}, \code{\link{R2}}}
\examples{
data(sensory)
mod <- plsr(Panel ~ Quality, data = sensory, validation = "LOO")
\dontrun{
## These three are equivalent:
validationplot(mod, estimate = "all")
plot(mod, "validation", estimate = "all")
plot(RMSEP(mod, estimate = "all"))
## Plot R2:
plot(mod, "validation", val.type = "R2")
}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
