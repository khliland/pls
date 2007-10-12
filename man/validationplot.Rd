%% $Id$
\encoding{latin1}
\name{validationplot}
\alias{validationplot}
\alias{plot.mvrVal}
\title{Validation Plots}
\description{
  Functions to plot validation statistics, such as RMSEP or \eqn{R^2}, as a
  function of the number of components.
}
\usage{
validationplot(object, val.type = c("RMSEP", "MSEP", "R2"), estimate,
               newdata, ncomp, comps, intercept, \dots)
\method{plot}{mvrVal}(x, nCols, nRows, type = "l", lty = 1:nEst, lwd = NULL,
           pch = 1:nEst, cex = NULL, col = 1:nEst, legendpos,
           xlab = "number of components", ylab = x$type, \dots)
}
\arguments{
  \item{object}{an \code{mvr} object.}
  \item{val.type}{character.  What type of validation statistic to plot.}
  \item{estimate}{character.  Which estimates of the statistic to
    calculate.  See \code{\link{RMSEP}}.}
  \item{newdata}{data frame.  Optional new data used to calculate statistic.}
  \item{ncomp, comps}{integer vector.  The model sizes to compute the statistic
    for.  See \code{\link{RMSEP}}.}
  \item{intercept}{logical.  Whether estimates for a model with zero
    components should be calculated as well.}
  \item{x}{an \code{mvrVal} object.  Usually the result of a
    \code{\link{RMSEP}}, \code{\link{MSEP}} or \code{\link{R2}} call.}
  \item{nCols, nRows}{integers.  The number of coloumns and rows the
    plots will be laid out in.  If not specified, \code{plot.mvrVal} tries
    to be intelligent.}
  \item{type}{character.  What type of plots to create.  Defaults to
    \code{"l"} (lines).  Alternative types include \code{"p"} (points)
    and \code{"b"} (both).  See \code{\link{plot}} for a complete list
    of types.}
  \item{lty}{vector of line types (recycled as neccessary).  Line types can be
    specified as integers or character strings (see \code{\link{par}}
    for the details).}
  \item{lwd}{vector of positive numbers (recycled as neccessary), giving
    the width of the lines.}
  \item{pch}{plot character.  A character string or a vector of
    single characters or integers (recycled as neccessary).  See
    \code{\link{points}} for all alternatives.}
  \item{cex}{numeric vector of character expansion sizes (recycled as
    neccessary) for the plotted symbols.}
  \item{col}{character or integer vector of colors for plotted lines and
    symbols (recycled as neccessary).  See \code{\link{par}} for the details.}
  \item{legendpos}{Legend position.  Optional.  If present, a legend is
    drawn at the given position.  The position can be specified
    symbolically (e.g., \code{legendpos = "topright"}).  This requires
    \R >= 2.1.0.  Alternatively, the position can be specified
    explicitly (\code{legendpos = t(c(x,y))}) or interactively
    (\code{legendpos = \link{locator}()}).  This only works well for
    plots of single-response models.}
  \item{xlab,ylab}{titles for \eqn{x} and \eqn{y} axes.  Typically
    character strings, but can be expressions (e.g.,
    \code{expression(R^2)} or lists.  See \code{\link{title}} for details.}
  \item{\dots}{Further arguments sent to underlying plot functions.}
}
\details{
  \code{validationplot} calls the proper validation function (currently
  \code{\link{MSEP}}, \code{\link{RMSEP}} or \code{\link{R2}}) and plots
  the results with \code{plot.mvrVal}.  \code{validationplot} can be
  called through the \code{mvr} plot method, by specifying
  \code{plottype = "validation"}.

  \code{plot.mvrVal} creates one plot for each response variable in the
  model, laid out in a rectangle.  It uses \code{\link{matplot}} for
  performing the actual plotting.  If \code{legendpos} is given, a
  legend is drawn at the given position.
}
\value{
  If \code{legendpos} is given, the functions return whatever
  \code{legend} returns.  Otherwise they do not return any values.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\note{\code{\link{legend}} has many options.  If you want greater
  control over the appearance of the legend, omit the \code{legendpos}
  argument and call \code{legend} manually.
}
\seealso{\code{\link{mvr}}, \code{\link{plot.mvr}}, \code{\link{RMSEP}},
  \code{\link{MSEP}}, \code{\link{R2}}, \code{\link{matplot}},
  \code{\link{legend}}}
\examples{
data(oliveoil)
mod <- plsr(sensory ~ chemical, data = oliveoil, validation = "LOO")
\dontrun{
## These three are equivalent:
validationplot(mod, estimate = "all")
plot(mod, "validation", estimate = "all")
plot(RMSEP(mod, estimate = "all"))
## Plot R2:
plot(mod, "validation", val.type = "R2")
## Plot R2, with a legend:
plot(mod, "validation", val.type = "MSEP", legendpos = "top") # R >= 2.1.0
}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
