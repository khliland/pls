%% $Id$
\encoding{UTF-8}
\name{biplot.mvr}
\alias{biplot.mvr}
\title{Biplots of PLSR and PCR Models.}
\description{
  Biplot method for \code{mvr} objects.
}
\usage{
\method{biplot}{mvr}(x, comps = 1:2, which = c("x", "y", "scores", "loadings"),
           var.axes = FALSE, xlabs, ylabs, main, \dots)
}
\arguments{
  \item{x}{an \code{mvr} object.}
  \item{comps}{integer vector of length two.  The components to plot.}
  \item{which}{character.  Which matrices to plot.  One of \code{"x"} (X
    scores and loadings), \code{"y"} (Y scores and loadings),
    \code{"scores"} (X and Y scores) and \code{"loadings"} (X and Y loadings).}
  \item{var.axes}{logical.  If \code{TRUE}, the second set of points
    have arrows representing them.}
  \item{xlabs}{either a character vector of labels for the first set of
    points, or \code{FALSE} for no labels.  If missing, the row names of
    the first matrix is used as labels.}
  \item{ylabs}{either a character vector of labels for the second set of
    points, or \code{FALSE} for no labels.  If missing, the row names of
    the second matrix is used as labels.}
  \item{main}{character.  Title of plot.  If missing, a title is
    constructed by \code{biplot.mvr}.}
  \item{\dots}{Further arguments passed on to \code{biplot.default}.}
}
\details{
  \code{biplot.mvr} can also be called through the \code{mvr} plot
  method by specifying \code{plottype = "biplot"}.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{plot.mvr}}, \code{\link{biplot.default}}}
\examples{
data(oliveoil)
mod <- plsr(sensory ~ chemical, data = oliveoil)
\dontrun{
## These are equivalent
biplot(mod)
plot(mod, plottype = "biplot")

## The four combinations of x and y points:
par(mfrow = c(2,2))
biplot(mod, which = "x") # Default
biplot(mod, which = "y")
biplot(mod, which = "scores")
biplot(mod, which = "loadings")
}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
