%% $Id$
\encoding{latin1}
\name{scoreplot}
\alias{scoreplot}
\alias{scoreplot.default}
\alias{plot.scores}
\alias{loadingplot}
\alias{plot.loadings}
\alias{corrplot}
\title{Plots of Scores, Loadings and Correlation Loadings}
\description{
  Functions to make scatter plots of scores or correlation loadings, and
  scatter or line plots of loadings.
}
\usage{
scoreplot(object, \dots)
\method{scoreplot}{default}(object, comps = 1:2, labels, identify = FALSE, type = "p",
          xlab, ylab, \dots)
\method{plot}{scores}(x, \dots)

loadingplot(object, comps = 1:2, scatter = FALSE, labels,
            identify = FALSE, type, lty, lwd = NULL, pch, cex = NULL,
            col, legendpos, xlab, ylab, \dots)
\method{plot}{loadings}(x, \dots)

corrplot(object, comps = 1:2, labels, identify = FALSE, type = "p",
          xlab, ylab, \dots)
}
\arguments{
  \item{object}{an \R object.  The fitted model.}
  \item{comps}{integer vector.  The components to plot.}
  \item{scatter}{logical.  Whether the loadings should be plotted as a
    scatter instead of as lines.}
  \item{labels}{optional.  Labels to use instead of plotting symbols.
    If \code{scatter = FALSE} in \code{loadingplot}, they are used to
    label the \eqn{x} axis tick marks.  Either a vector (of length > 1)
    of labels, or one of \code{"names"} and \code{"numbers"} for using
    row names and row numbers, respectively.}
  \item{identify}{logical.  Whether to use \code{identify} to
    interactively identify points.  See below.}
  \item{type}{character.  What type of plot to make.  Defaults to
    \code{"p"} (points) for scatter plots and \code{"l"} (lines) for
    line plots.  See \code{\link{plot}} for a complete list
    of types (not all types are possible/meaningful for all plots).}
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
  \item{legendpos}{Legend position.  Optional.  Ignored if \code{scatter} is
    \code{TRUE}.  If present, a legend is drawn at the given position.
    The position can be specified symbolically (e.g., \code{legendpos =
      "topright"}).  This requires \R >= 2.1.0.  Alternatively, the
    position can be specified explicitly (\code{legendpos = t(c(x,y))})
    or interactively (\code{legendpos = \link{locator}()}).}
  \item{xlab,ylab}{titles for \eqn{x} and \eqn{y} axes.  Typically
    character strings, but can be expressions or lists.  See
    \code{\link{title}} for details.}
  \item{x}{a \code{scores} or \code{loadings} object.  The scores or
    loadings to plot.}
  \item{\dots}{further arguments sent to the underlying plot function(s).}
}
\details{
  \code{plot.scores} is simply a wrapper calling \code{scoreplot},
  passing all arguments.  Similarly for \code{plot.loadings}.

  \code{scoreplot} is generic, currently with a default method that
  works for matrices and any object for which \code{\link{scores}}
  returns a matrix.  The default \code{scoreplot} method
  makes one or more scatter plots of the scores,
  depending on how many components are selected.  If one or two
  components are selected, and \code{identify} is \code{TRUE}, the
  function \code{\link{identify}} is used to interactively identify
  points.

  If \code{scatter} is \code{TRUE}, \code{loadingplot} works exactly
  like the default \code{scoreplot} method.  Otherwise, it makes a lineplot of the selected
  loading vectors, and if \code{identify} is \code{TRUE},
  uses \code{\link{identify}} to interactively identify points.  Also,
  if \code{legendpos} is given, a legend is drawn at the position
  indicated.

  \code{loadingplot} also works with \code{princomp} objects.  (However,
  \code{plot(loadings(\dots))} will not work.)
  
  \code{corrplot} works exactly like the default \code{scoreplot} method, except that at
  least two components must be selected.  The \dQuote{correlation
    loadings}, i.e. the correlations between each variable and the
  selected components, are plotted as pairwise scatter plots, with
  two concentric circles of radius 0.5 and 1.

  \code{scoreplot}, \code{loadingplot} and \code{corrplot} can also be
  called through the plot method for \code{mvr} objects, by specifying
  \code{plottype} as \code{"scores"}, \code{"loadings"} or
  \code{"correlation"}, respectively.  See \code{\link{plot.mvr}}.
}
\value{
  The functions return whatever the underlying plot function (or
  \code{identify}) returns.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\note{\code{\link{legend}} has many options.  If you want greater
  control over the appearance of the legend, omit the \code{legendpos}
  argument and call \code{legend} manually.

  Graphical parametres (such as \code{pch} and \code{cex}) can also
  be used with \code{scoreplot} and \code{corrplot}.  They are not
  listed in the argument list simply because they are not handled
  specifically in the function (unlike in \code{loadingplot}), but
  passed directly to the underlying plot functions by \code{\dots}.
}
\seealso{\code{\link{mvr}}, \code{\link{plot.mvr}},
  \code{\link{scores}}, \code{\link{loadings}}, \code{\link{identify}},
  \code{\link{legend}}}
\examples{
data(NIR)
mod <- plsr(y ~ X, ncomp = 10, data = NIR)
## These three are equivalent:
\dontrun{
scoreplot(mod, comps = 1:5)
plot(scores(mod), comps = 1:5)
plot(mod, plottype = "scores", comps = 1:5)

loadingplot(mod, comps = 1:5)
loadingplot(mod, comps = 1:5, legendpos = "topright") # With legend
loadingplot(mod, comps = 1:5, scatter = TRUE) # Plot as scatterplots

corrplot(mod, comps = 1:2)
corrplot(mod, comps = 1:3)
}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
