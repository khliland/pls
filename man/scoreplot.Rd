%% $Id$
\encoding{latin1}
\name{scoreplot}
\alias{scoreplot}
\alias{plot.scores}
\alias{loadingplot}
\alias{plot.loadings}
\title{Plots of Scores and Loadings}
\description{
  Functions to make scatter plots of scores and scatter or line plots of
  loadings.
}
\usage{
scoreplot(object, comps = 1:2, labels, identify = FALSE, type = "p", \dots)
\method{plot}{scores}(x, \dots)

loadingplot(object, comps = 1:2, scatter = FALSE, labels, identify = FALSE,
            type, \dots)
\method{plot}{loadings}(x, \dots)
}
\arguments{
  \item{object}{an \code{mvr} object.  The fitted model.}
  \item{comps}{integer vector.  The components to plot.}
  \item{scatter}{logical.  Whether the loadings should be plotted as a
    scatter instead of as lines.}
  \item{labels}{optional.  Labels to use instead of plotting symbols.
    Ignored in \code{loadingplot} unless \code{scatter = TRUE}.  Either
    a vector (of length > 1) of labels, or one of \samp{"names"} and
    \samp{"numbers"} for using row names and row numbers, respectively.}
  \item{identify}{logical.  Whether to use \code{identify} to
    interactively identify points.  See below.}
  \item{type}{character.  What type of plot to make.  Defaults to
    \samp{"p"} (points) for scatter plots and \samp{"l"} (lines) for
    line plots.}
  \item{x}{a \code{scores} or \code{loadings} object.  The scores or
    loadings to plot.}
  \item{\dots}{further arguments sent to the underlying plot function(s).}
}
\details{
  \code{plot.scores} is simply a wrapper calling \code{scoreplot},
  passing all arguments.  Similarly for \code{plot.loadings}.

  \code{scoreplot} makes one or more scatter plots of the scores,
  depending on how many components are selected.  If one or two
  components are selected, and \code{identify} is \code{TRUE}, the
  function \code{\link{identify}} is used to interactively identify
  points.

  If \code{scatter} is \code{TRUE}, \code{loadingplot} works exactly
  like \code{scoreplot}.  Otherwise, it makes a lineplot of the selected
  loading vectors, and if \code{identify} is \code{TRUE},
  uses \code{\link{identify}} to interactively identify points.

  \code{scoreplot} and \code{loadingplot} can also be called through the
  plot method for \code{mvr} objects, by specifying \code{plottype} as
  \samp{"scores"} or \samp{"loadings"}.  See \code{\link{plot.mvr}}.
  
  \code{scoreplot} and \code{loadingplot} also work with
  \code{princomp} objects.  (However, \code{plot(scores(\dots))} and
  \code{plot(loadings(\dots))} will not work.)
}
\value{
  The functions return whatever the underlying plot function (or
  \code{identify}) returns.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{plot.mvr}},
  \code{\link{scores}}, \code{\link{loadings}}}
\examples{
data(NIR)
mod <- plsr(y ~ X, ncomp = 10, data = NIR)
## These three are equivalent:
\dontrun{
scoreplot(mod, comps = 1:5)
plot(scores(mod), comps = 1:5)
plot(mod, plottype = "scores", comps = 1:5)

loadingplot(mod, comps = 1:5)
loadingplot(mod, comps = 1:5, scatter = TRUE) # Plot as scatterplots
}
}
\keyword{regression}
\keyword{multivariate}
\keyword{hplot}
