%% $Id$
\encoding{UTF-8}
\name{msc}
\alias{msc}
\alias{predict.msc}
\alias{makepredictcall.msc}
\title{Multiplicative Scatter Correction}
\description{
  Performs multiplicative scatter/signal correction on a data matrix.
}
\usage{
msc(X, reference = NULL)
\method{predict}{msc}(object, newdata, \dots)
\method{makepredictcall}{msc}(var, call)
}
\arguments{
  \item{X, newdata}{numeric matrices.  The data to scatter correct.}
  \item{reference}{numeric vector.  Spectre to use as reference.  If
    \code{NULL}, the column means of \code{X} are used.}
  \item{object}{an object inheriting from class \code{"msc"}, normally
    the result of a call to \code{msc} with a single matrix argument.}
  \item{var}{A variable.}
  \item{call}{The term in the formula, as a call.}
  \item{\dots}{other arguments.  Currently ignored.}
}
\details{
  \code{makepredictcall.msc} is an internal utility function; it is not
  meant for interactive use.  See \code{\link{makepredictcall}} for details.
}
\value{
  Both \code{msc} and \code{predict.msc} return a multiplicative scatter
  corrected matrix, with attribute \code{"reference"} the vector used as
  reference spectre. The matrix is given class \code{c("msc",
    "matrix")}.  For \code{predict.msc}, the \code{"reference"}
  attribute of \code{object} is used as reference spectre.
}
\references{
  Martens, H., Næs, T. (1989) \emph{Multivariate calibration.}
  Chichester: Wiley.
}
\author{Bjørn-Helge Mevik and Ron Wehrens}
\seealso{\code{\link{mvr}}, \code{\link{pcr}}, \code{\link{plsr}},
  \code{\link{stdize}}}
\examples{
data(yarn)
## Direct correction:
Ztrain <- msc(yarn$NIR[yarn$train,])
Ztest <- predict(Ztrain, yarn$NIR[!yarn$train,])

## Used in formula:
mod <- plsr(density ~ msc(NIR), ncomp = 6, data = yarn[yarn$train,])
pred <- predict(mod, newdata = yarn[!yarn$train,]) # Automatically scatter corrected
}
\keyword{regression}
\keyword{multivariate}
