%% $Id$
\name{mvr}
\alias{mvr}
\alias{pls}
\alias{pcr}
\alias{plsr}
\title{Partial Least Squares and Principal Components Regression}
\description{
  Functions to perform partial least squares regression (PLSR) or
  principal components regression (PCR), with a formula interface.
  Cross-validation can be used.  Prediction, model extraction, plot,
  print and summary methods exist.
}
%% FIXME: Maybe use a \synopsis section, and show typical uses in \usage:
\usage{
mvr(formula, ncomp, data, subset, na.action,
    method = c("kernelpls", "simpls", "oscorespls", "svdpc"),
    CV = FALSE, model = TRUE, x = FALSE, y = FALSE, \dots)
plsr(\dots, method = c("kernelpls", "simpls", "oscorespls"))
pls(\dots, method = c("kernelpls", "simpls", "oscorespls"))
pcr(\dots, method = "svdpc")
}
\arguments{
  \item{formula}{a model formula.  Most of the \code{lm} formula
    constructs are supported.}
  \item{ncomp}{the number of components to include in the model (see below).}
  \item{data}{an optional data frame with the data to fit the model from.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain missing values.}
  \item{method}{the multivariate regression method to be used.}
  \item{CV}{a logical.  Wether to cross-validate the model.  See below.}
  \item{model}{a logical.  If \code{TRUE}, the model frame is returned.}
  \item{x}{a logical.  If \code{TRUE}, the model matrix is returned.}
  \item{y}{a logical.  If \code{TRUE}, the response is returned.}
  \item{\dots}{additional arguments, passed to the underlying fit
    functions, and \code{mvrCv}.}
}
\details{
  The functions fit PLSR or PCR models with 1, \eqn{\ldots},
  \code{ncomp} number of components.  Multi-response models are fully
  supported.

  Three PLSR algorithms are available: the kernel algorithm, SIMPLS and
  the classical orthogonal scores algorithm.  One PCR algorithm is
  available: using the singular value decomposition.  The type of
  regression is specified with the \code{method} argument.  \code{pcr}
  are wrappers for \code{mvr}, with different values for \code{method}.

  If \code{CV = TRUE}, cross-validation is performed.  The number and
  type of cross-validation segments are specified with the arguments
  \code{segments} and \code{segment.type}.  See \code{\link{mvrCv}} for
  details.

  Note that the cross-validation is optimised for speed, and some
  generality has been sacrificed.  Especially, the model matrix is
  calculated only once for the complete cross-validation, so models like
  \code{y ~ msc(X)} will not be properly cross-validated.  For proper
  cross-validation of models where the model matrix must be
  updated/regenerated for each segment, use the separate function
  \code{\link{crossval}}.
}
\value{
  \code{mvr} returns an object of class \code{mvr}.
  The object contains all components returned by the underlying fit
  function.  In addition, it contains the following components:
  \item{validation}{if \code{CV = TRUE}, the results of the cross-validation.
    See \code{\link{mvrCv}} for details.}
  \item{ncomp}{the number of components of the model.}
  \item{method}{the method used to fit the model.  See the argument
    \code{method} for possible values.}
  \item{call}{the function call.}
  \item{terms}{the model terms.}
  \item{model}{if \code{model = TRUE}, the model frame.}
  \item{x}{if \code{x = TRUE}, the model matrix.}
  \item{y}{if \code{y = TRUE}, the model response.}
}
\references{
  Martens, H., Næs, T. (1989) \emph{Multivariate calibration.}
  Chichester: Wiley.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
%% FIXME: Add more functions:
\seealso{
  \code{\link{kernelpls.fit}}
  \code{\link{simpls.fit}}
  \code{\link{oscorespls.fit}}
  \code{\link{svdpc.fit}}
  \code{\link{mvrCv}}
  \code{\link[stats]{loadings}},
  \code{\link{scores}},
  \code{\link{loading.weights}},
  \code{\link{coef.mvr}},
  \code{\link{predict.mvr}},
  \code{\link{MSEP}}}
  \code{\link{RMSEP}}}
%% FIXME: Add plots (with \dontrun):
\examples{
data(NIR)
## Default methods:
NIR.pcr <- pcr(y ~ X, 6, data = NIR, CV = TRUE)
NIR.pls <- plsr(y ~ X, 6, data = NIR, CV = TRUE)

## Alternative methods:
NIR.oscorespls <- mvr(y ~ X, 6, data = NIR, CV = TRUE,
                      method = "oscorespls")
NIR.simpls <- mvr(y ~ X, 6, data = NIR, CV = TRUE,
                  method = "simpls")

data(sensory)
Pn <- scale(sensory$Panel)
Ql <- scale(sensory$Quality)
sens.pcr <- pcr(Ql ~ Pn, ncomp = 4)
sens.pls <- pls(Ql ~ Pn, ncomp = 4)
}
\keyword{regression}
\keyword{multivariate}
