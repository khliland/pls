%% $Id$
\name{coef.mvr}
\alias{coef.mvr}
\alias{model.matrix.mvr}
\alias{R2}
\title{Extract Information From a Fitted PLSR or PCR Model}
\description{
  Functions to extract the regression coefficients, the model matrix or the
  calibration \eqn{R^2} from \code{mvr} objects.
}
\usage{
\method{coef}{mvr}(object, comps = object$ncomp, intercept = FALSE,
     cumulative = TRUE, \dots)
\method{model.matrix}{mvr}(object, \dots)
R2(object)
}
\arguments{
  \item{object}{an \code{mvr} object.  The fitted model.}
  \item{comps}{vector of positive integers.  The components to include in
    the coefficients.  Defaults to the number of components used for
    fitting the model.}
  \item{intercept}{logical.  Whether coefficients for the intercept should
    be included.  Ignored if \code{cumulative = FALSE}.  Defaults to
    \samp{FALSE}.}
  \item{cumulative}{logical.  Whether cumulative (the default) or
    individual coefficients for each component should be returned.  See below.}
  \item{\dots}{other arguments sent to underlying functions.  Currently
    only used for \code{model.matrix.mvr}.}
}
\details{
  \code{coef.mvr} is used to extract the regression coefficients of a
  model, i.e. the \eqn{B} in \eqn{y = XB}.  An array of dimension
  \code{c(nxvar, nyvar, length(comps))} is returned.  

  If \code{cumulative = TRUE}, \code{coef()[,,comps[i]]} are
  the coefficients for models with \code{comps[i]} components, for
  \eqn{i = 1, \ldots, length(comps)}.  Also, if \code{intercept = TRUE},
  the first dimension is \eqn{nxvar + 1}, with the intercept
  coefficients as the first row.

  If \code{cumulative = FALSE}, however, \code{coef()[,,comps[i]]} are
  the coefficients for a model with only the component \code{comps[i]},
  i.e. the contribution of the component \code{comps[i]} on the
  regression coefficients.

  \code{model.matrix.mvr} returns the (possibly coded) matrix used as
  \eqn{X} in the fitting.
  
  \code{R2} simply returns the \code{R2} component of the fitted model,
  i.e. the \eqn{R^2} of the calibration.
}
\value{
  \code{coef.mvr} returns an array of regression coefficients.
  
  \code{model.matrix.mvr} returns the \eqn{X} matrix.
  
  \code{R2} returns a vector of \eqn{R^2} values.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{predict.mvr}}, \code{\link{scores}}}
\examples{
data(NIR)
mod <- pcr(y ~ X, data = NIR[NIR$train,], ncomp = 5)
B <- coef(mod, comps = 3, intercept = TRUE)
## A manual predict method:
stopifnot(drop(B[1,,] + NIR$X[!NIR$train,] \%*\% B[-1,,]) ==
          drop(predict(mod, comps = 3, newdata = NIR[!NIR$train,])))
}
\keyword{regression}
\keyword{multivariate}
