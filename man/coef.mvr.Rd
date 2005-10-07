%% $Id$
\encoding{latin1}
\name{coef.mvr}
\alias{coef.mvr}
\alias{model.frame.mvr}
\alias{model.matrix.mvr}
\alias{prednames}
\alias{respnames}
\alias{compnames}
\alias{explvar}
\title{Extract Information From a Fitted PLSR or PCR Model}
\description{
  Functions to extract information from \code{mvr} objects:  Regression
  coefficients, the model frame, the the model matrix, names of the
  variables and components, and the \eqn{X} variance explained by the
  components.
}
\usage{
\method{coef}{mvr}(object, comps = object$ncomp, intercept = FALSE,
     cumulative = TRUE, \dots)
\method{model.matrix}{mvr}(object, \dots)
\method{model.frame}{mvr}(formula, \dots)
prednames(object, intercept = FALSE)
respnames(object)
compnames(object, comps = 1:object$ncomp, explvar = FALSE)
explvar(object)
}
\arguments{
  \item{object, formula}{an \code{mvr} object.  The fitted model.}
  \item{comps}{vector of positive integers.  The components to include in
    the coefficients or to extract the names of.}
  \item{intercept}{logical.  Whether coefficients for the intercept should
    be included.  Ignored if \code{cumulative = FALSE}.  Defaults to
    \code{FALSE}.}
  \item{cumulative}{logical.  Whether cumulative (the default) or
    individual coefficients for each component should be returned.  See below.}
  \item{explvar}{logical.  Whether the explained \eqn{X} variance should
    be appended to the component names.}
  \item{\dots}{other arguments sent to underlying functions.  Currently
    only used for \code{model.frame.mvr} and \code{model.matrix.mvr}.}
}
\details{
  Except for \code{coef.mvr}, these functions are mostly used inside
  other functions.
  
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

  \code{model.frame.mvr} returns the model frame; i.e. a data frame with
  all variables neccessary to generate the model matrix.  See
  \code{\link[stats]{model.frame}} for details.
  
  \code{model.matrix.mvr} returns the (possibly coded) matrix used as
  \eqn{X} in the fitting.  See \code{\link[stats]{model.matrix}} for
  details.

  \code{prednames}, \code{respnames} and \code{compnames} extract the
  names of the \eqn{X} variables, responses and components,
  respectively.  With \code{intercept = TRUE} in \code{prednames},
  the name of the intercept variable (i.e. \code{"(Intercept)"}) is
  returned as well.  If \code{explvar = TRUE} in \code{compnames}, the
  explained variance for each component is appended to the component
  names.  For optimal formatting of the explained variances when not all
  components are to be used, one should specify the desired components
  with the argument \code{comps}.

  \code{explvar} extracts the amount of \eqn{X} variance (in per cent)
  explained by for each component in the model.
}
\value{
  \code{coef.mvr} returns an array of regression coefficients.
  
  \code{model.frame.mvr} returns a data frame.
  
  \code{model.matrix.mvr} returns the \eqn{X} matrix.

  \code{prednames}, \code{respnames} and \code{compnames} return a
  character vector with the corresponding names.

  \code{explvar} returns a numeric vector with the explained variances.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}}
\examples{
data(NIR)
mod <- pcr(y ~ X, data = NIR[NIR$train,], ncomp = 5)
B <- coef(mod, comps = 3, intercept = TRUE)
## A manual predict method:
stopifnot(drop(B[1,,] + NIR$X[!NIR$train,] \%*\% B[-1,,]) ==
          drop(predict(mod, comps = 3, newdata = NIR[!NIR$train,])))

## Note the difference in formatting:
mod2 <- pcr(y ~ X, data = NIR[NIR$train,])
compnames(mod2, explvar = TRUE)[1:3]
compnames(mod2, comps = 1:3, explvar = TRUE)
}
\keyword{regression}
\keyword{multivariate}
