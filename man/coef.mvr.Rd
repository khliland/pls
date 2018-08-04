%% $Id$
\encoding{UTF-8}
\name{coef.mvr}
\alias{coef.mvr}
\alias{fitted.mvr}
\alias{residuals.mvr}
\alias{model.frame.mvr}
\alias{model.matrix.mvr}
\alias{prednames}
\alias{respnames}
\alias{compnames}
\alias{explvar}
\title{Extract Information From a Fitted PLSR or PCR Model}
\description{
  Functions to extract information from \code{mvr} objects:  Regression
  coefficients, fitted values, residuals, the model frame, the model
  matrix, names of the variables and components, and the \eqn{X}
  variance explained by the components.
}
\usage{
\method{coef}{mvr}(object, ncomp = object$ncomp, comps, intercept = FALSE, \dots)
\method{fitted}{mvr}(object, \dots)
\method{residuals}{mvr}(object, \dots)
\method{model.matrix}{mvr}(object, \dots)
\method{model.frame}{mvr}(formula, \dots)
prednames(object, intercept = FALSE)
respnames(object)
compnames(object, comps, explvar = FALSE, \dots)
explvar(object)
}
\arguments{
  \item{object, formula}{an \code{mvr} object.  The fitted model.}
  \item{ncomp, comps}{vector of positive integers.  The components to include in
    the coefficients or to extract the names of.  See below.}
  \item{intercept}{logical.  Whether coefficients for the intercept should
    be included.  Ignored if \code{comps} is specified.  Defaults to
    \code{FALSE}.}
  \item{explvar}{logical.  Whether the explained \eqn{X} variance should
    be appended to the component names.}
  \item{\dots}{other arguments sent to underlying functions.  Currently
    only used for \code{model.frame.mvr} and \code{model.matrix.mvr}.}
}
\details{
  These functions are mostly used inside other functions.  (Functions
  \code{coef.mvr}, \code{fitted.mvr} and \code{residuals.mvr} are
  usually called through their generic functions \code{\link{coef}},
  \code{\link{fitted}} and \code{\link{residuals}}, respectively.)
  
  \code{coef.mvr} is used to extract the regression coefficients of a
  model, i.e. the \eqn{B} in \eqn{y = XB} (for the \eqn{Q} in \eqn{y = TQ}
  where \eqn{T} is the scores, see \code{\link{Yloadings}}).  An array of
  dimension \code{c(nxvar, nyvar, length(ncomp))} or \code{c(nxvar, nyvar,
    length(comps))} is returned.

  If \code{comps} is missing (or is \code{NULL}),
  \code{coef()[,,ncomp[i]]} are the coefficients for models with
  \code{ncomp[i]} components, for \eqn{i = 1, \ldots, length(ncomp)}.
  Also, if \code{intercept = TRUE}, the first dimension is \eqn{nxvar +
    1}, with the intercept coefficients as the first row.

  If \code{comps} is given, however, \code{coef()[,,comps[i]]} are
  the coefficients for a model with only the component \code{comps[i]},
  i.e. the contribution of the component \code{comps[i]} on the
  regression coefficients.

  \code{fitted.mvr} and \code{residuals.mvr} return the fitted values
  and residuals, respectively.  If the model was fitted with
  \code{na.action = na.exclude} (or after setting the default
  \code{na.action} to \code{"na.exclude"} with \code{\link{options}}),
  the fitted values (or residuals) corresponding to excluded 
  observations are returned as \code{NA}; otherwise, they are omitted.

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
  returned as well.  \code{compnames} can also extract component names
  from score and loading matrices.  If \code{explvar = TRUE} in \code{compnames}, the
  explained variance for each component (if available) is appended to the component
  names.  For optimal formatting of the explained variances when not all
  components are to be used, one should specify the desired components
  with the argument \code{comps}.

  \code{explvar} extracts the amount of \eqn{X} variance (in per cent)
  explained by each component in the model.  It can also handle
  score and loading matrices returned by \code{\link{scores}} and \code{\link{loadings}}.
}
\value{
  \code{coef.mvr} returns an array of regression coefficients.

  \code{fitted.mvr} returns an array with fitted values.
  
  \code{residuals.mvr} returns an array with residuals.

  \code{model.frame.mvr} returns a data frame.
  
  \code{model.matrix.mvr} returns the \eqn{X} matrix.

  \code{prednames}, \code{respnames} and \code{compnames} return a
  character vector with the corresponding names.

  \code{explvar} returns a numeric vector with the explained variances,
  or \code{NULL} if not available.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{coef}}, \code{\link{fitted}},
  \code{\link{residuals}}, \code{\link{model.frame}},
  \code{\link{model.matrix}}, \code{\link{na.omit}}
}
\examples{
data(yarn)
mod <- pcr(density ~ NIR, data = yarn[yarn$train,], ncomp = 5)
B <- coef(mod, ncomp = 3, intercept = TRUE)
## A manual predict method:
stopifnot(drop(B[1,,] + yarn$NIR[!yarn$train,] \%*\% B[-1,,]) ==
          drop(predict(mod, ncomp = 3, newdata = yarn[!yarn$train,])))

## Note the difference in formatting:
mod2 <- pcr(density ~ NIR, data = yarn[yarn$train,])
compnames(mod2, explvar = TRUE)[1:3]
compnames(mod2, comps = 1:3, explvar = TRUE)
}
\keyword{regression}
\keyword{multivariate}
