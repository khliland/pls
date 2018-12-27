\encoding{UTF-8}
\name{mvr}
\alias{mvr}
\alias{pcr}
\alias{plsr}
\alias{cppls}
\title{Partial Least Squares and Principal Component Regression}
\description{
  Functions to perform partial least squares regression (PLSR),
  canonical powered partial least squares (CPPLS) or
  principal component regression (PCR), with a formula interface.
  Cross-validation can be used.  Prediction, model extraction, plot,
  print and summary methods exist.
}
\usage{
mvr(formula, ncomp, Y.add, data, subset, na.action,
    method = pls.options()$mvralg,
    scale = FALSE, center = TRUE, validation = c("none", "CV", "LOO"),
    model = TRUE, x = FALSE, y = FALSE, \dots)
plsr(\dots, method = pls.options()$plsralg)
cppls(\dots, Y.add, weights, method = pls.options()$cpplsalg)
pcr(\dots, method = pls.options()$pcralg)
}
\arguments{
  \item{formula}{a model formula.  Most of the \code{\link{lm}} formula
    constructs are supported.  See below.}
  \item{ncomp}{the number of components to include in the model (see below).}
  \item{Y.add}{a vector or matrix of additional responses containing
    relevant information about the observations.  Only used for \code{cppls}.}
  \item{data}{an optional data frame with the data to fit the model from.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used in the fitting process.}
  \item{na.action}{a function which indicates what should happen when
    the data contain missing values.  The default is set by
    the \code{na.action} setting of \code{\link{options}}, and is
    \code{\link{na.fail}} if that is unset.  The \sQuote{factory-fresh}
    default is \code{\link{na.omit}}.  Another possible value is
    \code{NULL}, no action.  Value \code{\link{na.exclude}} can be
    useful.  See \code{\link{na.omit}} for other alternatives.}
  \item{method}{the multivariate regression method to be used.  If
    \code{"model.frame"}, the model frame is returned.}
  \item{scale}{numeric vector, or logical.  If numeric vector, \eqn{X}
    is scaled by dividing each variable with the corresponding element
    of \code{scale}.  If \code{scale} is \code{TRUE}, \eqn{X} is scaled
    by dividing each variable by its sample standard deviation.  If
    cross-validation is selected, scaling by the standard deviation is
    done for every segment.}
  \item{center}{logical, determines if the \eqn{X} and \eqn{Y} matrices
    are mean centered or not. Default is to perform mean centering.}
  \item{validation}{character.  What kind of (internal) validation to
    use.  See below.}
  \item{model}{a logical.  If \code{TRUE}, the model frame is returned.}
  \item{x}{a logical.  If \code{TRUE}, the model matrix is returned.}
  \item{y}{a logical.  If \code{TRUE}, the response is returned.}
  \item{weights}{a vector of individual weights for the observations.
    Only used for \code{cppls}.  (Optional)}
  \item{\dots}{additional optional arguments, passed to the underlying fit
    functions, and \code{\link{mvrCv}}.

    Currently, the fit functions \code{\link{oscorespls.fit}} and
    \code{\link{widekernelpls.fit}} implement these extra arguments:
    \describe{
      \item{tol:}{numeric.  Tolerance used for determining convergence.}
      \item{maxit:}{positive integer.  The maximal number of iterations used.}
    }
    and \code{\link{cppls.fit}} implements:
    \describe{
      \item{lower:}{a vector of lower limits for power optimisation.}
      \item{upper:}{a vector of upper limits for power optimisation.}
      \item{trunc.pow:}{logical. Whether to use an experimental alternative power algorithm.}
    }
    \code{\link{mvrCv}} implements several arguments; the following are
    probably the most useful of them:
    \describe{
      \item{segments:}{the number of segments to use, or a list with segments.}
      \item{segment.type:}{the type of segments to use.}
      \item{length.seg:}{Positive integer.  The length of the segments to use.}
      \item{jackknife:}{logical.  Whether to perform jackknifing of regression
	coefficients.}
    }

    See the functions' documentation for details.
  }
}
\details{
  The functions fit PLSR, CPPLS or PCR models with 1, \eqn{\ldots},
  \code{ncomp} number of components.  Multi-response models are fully
  supported.

  The type of model to fit is specified with the \code{method}
  argument. Four PLSR algorithms are available: the kernel algorithm
  (\code{"kernelpls"}), the wide kernel algorithm (\code{"widekernelpls"}),
  SIMPLS (\code{"simpls"}) and the classical orthogonal scores algorithm
  (\code{"oscorespls"}). One CPPLS algorithm is available (\code{"cppls"})
  providing several extensions to PLS. One PCR algorithm
  is available: using the singular value decomposition (\code{"svdpc"}).
  If \code{method} is \code{"model.frame"}, the model frame is returned.
  The functions \code{pcr}, \code{plsr} and \code{cppls}
  are wrappers for \code{mvr}, with different values for \code{method}.

  The \code{formula} argument should be a symbolic formula of the form
  \code{response ~ terms}, where \code{response} is the name of the
  response vector or matrix (for multi-response models) and \code{terms}
  is the name of one or more predictor matrices, usually separated by
  \code{+}, e.g., \code{water ~ FTIR} or \code{y ~ X + Z}.  See
  \code{\link{lm}} for a detailed description.  The named
  variables should exist in the supplied \code{data} data frame or in
  the global environment.  Note: Do not use \code{mvr(mydata$y ~
    mydata$X, \ldots)}, instead use \code{mvr(y ~ X, data = mydata,
    \ldots)}.  Otherwise, \code{\link{predict.mvr}} will not work properly.
  The chapter \samp{Statistical models in R} of the manual \samp{An
    Introduction to R} distributed with \R is a good reference on
  formulas in \R.

  The number of components to fit is specified with the argument
  \code{ncomp}.  It this is not supplied, the maximal number of
  components is used (taking account of any cross-validation).

  All implemented algorithms mean-center both predictor and response
  matrices.  This can be turned off by specifying \code{center = FALSE}.
  See Seasholtz and Kowalski for a discussion about centering in PLS
  regression.

  If \code{validation = "CV"}, cross-validation is performed.  The number and
  type of cross-validation segments are specified with the arguments
  \code{segments} and \code{segment.type}.  See \code{\link{mvrCv}} for
  details.  If \code{validation = "LOO"}, leave-one-out cross-validation
  is performed.  It is an error to specify the segments when
  \code{validation = "LOO"} is specified.

  By default, the cross-validation will be performed serially.  However,
  it can be done in parallel using functionality in the
  \code{\link{parallel}} package by setting the option \code{parallel} in
  \code{\link{pls.options}}.  See \code{\link{pls.options}} for the
  differnt ways to specify the parallelism.  See also Examples below.

  Note that the cross-validation is optimised for speed, and some
  generality has been sacrificed.  Especially, the model matrix is
  calculated only once for the complete cross-validation, so models like
  \code{y ~ msc(X)} will not be properly cross-validated.  However,
  scaling requested by \code{scale = TRUE} is properly cross-validated.
  For proper cross-validation of models where the model matrix must be
  updated/regenerated for each segment, use the separate function
  \code{\link{crossval}}.
}
\value{
  If \code{method = "model.frame"}, the model frame is returned.
  Otherwise, an object of class \code{mvr} is returned.
  The object contains all components returned by the underlying fit
  function.  In addition, it contains the following components:
  \item{validation}{if validation was requested, the results of the
    cross-validation.  See \code{\link{mvrCv}} for details.}
  \item{fit.time}{the elapsed time for the fit.  This is used by
    \code{\link{crossval}} to decide whether to turn on tracing.}
  \item{na.action}{if observations with missing values were removed,
    \code{na.action} contains a vector with their indices.  The
    class of this vector is used by functions like \code{fitted} to
    decide how to treat the observations.}
  \item{ncomp}{the number of components of the model.}
  \item{method}{the method used to fit the model.  See the argument
    \code{method} for possible values.}
  \item{scale}{if scaling was requested (with \code{scale}), the
    scaling used.}
  \item{call}{the function call.}
  \item{terms}{the model terms.}
  \item{model}{if \code{model = TRUE}, the model frame.}
  \item{x}{if \code{x = TRUE}, the model matrix.}
  \item{y}{if \code{y = TRUE}, the model response.}
}
\references{
  Martens, H., Næs, T. (1989) \emph{Multivariate calibration.}
  Chichester: Wiley.

  Seasholtz, M. B. and Kowalski, B. R. (1992) The effect of mean
  centering on prediction in multivariate calibration.  \emph{Journal of
  Chemometrics}, \bold{6}(2), 103--111.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{
  \code{\link{kernelpls.fit}},
  \code{\link{widekernelpls.fit}},
  \code{\link{simpls.fit}},
  \code{\link{oscorespls.fit}},
  \code{\link{cppls.fit}},
  \code{\link{svdpc.fit}},
  \code{\link{mvrCv}},
  \code{\link{crossval}},
  \code{\link[stats]{loadings}},
  \code{\link{scores}},
  \code{\link{loading.weights}},
  \code{\link{coef.mvr}},
  \code{\link{predict.mvr}},
  \code{\link{R2}},
  \code{\link{MSEP}},
  \code{\link{RMSEP}},
  \code{\link{plot.mvr}}}
\examples{
data(yarn)
## Default methods:
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.cppls <- cppls(density ~ NIR, 6, data = yarn, validation = "CV")

## Alternative methods:
yarn.oscorespls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                      method = "oscorespls")
yarn.simpls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                  method = "simpls")

\dontrun{
## Parallelised cross-validation, using transient cluster:
pls.options(parallel = 4) # use mclapply
pls.options(parallel = quote(makeCluster(4, type = "PSOCK"))) # use parLapply
## A new cluster is created and stopped for each cross-validation:
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")

## Parallelised cross-validation, using persistent cluster:
library(parallel)
## This creates the cluster:
pls.options(parallel = makeCluster(4, type = "PSOCK"))
## The cluster can be used several times:
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
## The cluster should be stopped manually afterwards:
stopCluster(pls.options()$parallel)

## Parallelised cross-validation, using persistent MPI cluster:
## This requires the packages snow and Rmpi to be installed
library(parallel)
## This creates the cluster:
pls.options(parallel = makeCluster(4, type = "MPI"))
## The cluster can be used several times:
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
## The cluster should be stopped manually afterwards:
stopCluster(pls.options()$parallel)
## It is good practice to call mpi.exit() or mpi.quit() afterwards:
mpi.exit()
}

## Multi-response models:
data(oliveoil)
sens.pcr <- pcr(sensory ~ chemical, ncomp = 4, scale = TRUE, data = oliveoil)
sens.pls <- plsr(sensory ~ chemical, ncomp = 4, scale = TRUE, data = oliveoil)

## Classification
# A classification example utilizing additional response information
# (Y.add) is found in the cppls.fit manual ('See also' above).
}
\keyword{regression}
\keyword{multivariate}
