%% $Id$
\encoding{UTF-8}
\name{mvrVal}
\alias{MSEP}
\alias{MSEP.mvr}
\alias{RMSEP}
\alias{RMSEP.mvr}
\alias{R2}
\alias{R2.mvr}
\alias{mvrValstats}
\title{MSEP, RMSEP and R2 of PLSR and PCR models}
\description{
  Functions to estimate the mean squared error of prediction (MSEP),
  root mean squared error of prediction (RMSEP) and \eqn{R^2}
  (A.K.A. coefficient of multiple determination) for fitted
  PCR and PLSR models.  Test-set, cross-validation and calibration-set
  estimates are implemented.
}
\usage{
MSEP(object, ...)
\method{MSEP}{mvr}(object, estimate, newdata, ncomp = 1:object$ncomp, comps,
     intercept = cumulative, se = FALSE, \dots)

RMSEP(object, ...)
\method{RMSEP}{mvr}(object, ...)

R2(object, ...)
\method{R2}{mvr}(object, estimate, newdata, ncomp = 1:object$ncomp, comps,
   intercept = cumulative, se = FALSE, \dots)

mvrValstats(object, estimate, newdata, ncomp = 1:object$ncomp, comps,
            intercept = cumulative, se = FALSE, \dots)
}
\arguments{
  \item{object}{an \code{mvr} object}
  \item{estimate}{a character vector.  Which estimators to use.
    Should be a subset of \code{c("all", "train", "CV", "adjCV",
      "test")}.  \code{"adjCV"} is only available for (R)MSEP.  See
    below for how the estimators are chosen.}
  \item{newdata}{a data frame with test set data.}
  \item{ncomp, comps}{a vector of positive integers.  The components or number
    of components to use.  See below.}
  \item{intercept}{logical.  Whether estimates for a model with zero
    components should be returned as well.}
  \item{se}{logical.  Whether estimated standard errors of the estimates
    should be calculated.  Not implemented yet.}
  \item{\dots}{further arguments sent to underlying functions or (for
    \code{RMSEP}) to \code{MSEP}}
}
\details{
  \code{RMSEP} simply calls \code{MSEP} and takes the square root of the
  estimates.  It therefore accepts the same arguments as \code{MSEP}.

  Several estimators can be used.  \code{"train"} is the training
  or calibration data estimate, also called (R)MSEC.  For \code{R2},
  this is the unadjusted \eqn{R^2}.  It is
  overoptimistic and should not be used for assessing models.
  \code{"CV"} is the cross-validation estimate, and \code{"adjCV"} (for
  \code{RMSEP} and \code{MSEP}) is
  the bias-corrected cross-validation estimate.  They can only be
  calculated if the model has been cross-validated.
  Finally, \code{"test"} is the test set estimate, using \code{newdata}
  as test set.

  Which estimators to use is decided as follows (see below for
  \code{mvrValstats}).  If
  \code{estimate} is not specified, the test set estimate is returned if
  \code{newdata} is specified, otherwise the CV and adjusted CV (for
  \code{RMSEP} and \code{MSEP})
  estimates if the model has been cross-validated, otherwise the
  training data estimate.  If \code{estimate} is \code{"all"}, all
  possible estimates are calculated.  Otherwise, the specified estimates
  are calculated.

  Several model sizes can also be specified.  If \code{comps} is missing
  (or is \code{NULL}), \code{length(ncomp)} models are used, with
  \code{ncomp[1]} components, \ldots, \code{ncomp[length(ncomp)]}
  components.  Otherwise, a single model with the components
  \code{comps[1]}, \ldots, \code{comps[length(comps)]} is used.
  If \code{intercept} is \code{TRUE}, a model with zero components is
  also used (in addition to the above).

  The \eqn{R^2} values returned by \code{"R2"} are calculated as \eqn{1
    - SSE/SST}, where \eqn{SST} is the (corrected) total sum of squares
  of the response, and \eqn{SSE} is the sum of squared errors for either
  the fitted values (i.e., the residual sum of squares), test set
  predictions or cross-validated predictions (i.e., the \eqn{PRESS}).
  For \code{estimate = "train"}, this is equivalent to the squared
  correlation between the fitted values and the response.  For
  \code{estimate = "train"}, the estimate is often called the prediction
  \eqn{R^2}.
  
  \code{mvrValstats} is a utility function that calculates the
  statistics needed by \code{MSEP} and \code{R2}.  It is not intended to
  be used interactively.  It accepts the same arguments as \code{MSEP}
  and \code{R2}.  However, the \code{estimate} argument must be
  specified explicitly: no partial matching and no automatic choice is
  made.  The function simply calculates the types of estimates it knows,
  and leaves the other untouched.
}
%\value{
\section{Value}{
  \code{mvrValstats} returns a list with components
  \describe{
  \item{SSE}{three-dimensional array of SSE values.  The first dimension
    is the different estimators, the second is the response variables
    and the third is the models.}
  \item{SST}{matrix of SST values.  The first dimension
    is the different estimators and the second is the response
    variables.}
  \item{nobj}{a numeric vector giving the number of objects used for
    each estimator.}
  \item{comps}{the components specified, with \code{0} prepended if
    \code{intercept} is \code{TRUE}.}
  \item{cumulative}{\code{TRUE} if \code{comps} was \code{NULL} or not
    specified.}
  }

  The other functions return an object of class \code{"mvrVal"}, with
  components
  \describe{
  \item{val}{three-dimensional array of estimates.  The first dimension
    is the different estimators, the second is the response variables
    and the third is the models.}
  \item{type}{\code{"MSEP"}, \code{"RMSEP"} or \code{"R2"}.}
  \item{comps}{the components specified, with \code{0} prepended if
    \code{intercept} is \code{TRUE}.}
  \item{cumulative}{\code{TRUE} if \code{comps} was \code{NULL} or not
    specified.}
  \item{call}{the function call}
  }
}
\references{
  Mevik, B.-H., Cederkvist, H. R. (2004) Mean Squared Error of
  Prediction (MSEP) Estimates for Principal Component Regression (PCR)
  and Partial Least Squares Regression (PLSR).
  \emph{Journal of Chemometrics}, \bold{18}(9), 422--429.
}
\author{Ron Wehrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{crossval}}, \code{\link{mvrCv}},
  \code{\link{validationplot}}, \code{\link{plot.mvrVal}}}
\examples{
data(oliveoil)
mod <- plsr(sensory ~ chemical, ncomp = 4, data = oliveoil, validation = "LOO")
RMSEP(mod)
\dontrun{plot(R2(mod))}
}
\keyword{regression}
\keyword{multivariate}
