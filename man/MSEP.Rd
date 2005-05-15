%% $Id$
\encoding{latin1}
\name{MSEP}
\alias{MSEP}
\alias{RMSEP}
\alias{R2}
\title{MSEP, RMSEP and R2 of PLSR and PCR models}
\description{
  Functions to estimate the mean squared error of prediction (MSEP),
  root mean squared error of prediction (RMSEP) and \eqn{R^2} for fitted
  PCR and PLSR models.  Test-set, cross-validation and calibration-set
  estimates are implemented.
}
\usage{
MSEP(object, estimate, newdata, comps = 1:object$ncomp,
     cumulative = TRUE, intercept = cumulative, se = FALSE, \dots)
RMSEP(\dots)
R2(object, estimate, newdata, comps = 1:object$ncomp,
   cumulative = TRUE, intercept = cumulative, se = FALSE, \dots)
}
\arguments{
  \item{object}{an \code{mvr} object}
  \item{estimate}{a character vector.  Which estimators to use.
    Should be a subset of \code{c("all", "train", "CV", "adjCV",
      "test")}.  \code{"adjCV"} is only available for (R)MSEP.  See
    below for how the estimators are chosen.}
  \item{newdata}{a data frame with test set data.}
  \item{comps}{a vector of positive integers.  The components or number
    of components to use.  See below.}
  \item{cumulative}{logical.   See below.}
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

  Which estimators to use is decided as follows.  If
  \code{estimate} is not specified, the test set estimate is returned if
  \code{newdata} is specified, otherwise the CV and adjusted CV (for
  \code{RMSEP} and \code{MSEP})
  estimates if the model has been cross-validated, otherwise the
  training data estimate.  If \code{estimate} is \code{"all"}, all
  possible estimates are calculated.  Otherwise, the specified estimates
  are calculated.

  Several model sizes can also be specified.  If \code{cumulative} is
  \code{TRUE} (default), \code{length(comps)} models are used, with
  \code{comps[1]} components, \ldots, \code{comps[length(comps)]}
  components.  Otherwise, a single model with the components
  \code{comps[1]}, \ldots, \code{comps[length(comps)]} is used.

  If \code{intercept} is \code{TRUE}, a model with zero components is
  also used (in addition to the above).  For \code{R2}, this is simply
  defined as 0.
}
\value{
  An object of class \code{"mvrVal"}, with components
  \item{val}{three-dimensional array of estimates.  The first dimension
    is the different estimators, the second is the response variables
    and the third is the models.}
  \item{type}{\code{"MSEP"}, \code{"RMSEP"} or \code{"R2"}.}
  \item{comps}{the components specified, with \code{0} prepended if
    \code{intercept} is \code{TRUE}.}
  \item{call}{the function call}
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
data(sensory)
mod <- plsr(Panel ~ Quality, ncomp = 4, data = sensory, validation = "LOO")
RMSEP(mod)
\dontrun{plot(R2(mod))}
}
\keyword{regression}
\keyword{multivariate}
