%% $Id$
\encoding{latin1}
\name{predict.mvr}
\alias{predict.mvr}
\title{Predict Method for PLSR and PCR}
\description{
  Prediction for MVR (PCR, PLSR) models.  New responses or scores are
  predicted using a fitted model and a new matrix of observations.
}
\usage{
\method{predict}{mvr}(object, newdata, comps = 1:object$ncomp,
        type = c("response", "scores"), cumulative = TRUE, \dots)
}
\arguments{
  \item{object}{an \code{mvr} object.  The fitted model}
  \item{newdata}{a data frame.  The new data.  If missing, the training
    data is used.}
  \item{comps}{vector of positive integers.  The components to use in
    the prediction.  See below.}
  \item{type}{character.  Whether to predict scores or response values}
  \item{cumulative}{logical.  How the elements of \code{comps} are
    interpreted.  Ignored if \code{type} is \samp{"scores"}.  See below}
  \item{\dots}{further arguments.  Currently not used}
}
\details{
  When \code{type} is \samp{"response"} (default), predicted response
  values are returned.  If \code{cumulative} is \code{TRUE}, the
  elements of \code{comps} are interpreted cumulatively,
  i.e. predictions for models with \code{comps[1]} components,
  \code{comps[2]} components, etc., are returned.  Otherwise, predicted
  response values for a single model with the exact components in
  \code{comps} are returned.

  When \code{type} is \samp{"scores"}, predicted score values are
  returned for the components given in \code{comps}.
}
\value{
  When \code{type} is \samp{"response"}, a three dimensional array of
  predicted response values is returned.  The dimensions correspond to
  the observations, the response variables and the model sizes,
  respectively.

  When \code{type} is \samp{"scores"}, a score matrix is returned.
}
\author{Ron Wehnrens and Bjørn-Helge Mevik}
\seealso{\code{\link{mvr}}, \code{\link{summary.mvr}},
  \code{\link{coef.mvr}}, \code{\link{plot.mvr}}}
\examples{
data(NIR)
nir.mvr <- mvr(y ~ X, ncomp = 5, data = NIR[NIR$train,])

## Predicted responses for models with 1, 2, 3 and 4 components
pred.resp <- predict(nir.mvr, comps = 1:4, newdata = NIR[!NIR$train,])

## Predicted responses for a single model with components 1, 2, 3, 4
predict(nir.mvr, comps = 1:4, cumulative = FALSE, newdata = NIR[!NIR$train,])

## Predicted scores
predict(nir.mvr, comps = 1:3, type = "scores", newdata = NIR[!NIR$train,])
}
\keyword{regression}
\keyword{multivariate}
