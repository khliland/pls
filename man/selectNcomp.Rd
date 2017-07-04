\name{selectNcomp}
\alias{selectNcomp}
\title{Suggestions for the optimal number of components in PCR and PLSR models}
\description{
  Choosing the best number of components in PCR and PLSR models is difficult
  and usually done on the basis of visual inspection of the validation
  plots. In cases where large numbers of models are built this choice
  needs to be automated. This function implements two proposals, one
  based on randomization (permutation) testing, and an
  approach based on the standard error of the cross-validation residuals.
}
\usage{
selectNcomp(object, method = c("randomization", "onesigma"),
            nperm = 999, alpha = 0.01, ncomp = object$ncomp,
            plot = FALSE, ...)
}
\arguments{
  \item{object}{an \code{mvr} object.  The fitted model. It should
    contain a \code{validation} element.}
  \item{method}{character string, indicating the heuristic to use.}
  \item{nperm}{number of permutations in the \code{"randomization"}
    approach - not used in the \code{"onesigma"} approach.}
  \item{alpha}{cutoff for p values in the \code{"randomization"}
    approach - not used in the \code{"onesigma"} approach.}
  \item{ncomp}{maximum number of components to consider when determining
    the global minimum in the cross-validation curve.}
  \item{plot}{whether or not to show a cross-validation plot. The plot
    for the \code{"randomization"} approach shows models that do not differ
    significantly from the global RMSEP minimum with open circles; the
    \code{"onesigma"} approach shows the 
    one-sigma bands around the RMSEP values. In both cases, the
    selection is indicated with a blue dashed line.}
  \item{\dots}{Further plotting arguments, e.g., to add a title to the
    plot, or to limit the plotting range.}
}
\details{In both approaches the results of cross-validation are used, so
  the model should have been calculated with some form of
  cross-validation. First, the absolute minimum in the CV curve is
  determined (considering only the first ncomp components), leading to
  the reference model. The randomization test approach (Van der Voet, 1994)
  checks whether the squared prediction errors of models with fewer
  components are significantly larger than in the reference model. This
  leads for each model considered to a \eqn{p} value; the smallest model not
  significantly worse than the reference model is returned as the
  selected one. 

  The approach \code{"onesigma"} simply returns the first model where the
  optimal CV is within one standard error of the absolute optimum
  (Hastie, Tibshirani and Friedman, 2009). Note that here we
  simply use the standard deviation of the cross-validation
  residuals, in line with the procedure used to calculate the error
  measure itself. Some other packages implementing similar procedures
  (such as \code{glmnet}) calculate an error measure for each validation
  segment separately and use the average as the final estimate. In such
  cases the standard error across segments is the relevant measure of
  spread. For LOO, the two procedures are identical. In other forms of
  validation, small differences will occur.
}
\value{A number indicating the suggested number of components in the
  model.}

\references{
  Van der Voet, H. (1994) Comparing the predictive accuracy of models
  using a simple randomization test. Chemom. Intell. Lab. Syst. 25 (2),
  313-323

  Hastie, T., Friedman, J. and Tibshirani, R. The Elements of
  Statistical Learning: data mining, inference, and prediction,
  Springer (2013), 10th printing with corrections, paragraph 7.10.
}
\author{Ron Wehrens, Hilko van der Voet and Gerie van der Heijden}
\seealso{\code{\link{mvr}}, \code{\link{summary.mvr}}}
\examples{
data(yarn)
yarn.pls <- plsr(density ~ NIR, data = yarn, scale = TRUE,
                 ncomp = 20, validation = "LOO") 
selectNcomp(yarn.pls, "onesigma", plot = TRUE, ylim = c(0, 3))
selectNcomp(yarn.pls, "randomization", plot = TRUE)
selectNcomp(yarn.pls, "randomization", plot = TRUE,
            ncomp = 10, ylim = c(0, 3))
}
\keyword{regression}
\keyword{multivariate}
