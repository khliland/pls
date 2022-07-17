#' @title Calculate Variance-Covariance Matrix for a Fitted Model Object
#'
#' @description Returns the variance-covariance matrix of the coefficients
#' of a Principal Component Regression.
#'
#' @param object a fitted PCR object of class \code{mvr}.
#' @param ncomp number of principal components to estimate \code{vcov} for.
#' @param ... additional arguments (not used).
#'
#' @return A matrix of estimated covariances between regression coefficients.
#' @export
#'
#' @examples
#' data(yarn)
#' yarn.pcr <- pcr(density ~ NIR, 6, data = yarn)
#' vc <- vcov(yarn.pcr, 3)
#'
#' # Standard error of coefficients
#' se <- sqrt(diag(vc))
#' beta <- coef(yarn.pcr, ncomp = 3)
#'
#' # Plot regression coefficients with two standard errors shading.
#' plot(beta, type = 'l',
#'      panel.first = polygon(x = c(1:268, 268:1),
#'                            y = c(beta+2*se, rev(beta-2*se)),
#'                            col = 'lightblue',
#'                            border = NA))
vcov.mvr <- function(object, ncomp, ...){
  if(object$method != "svdpc")
    stop("For 'mvr' objects, only method 'svdpc' has a 'vcov' definition.")
  if(missing(ncomp)){
    ncomp <- object$ncomp
    warning(paste0("Missing parameter 'ncomp', defaulting to maximum: ", ncomp))
  }
  intercept <- ifelse(object$center, 1, 0)
  sigma2 <- sum(object$residuals[,,ncomp]^2)/(nrow(object$scores)-ncomp-intercept)
  vc <- sigma2 * object$loadings[,1:ncomp,drop=FALSE] %*% solve(crossprod(object$scores[,1:ncomp,drop=FALSE])) %*% t(object$loadings[,1:ncomp,drop=FALSE])
  return(vc)
}
