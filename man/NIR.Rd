%% $Id$
\name{NIR}
\docType{data}
\alias{NIR}
\title{Multivariate Dataset of NIR Spectra}
\description{
  A training set consisting of 21 NIR spectra of PET yarns, measured at
  268 wavelengths, and 21 corresponding densities.  A test set of 7
  samples is also provided.  Many thanks to Erik Swierenga.
}
\usage{data(NIR)}
\format{A data frame with components
  \describe{
    \item{X}{Numeric matrix of NIR measurements}
    \item{y}{Numeric vector of densities}
    \item{train}{Logical vector with \code{TRUE} for the training
      samples and \code{FALSE} for the test samples}
  }
}
\source{
  Swierenga H., de Weijer A. P., van Wijk R. J., Buydens L. M. C. (1999)
  Strategy for constructing robust multivariate calibration models
  \emph{Chemometrics and Intelligent Laboratoryy Systems}, \bold{49}(1), 1--17.
}
\keyword{datasets}
