%% $Id$
\encoding{UTF-8}
\name{yarn}
\alias{yarn}
\docType{data}
\title{NIR spectra and density measurements of PET yarns}
\description{
  A training set consisting of 21 NIR spectra of PET yarns, measured at
  268 wavelengths, and 21 corresponding densities.  A test set of 7
  samples is also provided.  Many thanks to Erik Swierenga.
}
\usage{yarn}
\format{A data frame with components
  \describe{
    \item{NIR}{Numeric matrix of NIR measurements}
    \item{density}{Numeric vector of densities}
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
