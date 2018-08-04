%% $Id$
\encoding{UTF-8}
\name{gasoline}
\alias{gasoline}
\docType{data}
\title{Octane numbers and NIR spectra of gasoline}
\description{
  A data set with NIR spectra and octane numbers of 60 gasoline samples.
  The NIR spectra were measured using diffuse reflectance as log(1/R)
  from 900 nm to 1700 nm in 2 nm intervals, giving 401 wavelengths.
  Many thanks to John H. Kalivas.
}
\usage{gasoline}
\format{
  A data frame with 60 observations on the following 2 variables.
  \describe{
    \item{\code{octane}}{a numeric vector.  The octane number.}
    \item{\code{NIR}}{a matrix with 401 columns.  The NIR spectrum.}
  }
}
\source{
  Kalivas, John H. (1997)
  Two Data Sets of Near Infrared Spectra
  \emph{Chemometrics and Intelligent Laboratory Systems}, \bold{37}, 255--259.
}
\keyword{datasets}
