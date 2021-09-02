

#' Octane numbers and NIR spectra of gasoline
#'
#' A data set with NIR spectra and octane numbers of 60 gasoline samples.  The
#' NIR spectra were measured using diffuse reflectance as log(1/R) from 900 nm
#' to 1700 nm in 2 nm intervals, giving 401 wavelengths.  Many thanks to John
#' H. Kalivas.
#'
#'
#' @name gasoline
#' @docType data
#' @format A data frame with 60 observations on the following 2 variables.
#' \describe{ \item{octane}{a numeric vector.  The octane number.}
#' \item{NIR}{a matrix with 401 columns.  The NIR spectrum.} }
#' @source Kalivas, John H. (1997) Two Data Sets of Near Infrared Spectra
#' \emph{Chemometrics and Intelligent Laboratory Systems}, \bold{37}, 255--259.
#' @keywords datasets
NULL





#' NIR measurements and oil types of mayonnaise
#'
#' Raw NIR measurements (351 wavelengths, 1100-2500 nm in steps of 4 nm) taken
#' on 54 samples of mayonnaise based on six different oil types (soybean,
#' sunflower, canola, olive, corn, and grapeseed). The resulting 54 samples
#' were measured in triplicates, resulting in 54 x 3 = 162 different spectra
#' (120/42 training/test).
#'
#'
#' @name mayonnaise
#' @docType data
#' @format A data frame with 162 observations on the following 4 variables.
#' \describe{ \item{NIR}{a matrix with 351 columns}
#' \item{oil.type}{a numeric vector} \item{design}{a matrix
#' with 5 columns} \item{train}{a logical vector} }
#' @source Indahl U, Sahni NS, Kirkhus B, Næs T.  Multivariate strategies for
#' classification based on NIR-spectra-with application to mayonnaise.
#' Chemometr. Intell. Lab. Sys. 1999; 49: 19-31.
#' @keywords datasets
NULL





#' Sensory and physico-chemical data of olive oils
#'
#' A data set with scores on 6 attributes from a sensory panel and measurements
#' of 5 physico-chemical quality parameters on 16 olive oil samples.  The first
#' five oils are Greek, the next five are Italian and the last six are Spanish.
#'
#'
#' @name oliveoil
#' @docType data
#' @format A data frame with 16 observations on the following 2 variables.
#' \describe{ \item{sensory}{a matrix with 6 columns.  Scores for
#' attributes \sQuote{yellow}, \sQuote{green}, \sQuote{brown}, \sQuote{glossy},
#' \sQuote{transp}, and \sQuote{syrup}.} \item{chemical}{a matrix with
#' 5 columns.  Measurements of acidity, peroxide, K232, K270, and DK.} }
#' @source Massart, D. L., Vandeginste, B. G. M., Buydens, L. M. C., de Jong,
#' S., Lewi, P. J., Smeyers-Verbeke, J. (1998) \emph{Handbook of Chemometrics
#' and Qualimetrics: Part B}.  Elsevier. Tables 35.1 and 35.4.
#' @keywords datasets
NULL





#' NIR spectra and density measurements of PET yarns
#'
#' A training set consisting of 21 NIR spectra of PET yarns, measured at 268
#' wavelengths, and 21 corresponding densities.  A test set of 7 samples is
#' also provided.  Many thanks to Erik Swierenga.
#'
#'
#' @name yarn
#' @docType data
#' @format A data frame with components \describe{ \item{NIR}{Numeric matrix of
#' NIR measurements} \item{density}{Numeric vector of densities}
#' \item{train}{Logical vector with \code{TRUE} for the training samples and
#' \code{FALSE} for the test samples} }
#' @source Swierenga H., de Weijer A. P., van Wijk R. J., Buydens L. M. C.
#' (1999) Strategy for constructing robust multivariate calibration models
#' \emph{Chemometrics and Intelligent Laboratoryy Systems}, \bold{49}(1),
#' 1--17.
#' @keywords datasets
NULL



