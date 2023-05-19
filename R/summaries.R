### summaries.R: print and summary methods.

## Print method for mvr objects:
#' @rdname summary.mvr
#' @export
print.mvr <- function(x, ...) {
  switch(x$method,
         kernelpls = {
           ana = "Partial least squares regression"
           alg = "kernel"
         },
         widekernelpls = {
           ana = "Partial least squares regression"
           alg = "wide kernel"
         },
         simpls = {
           ana = "Partial least squares regression"
           alg = "simpls"
         },
         oscorespls = {
           ana = "Partial least squares regression"
           alg = "orthogonal scores"
         },
         cppls = {
           ana = "Canonical powered partial least squares"
           alg = "cppls"
         },
         svdpc = {
           ana = "Principal component regression"
           alg = "singular value decomposition"
         },
         stop("Unknown fit method.")
  )
  cat(ana, ", fitted with the ", alg, " algorithm.", sep = "")
  if (!is.null(x$validation))
    cat("\nCross-validated using", length(x$validation$segments),
        attr(x$validation$segments, "type"), "segments.")
  cat("\nCall:\n", deparse(x$call), "\n", sep = "")
  invisible(x)
}

## Summary method for mvr objects


#' @name summary.mvr
#' @title Summary and Print Methods for PLSR and PCR objects
#'
#' @description Summary and print methods for \code{mvr} and \code{mvrVal} objects.
#'
#' @details If \code{what} is \code{"training"}, the explained variances are given; if
#' it is \code{"validation"}, the cross-validated RMSEPs (if available) are
#' given; if it is \code{"all"}, both are given.
#'
#' @aliases summary.mvr print.mvr print.mvrVal
#' @param x,object an \code{mvr} object
#' @param what one of \code{"all"}, \code{"validation"} or \code{"training"}
#' @param digits integer.  Minimum number of significant digits in the output.
#' Default is 4.
#' @param print.gap Integer.  Gap between coloumns of the printed tables.
#' @param shortAlgs Logical.  Shorten algorithm names (default = TRUE).
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional Not used, only included to match signature of \code{as.data.frame}.
#' @param \dots Other arguments sent to underlying methods.
#' @return \code{print.mvr} and \code{print.mvrVal} return the object
#' invisibly.
#' @author Ron Wehrens and Bjørn-Helge Mevik
#' @seealso \code{\link{mvr}}, \code{\link{pcr}}, \code{\link{plsr}},
#' \code{\link{RMSEP}}, \code{\link{MSEP}}
#' @keywords regression multivariate
#' @examples
#'
#' data(yarn)
#' nir.mvr <- mvr(density ~ NIR, ncomp = 8, validation = "LOO", data = yarn)
#' nir.mvr
#' summary(nir.mvr)
#' RMSEP(nir.mvr)
#' # Extract MVR validation statistics as data.frame:
#' as.data.frame(RMSEP(nir.mvr, estimate = "CV"))
#' as.data.frame(R2(nir.mvr))
#'
#' @export
summary.mvr <- function(object, what = c("all", "validation", "training"),
                        digits = 4, print.gap = 2, ...)
{
  what <- match.arg(what)
  if (what == "all") what <- c("validation", "training")
  if (is.null(object$validation)) what <- "training"

  nobj <- nrow(object$scores)
  nresp <- length(object$Ymeans)
  yvarnames <- respnames(object)
  cat("Data: \tX dimension:", nobj, length(object$Xmeans),
      "\n\tY dimension:", nobj, nresp)
  cat("\nFit method:", object$method)
  cat("\nNumber of components considered:", object$ncomp)

  for (wh in what) {
    if (wh == "training") {
      cat("\nTRAINING: % variance explained\n")
      xve <- explvar(object)
      yve <- 100 * drop(R2(object, estimate = "train",
                           intercept = FALSE)$val)
      tbl <- rbind(cumsum(xve), yve)
      dimnames(tbl) <- list(c("X", yvarnames),
                            paste(1:object$ncomp, "comps"))
      print(tbl, digits = digits, print.gap = print.gap, ...)
    } else {
      cat("\n\nVALIDATION: RMSEP")
      cat("\nCross-validated using", length(object$validation$segments),
          attr(object$validation$segments, "type"), "segments.\n")
      print(RMSEP(object), digits = digits, print.gap = print.gap, ...)
    }
  }
}

## Print method for mvrVal objects:
#' @rdname summary.mvr
#' @export
print.mvrVal <- function(x, digits = 4, print.gap = 2, ...) {
  nresp <- dim(x$val)[2]
  yvarnames <- dimnames(x$val)[[2]]
  names(dimnames(x$val)) <- NULL
  for (i in 1:nresp) {
    if (nresp > 1) cat("\nResponse:", yvarnames[i], "\n")
    print(x$val[,i,], digits = digits, print.gap = print.gap, ...)
  }
  invisible(x)
}

#' @rdname summary.mvr
#' @export
as.data.frame.mvrVal <- function(x, row.names = NULL, optional = FALSE,
                                 shortAlgs = TRUE, ...){
  # mvrVal values
  mvrVals <- x$val

  # Reshape and add columns
  dnames <- dimnames(mvrVals)
  dims <- dim(mvrVals)
  obj <- data.frame(estimate = rep(dnames$estimate, dims[2]*dims[3]),
                    response = rep(dnames$response, dims[1]*dims[3]),
                    comps    = rep(x$comps, each = dims[1]*dims[2]))

  # Fitting method and validation type
  object <- eval(x$call$object, envir = parent.frame())
  fit <- as.character(object$call[[1]])
  val <- object$call$validation
  switch(object$method,
         kernelpls = {
           alg = "kernel"
         },
         widekernelpls = {
           if(shortAlgs)
             alg = "wide"
           else
             alg = "wide kernel"
         },
         simpls = {
           alg = "simpls"
         },
         oscorespls = {
           if(shortAlgs)
             alg = "orthScores"
           else
             alg = "orthogonal scores"
         },
         cppls = {
           alg = "cppls"
         },
         svdpc = {
           if(shortAlgs)
             alg = "svd"
           else
             alg = "singular value decomposition"
         },
         stop("Unknown fit method.")
  )
  obj$validation <- rep(val, prod(dims))
  obj$method <- rep(fit, prod(dims))
  obj$algorithm <- rep(alg, prod(dims))
  obj$value <- c(mvrVals)
  if(!is.null(row.names))
    rownames(obj) <- row.names
  obj
}
