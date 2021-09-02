### pls.options.R:  Package specific options mechanism.
###
### Implements a slightly modified version of the sm.options() as found in
### sm 2.1-0.  The difference is that the option list is stored in an
### environment '.pls.data'.

## The list of initial options:
.pls.data <- new.env(parent = emptyenv())
.pls.data$options <-
    list(mvralg = "kernelpls", plsralg = "kernelpls", cpplsalg = "cppls",
         pcralg = "svdpc", parallel = NULL,
         w.tol = .Machine$double.eps, X.tol = 10^-12)




#' @title Set or return options for the pls package
#'
#' @description A function to set options for the \pkg{pls} package, or to return the
#' current options.
#'
#' @details If called with no arguments, or with an empty list as the single argument,
#' \code{pls.options} returns the current options.
#'
#' If called with a character vector as the single argument, a list with the
#' arguments named in the vector are returned.
#'
#' If called with a non-empty list as the single argument, the list elements
#' should be named, and are treated as named arguments to the function.
#'
#' Otherwise, \code{pls.options} should be called with one or more named
#' arguments \var{name = value}.  For each argument, the option named
#' \var{name} will be given the value \var{value}.
#'
#' The recognised options are: \describe{ \item{mvralg}{The fit method to use
#' in \code{\link{mvr}} and \code{\link{mvrCv}}.  The value should be one of
#' the allowed methods.  Defaults to \code{"kernelpls"}.  Can be overridden
#' with the argument \code{method} in \code{mvr} and \code{mvrCv}.}
#' \item{pcralg}{The fit method to use in \code{\link{pcr}}.  The value should
#' be one of the allowed methods.  Defaults to \code{"svdpc"}.  Can be
#' overridden with the argument \code{method} in \code{pcr}.}
#' \item{plsralg}{The fit method to use in \code{\link{plsr}}.  The value
#' should be one of the allowed methods.  Defaults to \code{"kernelpls"}.  Can
#' be overridden with the argument \code{method} in \code{plsr}.}
#' \item{cpplsalg}{The fit method to use in \code{\link{cppls}}.  The value
#' should be one of the allowed methods.  Defaults to \code{"cppls"}.  Can be
#' overridden with the argument \code{method} in \code{cppls}.}
#' \item{parallel}{Specification of how the cross-validation (CV) in
#' \code{\link{mvr}} should be performed.  If the specification is \code{NULL}
#' (default) or \code{1}, the CV is done serially, otherwise it is done in
#' parallel using functionality from the \code{\link{parallel}} package.
#'
#' If it is an integer greater than 1, the CV is done in parallel with the
#' specified number of processes, using \code{\link{mclapply}}.
#'
#' If it is a cluster object created by \code{\link{makeCluster}}, the CV is
#' done in parallel on that cluster, using \code{\link{parLapply}}.  The user
#' should stop the cluster herself when it is no longer needed, using
#' \code{\link{stopCluster}}.
#'
#' Finally, if the specification is an unevaluated call to
#' \code{\link{makeCluster}}, the call is evaluated, and the CV is done in
#' parallel on the resulting cluster, using \code{\link{parLapply}}.  In this
#' case, the cluster will be stopped (with \code{\link{stopCluster}}) after the
#' CV.  Thus, in the final case, the cluster is created and destroyed for each
#' CV, just like when using \code{\link{mclapply}}.} \item{w.tol}{The tolerance
#' used for removing values close to 0 in the vectors of loading weights in
#' \code{\link{cppls}}.  Defaults to .Machine$double.eps.} \item{X.tol}{The
#' tolerance used for removing predictor variables with L1 norms close to 0 in
#' \code{\link{cppls}}.  Defaults to 10^-12.} }
#'
#' @param \dots a single list, a single character vector, or any number of
#' named arguments (\var{name = value}).
#' @return A list with the (possibly changed) options.  If any named argument
#' (or list element) was provided, the list is returned invisibly.
#' @note The function is a slight modification of the function
#' \code{\link[sm]{sm.options}} from the package \pkg{sm}.
#' @author Bjørn-Helge Mevik and Ron Wehrens
#' @keywords regression multivariate
#' @examples
#'
#' ## Return current options:
#' pls.options()
#' pls.options("plsralg")
#' pls.options(c("plsralg", "pcralg"))
#'
#' ## Set options:
#' pls.options(plsralg = "simpls", mvralg = "simpls")
#' pls.options(list(plsralg = "simpls", mvralg = "simpls")) # Equivalent
#' pls.options()
#'
#' ## Restore `factory settings':
#' pls.options(list(mvralg = "kernelpls", plsralg = "kernelpls", cpplsalg = "cppls",
#'                  pcralg = "svdpc", parallel = NULL,
#'                  w.tol = .Machine$double.eps, X.tol = 10^-12))
#' pls.options()
#'
#' @export
pls.options <- function(...) {
    if (nargs() == 0) return(.pls.data$options)
    current <- .pls.data$options
    temp <- list(...)
    if (length(temp) == 1 && is.null(names(temp))) {
        arg <- temp[[1]]
        switch(mode(arg),
               list = temp <- arg,
               character = return(.pls.data$options[arg]),
               stop("invalid argument: ", sQuote(arg)))
    }
    if (length(temp) == 0) return(current)
    n <- names(temp)
    if (is.null(n)) stop("options must be given by name")
    changed <- current[n]
    current[n] <- temp
    .pls.data$options <- current
    invisible(current)
}
