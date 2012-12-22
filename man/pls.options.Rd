%% $Id$
\encoding{latin1}
\name{pls.options}
\alias{pls.options}
\title{Set or return options for the pls package}
\description{
  A function to set options for the \pkg{pls} package, or to return the
  current options.
}
\usage{
pls.options(\dots)
}
\arguments{
  \item{\dots}{a single list, a single character vector, or any number
    of named arguments (\var{name = value}).}
}
\details{
  If called with no arguments, or with an empty list as the single
  argument, \code{pls.options} returns the current options.

  If called with a character vector as the single argument, a list with
  the arguments named in the vector are returned.

  If called with a non-empty list as the single arguments, the list
  elements should be named, and are treated as named arguments to the
  function.

  Otherwise, \code{pls.options} should be called with one or more named
  arguments \var{name = value}.  For each argument, the option named
  \var{name} will be given the value \var{value}.

  The options are saved in a variable \code{.pls.Options} in the global
  environment, and remain in effect until the end of the session.  If
  the environment is saved upon exit, they will be remembered in the
  next session.  The `factory defaults' can be restored by removing
  \code{.pls.Options} from the global environment.

  The recognised options are:
  \describe{
    \item{mvralg}{The fit method to use in \code{\link{mvr}} and
      \code{\link{mvrCv}}.  The value should be one of the allowed
      methods.  Defaults to \code{"kernelpls"}.  Can be overridden with
      the argument \code{method} in \code{mvr} and \code{mvrCv}.}
    \item{pcralg}{The fit method to use in \code{\link{pcr}}.  The
      value should be one of the allowed methods.  Defaults to
      \code{"svdpc"}.  Can be overridden with the argument
      \code{method} in \code{pcr}.}
    \item{plsralg}{The fit method to use in \code{\link{plsr}}.  The
      value should be one of the allowed methods.  Defaults to
      \code{"kernelpls"}.  Can be overridden with the argument
      \code{method} in \code{plsr}.}
    \item{cpplsalg}{The fit method to use in \code{\link{cppls}}.  The
      value should be one of the allowed methods.  Defaults to
      \code{"cppls"}.  Can be overridden with the argument
      \code{method} in \code{cppls}.}
    \item{parallel}{Specification of how the cross-validation (CV) in
      \code{\link{mvr}} should be performed.  If it is \code{NULL}
      (default) or \code{1}, the CV is done serially.  If
      it is an integer greater than 1, the CV is done in parallel with
      the specified number of processes, using
      \code{\link[parallel]{mclapply}}.  If it is a cluster object
      created by \code{\link[parallel]{makeCluster}}, the CV is done in
      parallel on that cluster, using \code{\link[parallel]{parLapply}}.
      The user should stop the cluster herself when it is no longer
      needed, using \code{\link[parallel]{stopCluster}}.
      Finally, if it is an unevaluated call to
      \code{\link[parallel]{makeCluster}}, the call is evaluated, and
      the CV is done in parallel on the resulting cluster, using
      \code{\link[parallel]{parLapply}}.  In this case, the cluster will
      be stopped (with \code{\link[parallel]{stopCluster}}) after the
      CV.  Thus, in the final case, the cluster is created and
      destroyed for each CV, just like when using
      \code{\link[parallel]{mclapply}}.}
    \item{w.tol}{The tolerance used for removing values close to 0
	  in the vectors of loading weights in \code{\link{cppls}}.  Defaults to
      .Machine$double.eps.}
    \item{X.tol}{The tolerance used for removing predictor variables
      with L1 norms close to 0 in \code{\link{cppls}}.  Defaults to 10^-12.}
  }
}
\section{Side Effects}{
  If any named argument (or list element) was provided,
  \code{pls.options} updates the elements of the option list
  \code{.pls.Options} in the global environment.
}
\value{
  A list with the (possibly changed) options.  If any named argument (or
  list element) was provided, the list is returned invisibly.
}
\author{Bjørn-Helge Mevik and Ron Wehrens}
\note{
  The function is a slight modification of the function
  \code{\link[sm]{sm.options}} from the package \pkg{sm}.
}
\examples{
## Return current options:
pls.options()
pls.options("plsralg")
pls.options(c("plsralg", "pcralg"))

## Set options:
pls.options(plsralg = "simpls", mvralg = "simpls")
pls.options(list(plsralg = "simpls", mvralg = "simpls")) # Equivalent
pls.options()

## Restore `factory settings':
rm(.pls.Options)
pls.options()
}
\keyword{regression}
\keyword{multivariate}
