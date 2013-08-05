### pls.options.R:  Package specific options mechanism.
###
### $Id$
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
