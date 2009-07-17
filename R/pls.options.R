### pls.options.R:  Package specific options mechanism.
###
### $Id$
###
### Implements a slightly modified version of the sm.options() as found in
### sm 2.1-0.

## The list of initial options:
.pls.Options <-
    list(mvralg = "kernelpls", plsralg = "kernelpls", pcralg = "svdpc")


pls.options <- function(...) {
    ## Use modified version, if exists.  (Needed due to Namespace.)
    if (exists(".pls.Options", where = .GlobalEnv)) {
        .pls.Options <- get(".pls.Options", pos = .GlobalEnv)
    }
    if (nargs() == 0) return(.pls.Options)
    current <- .pls.Options
    temp <- list(...)
    if (length(temp) == 1 && is.null(names(temp))) {
        arg <- temp[[1]]
        switch(mode(arg),
               list = temp <- arg,
               character = return(.pls.Options[arg]),
               stop("invalid argument: ", sQuote(arg)))
    }
    if (length(temp) == 0) return(current)
    n <- names(temp)
    if (is.null(n)) stop("options must be given by name")
    changed <- current[n]
    current[n] <- temp
    ## This assigns .pls.Options in the global environment.  That way one
    ## can get back to the `factory defaults' by removing the variable from
    ## the global environment.  It also means that options are remembered
    ## between sessions (if the environment is saved).  Except for renaming
    ## .sm.Options to .pls.Options, this is the only modification of the
    ## function:
    assign(".pls.Options", current, pos = .GlobalEnv)
    invisible(current)
}
