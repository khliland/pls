### mvr_wrappers.R: plsr, pls and pcr wrappers for mvr
### $Id$

plsr <- function(..., method = pls.options()$plsralg)
{
    cl <- match.call()
    cl$method <- match.arg(method, c("kernelpls", "widekernelpls", "simpls",
                                     "oscorespls", "model.frame"))
    cl[[1]] <- as.name("mvr")
    res <- eval(cl, parent.frame())
    res$call <- match.call()            # Fix call component
    res
}

pcr <- function(..., method = pls.options()$pcralg)
{
    cl <- match.call()
    cl$method <- match.arg(method, c("svdpc", "model.frame"))
    cl[[1]] <- as.name("mvr")
    res <- eval(cl, parent.frame())
    res$call <- match.call()            # Fix call component
    res
}

## FIXME: Perhaps use a pls.options()$cppls default?
cpplsr <- function(..., Y.sec, method = c("cppls", "model.frame"))
{
    cl <- match.call()
    cl$method <- match.arg(method)
    cl[[1]] <- as.name("mvr")
    res <- eval(cl, parent.frame())
    ## Fix call component
    if (cl$method != "model.frame") res$call[[1]] <- as.name("cpplsr")
    if (missing(method)) res$call$method <- NULL
    res
}
