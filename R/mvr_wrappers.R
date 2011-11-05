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

cppls <- function(..., Y.add, method = pls.options()$cpplsalg)
{
    cl <- match.call()
    cl$method <- match.arg(method, c("cppls", "model.frame"))
    cl[[1]] <- as.name("mvr")
    res <- eval(cl, parent.frame())
    ## Fix call component
    if (cl$method != "model.frame") res$call[[1]] <- as.name("cppls")
    if (missing(method)) res$call$method <- NULL
    res
}

plsda <- function(..., method = pls.options()$plsdaalg)
{
    cl <- match.call()
    cl$method <- match.arg(method, c("plsda", "model.frame"))
    cl[[1]] <- as.name("mvr")
    res <- eval(cl, parent.frame())
    ## Fix call component
    if (cl$method != "model.frame") res$call[[1]] <- as.name("plsda")
    if (missing(method)) res$call$method <- NULL
    res
}
