### mvr_wrappers.R: plsr, pls and pcr wrappers for mvr
### $Id$

plsr <- function(..., method = c("kernelpls", "simpls", "oscorespls"))
{
    cl <- match.call()
    cl$method <- match.arg(method)
    cl[[1]] <- as.name("mvr")
    res <- eval(cl, parent.frame())
    res$call <- match.call()            # Fix call component
    res
}

pls <- plsr

pcr <- plsr
formals(pcr)$method <- "svdpc"
