### mvr_wrappers.R: plsr, pls and pcr wrappers for mvr

#' @rdname mvr
#' @export
plsr <- function(..., method = pls.options()$plsralg) {
    cl <- match.call()
    cl$method <- match.arg(method, c("kernelpls", "widekernelpls", "simpls",
                                     "oscorespls", "model.frame"))
    cl[[1]] <- quote(pls::mvr)
    res <- eval(cl, parent.frame())
    ## Fix call component
    if (cl$method != "model.frame") res$call[[1]] <- as.name("plsr")
    if (missing(method)) res$call$method <- NULL
    res
}

#' @rdname mvr
#' @export
pcr <- function(..., method = pls.options()$pcralg) {
    cl <- match.call()
    cl$method <- match.arg(method, c("svdpc", "model.frame"))
    cl[[1]] <- quote(pls::mvr)
    res <- eval(cl, parent.frame())
    ## Fix call component
    if (cl$method != "model.frame") res$call[[1]] <- as.name("pcr")
    if (missing(method)) res$call$method <- NULL
    res
}

#' @rdname mvr
#' @export
cppls <- function(..., Y.add, weights, method = pls.options()$cpplsalg) {
    cl <- match.call()
    cl$method <- match.arg(method, c("cppls", "model.frame"))
    cl[[1]] <- quote(pls::mvr)
    res <- eval(cl, parent.frame())
    ## Fix call component
    if (cl$method != "model.frame") res$call[[1]] <- as.name("cppls")
    if (missing(method)) res$call$method <- NULL
    res
}
