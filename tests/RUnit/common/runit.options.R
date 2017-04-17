### runit.options.R: tests for pls.options()
### By Bjørn-Helge Mevik
### Started 2013-07-30

test.options <- function() {
    ## Check results when called with allowed arguments:

    ## none
    ## Save original options for later restore:
    old_opts <- pls.options()
    checkTrue(is.list(old_opts),
              "pls.options() doesn't return a list")

    ## a single list
    checkTrue(is.list(pls.options(list())),
              "pls.options(list()) doesn't return a list")
    pls.options(list(mvralg = "val1"))
    checkEquals(pls.options()$mvralg, "val1",
                "pls.options(list(mvralg = 'val1')) didn't set 'mvralg'")
    pls.options(list(mvralg = "val2", plsralg = "val3"))
    checkEquals(pls.options()$mvralg, "val2",
        "pls.options(list(mvralg = 'val2', plsralg = 'val3')) didn't set 'mvralg'")
    checkEquals(pls.options()$plsralg, "val3",
        "pls.options(list(mvralg = 'val2', plsralg = 'val3')) didn't set 'plsralg'")

    ## a single character vector
    res <- pls.options("mvralg")
    checkTrue(is.list(res), "pls.options('mvralg') doesn't return a list")
    checkEquals(names(res), "mvralg",
                "pls.options('mvralg') doesn't return the correct option")
    res <- pls.options(c("mvralg", "plsralg"))
    checkTrue(is.list(res),
              "pls.options(c('mvralg', 'plsralg') doesn't return a list")
    checkEquals(names(res), c("mvralg", "plsralg"),
        "pls.options(c('mvralg', 'plsralg') doesn't return the correct options")

    ## any number of named arguments
    pls.options(mvralg = "val4")
    checkEquals(pls.options()$mvralg, "val4",
                "pls.options(mvralg = 'val4') didn't set 'mvralg'")
    pls.options(mvralg = "val5", plsralg = "val6")
    checkEquals(pls.options()$mvralg, "val5",
        "pls.options(mvralg = 'val5', plsralg = 'val6') didn't set 'mvralg'")
    checkEquals(pls.options()$plsralg, "val6",
        "pls.options(mvralg = 'val5', plsralg = 'val6')) didn't set 'plsralg'")

    ## Check handling invalid arguments
    checkException(pls.options(list("mvralg")),
        "pls.options(list('mvralg')) didn't throw an error")
    checkException(pls.options(list("mvralg", "plsralg")),
        "pls.options(list('mvralg', 'plsralg')) didn't throw an error")

    ## Restore the original options:
    pls.options(old_opts)
}
