### runit.options.R: test the pls.options() function
### By Bjørn-Helge Mevik
### Started 2013-07-30

## test.options: test calling pls::pls.options directly
test.options <- function() {
    ## Save original options for later restore:
    old_opts <- pls::pls.options()
    checkTrue(is.list(old_opts),
              "pls::pls.options() doesn't return a list")
    
    pls::pls.options(mvralg = "val1")
    checkEquals(pls::pls.options()$mvralg, "val1",
                "pls::pls.options(mvralg = 'val1') didn't set 'mvralg'")

    ## Restore the original options:
    pls::pls.options(old_opts)
}
