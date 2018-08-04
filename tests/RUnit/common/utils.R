### utils.R: Utilities for the test suites
### By Bjørn-Helge Mevik
### Started 2007-10-18

## scalecomps:  Utility function to scale and/or remove sign
## differences of all scores/loadings in a fit
scalecomps <- function(fit, scale = FALSE, sign = FALSE) {
    ## Matrices that are to be scaled and/or signed:
    for (nm in c("loadings", "scores", "loading.weights", "Yscores",
                 "Yloadings", "projection")) {
        if(!is.null(fit[[nm]])) {
            if (scale) {
                fit[[nm]] <- scale(fit[[nm]], center = FALSE)
                attr(fit[[nm]], "scaled:scale") <- NULL
            }
            if (sign) fit[[nm]] <- abs(fit[[nm]])
        }
    }
    fit
}
