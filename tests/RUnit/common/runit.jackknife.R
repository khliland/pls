### runit.jackknife.R: test functions for jackknifing
### By Bjørn-Helge Mevik
### Started 2007-08-02

## test.var.jack: Check dimensions of var.jack return value
test.var.jack <- function() {

    ## Single-response model, CV:
    npred <- ncol(gasoline$NIR)
    nresp <- 1
    mod <- mvr(octane ~ NIR, ncomp = 5, data = gasoline, validation = "CV",
                jackknife = TRUE)
    ## Dimensions
    checkEquals(dim(var.jack(mod)), c(npred, nresp, 1))
    checkEquals(dim(var.jack(mod, ncomp = 1:4)), c(npred, nresp, 4))
    ## Dimensions, with covariance
    checkEquals(dim(var.jack(mod, covariance = TRUE)),
                c(npred*nresp, npred*nresp, 1))
    checkEquals(dim(var.jack(mod, ncomp = 1:2, covariance = TRUE)),
                c(npred*nresp, npred*nresp, 2))

    ## Multi-response model, LOO:
    npred <- ncol(oliveoil$chemical)
    nresp <- ncol(oliveoil$sensory)
    mod <- mvr(sensory ~ chemical, data = oliveoil, validation = "LOO",
                jackknife = TRUE, ncomp = 4)
    ## Dimensions
    checkEquals(dim(var.jack(mod)), c(npred, nresp, 1))
    checkEquals(dim(var.jack(mod, ncomp = 1:4)), c(npred, nresp, 4))
    ## Dimensions, with covariance
    checkEquals(dim(var.jack(mod, covariance = TRUE)),
                c(npred*nresp, npred*nresp, 1))
    checkEquals(dim(var.jack(mod, ncomp = 1:2, covariance = TRUE)),
                c(npred*nresp, npred*nresp, 2))
}

## test.jack.test: Test output from jack.test
test.jack.test <- function() {

    ## Single-response model, CV:
    npred <- ncol(gasoline$NIR)
    nresp <- 1
    mod <- mvr(octane ~ NIR, ncomp = 6, data = gasoline, validation = "CV",
                jackknife = TRUE)
    ## Things to change:
    ## ncomp (== object$ncomp, length > 1)
    ## use.mean (FALSE/TRUE)
    for (nc in list(6, 4:5)) {
        for (use.mean in c(FALSE, TRUE)) {
            res <- if (length(nc) == 1)
                jack.test(mod, use.mean = use.mean)
            else
                jack.test(mod, ncomp = nc, use.mean = use.mean)
            ## Check for all finite values:
            checkTrue(all(sapply(res, function(x) all(is.finite(x)))),
                      paste("uniresp", length(nc), use.mean, "finite"))
        }
    }

    ## Multi-response model, LOO:
    npred <- ncol(oliveoil$chemical)
    nresp <- ncol(oliveoil$sensory)
    mod <- mvr(sensory ~ chemical, data = oliveoil, validation = "LOO",
                jackknife = TRUE, ncomp = 5)
    ## Things to change:
    ## ncomp (== object$ncomp, length > 1)
    ## use.mean (FALSE/TRUE)
    for (nc in list(6, 3:4)) {
        for (use.mean in c(FALSE, TRUE)) {
            res <- if (length(nc) == 1)
                jack.test(mod, use.mean = use.mean)
            else
                jack.test(mod, ncomp = nc, use.mean = use.mean)
            ## Check for all finite values:
            checkTrue(all(sapply(res, function(x) all(is.finite(x)))),
                      paste("uniresp", length(nc), use.mean, "finite"))
        }
    }
}
