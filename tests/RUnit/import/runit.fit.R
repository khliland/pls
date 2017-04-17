### runit.fit.R: test fitting models
### By Bjørn-Helge Mevik
### Started 2013-07-12

## test.mvr: test calling pls::mvr directly
test.mvr <- function() {
    mvrmod <- pls::mvr(sensory ~ chemical, data = oliveoil)
    checkEquals(class(mvrmod), "mvr")
}

## test.plsr: test calling pls::plsr directly
test.plsr <- function() {
    plsrmod <- pls::plsr(sensory ~ chemical, data = oliveoil)
    checkEquals(class(plsrmod), "mvr")
}

## test.pcr: test calling pls::pcr directly
test.pcr <- function() {
    pcrmod <- pls::pcr(sensory ~ chemical, data = oliveoil)
    checkEquals(class(pcrmod), "mvr")
}
