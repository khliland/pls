### runit.algorithms.R: test functions for checking algorithm accurracy
### By Bjørn-Helge Mevik
### Started 2007-08-22

###
### Check that all algorithms give the same results
###

## test.plsrRes: check that the plsr algorithms give the same results
test.plsrRes <- function() {
    ## Load single response reference model (created with oscorespls):
    load("RUnit/common/ref_singresp.RData")
    oscmod <- mvr(form, nc = nc, data = mydata, method = "oscorespls")
    oscmod$method <- oscmod$call <- oscmod$fit.time <- NULL
    checkEquals(refmod, oscmod)
    kernmod <- mvr(form, nc = nc, data = mydata, method = "kernelpls")
    kernmod$method <- kernmod$call <- kernmod$fit.time <- NULL
    ## MSWin: ok with 49 comps, Linux/64: 47:
    ## oscores Yscores (5) have different scaling
    checkEquals(refmod[-5], kernmod[-5])
    widekernmod <- mvr(form, nc = nc, data = mydata, method = "widekernelpls")
    widekernmod$method <- widekernmod$call <- widekernmod$fit.time <- NULL
    widekernmod$nits <- NULL            # widekern also returns nits
    ## MSWin: ok with 44 comps, Linux/64: 43:
    ## oscores Yscores have different scaling
    checkEquals(refmod[-5], widekernmod[-5])
    ## MSWin: ok with 44 comps, Linux/64: 43:
    checkEquals(kernmod, widekernmod)
    simplsmod <- mvr(form, nc = nc, data = mydata, method = "simpls")
    simplsmod$method <- simplsmod$call <- simplsmod$fit.time <- NULL
    scaled.simplsmod <- scalecomps(simplsmod, scale = TRUE)
    ## MSWin: ok with 36 comps, Linux/64: 27:
    refmod$loading.weights <- NULL      # simpls doesn't give loading weights
    checkEquals(scalecomps(refmod, scale = TRUE), scaled.simplsmod)
    ## MSWin: ok with 36 comps, Linux/64: 27:
    kernmod$loading.weights <- NULL      # simpls doesn't give loading weights
    checkEquals(scalecomps(kernmod, scale = TRUE), scaled.simplsmod)
    ## MSWin: ok with 36 comps, Linux/64: 27:
    widekernmod$loading.weights <- NULL # simpls doesn't give loading weights
    widekernmod$nits <- NULL            # widekern also returns nits
    checkEquals(scalecomps(widekernmod, scale = TRUE), scaled.simplsmod)

    ## Multi-response model:
    ## Load multi response reference model (created with oscorespls):
    load("RUnit/common/ref_multiresp.RData")
    ## Note: the algs give sign differences, in general, for scores
    ## and loadings
    oscmod <- mvr(form, nc = nc, data = mydata, method = "oscorespls")
    oscmod$method <- oscmod$call <- oscmod$fit.time <-NULL
    for (i in 2:7) oscmod[[i]] <- abs(oscmod[[i]])
    checkEquals(refmod, oscmod)
    kernmod <- mvr(form, nc = nc, data = mydata, method = "kernelpls")
    kernmod$method <- kernmod$call <- kernmod$fit.time <- NULL
    for (i in 2:7) kernmod[[i]] <- abs(kernmod[[i]])
    ## MSWin: ok with 5 comps, Linux/64: 5:
    checkEquals(refmod, kernmod)
    widekernmod <- mvr(form, nc = nc, data = mydata, method = "widekernelpls")
    widekernmod$method <- widekernmod$call <- widekernmod$fit.time <- NULL
    widekernmod$nits <- NULL            # widekern also returns nits
    for (i in 2:7) widekernmod[[i]] <- abs(widekernmod[[i]])
    ## MSWin: ok with 4 comps, Linux/64: 4:
    checkEquals(refmod, widekernmod)
    ## MSWin: ok with 4 comps, Linux/64: 4:
    checkEquals(kernmod, widekernmod)

    ## SimPLS is supposed to give (slightly) different results for
    ## multi-response models, so we do not compare it
}
