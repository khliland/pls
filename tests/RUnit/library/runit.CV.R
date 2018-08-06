### runit.CV.R: test functions for checking cross-validation functions
### By Bjørn-Helge Mevik
### Started 2007-10-18

## Parallel computations, crossval:
test.crossval.parallel <- function() {
    ## Make sure we start with a `clean slate'
    pls.options(parallel = NULL)

    ## Create a baseline version to compare the parallel results with:
    plsmod <- plsr(octane ~ NIR, ncomp = 5, data = gasoline)
    serial <- crossval(plsmod, length.seg = 1, segment.type = "cons")

    ## mclapply:
    if (.Platform$OS.type != "windows") {
        pls.options(parallel = 2)
        parallel <- crossval(plsmod, length.seg = 1, segment.type = "cons")
        checkEquals(serial, parallel, "mclapply")
    }

    ## Auto-created FORK cluster:
    if (.Platform$OS.type != "windows") {
        pls.options(parallel = quote(parallel::makeCluster(2, type = "FORK")))
        parallel <- crossval(plsmod, length.seg = 1, segment.type = "cons")
        checkEquals(serial, parallel, "parLapply, auto-created FORK cluster")
    }

    ## Auto-created PSOCK cluster:
    pls.options(parallel = quote(parallel::makeCluster(2, type = "PSOCK")))
    parallel <- crossval(plsmod, length.seg = 1, segment.type = "cons")
    checkEquals(serial, parallel, "parLapply, auto-created PSOCK cluster")

    ## The rest of the tests use parallel functions directly:
    require(parallel)

    ## Permanent FORK cluster:
    if (.Platform$OS.type != "windows") {
        pls.options(parallel = makeCluster(2, type = "FORK"))
        parallel <- crossval(plsmod, length.seg = 1, segment.type = "cons")
        stopCluster(pls.options()$parallel)
        checkEquals(serial, parallel, "parLapply, permanent FORK cluster")
    }

    ## Permanent PSOCK cluster:
    pls.options(parallel = makeCluster(2, type = "PSOCK"))
    parallel <- crossval(plsmod, length.seg = 1, segment.type = "cons")
    stopCluster(pls.options()$parallel)
    checkEquals(serial, parallel, "parLapply, permanent PSOCK cluster")

    ## Clean up.  Note: this will not run if any check above fails:
    pls.options(parallel = NULL)
}

## Parallel computations, calling pls:::mvrCv directly:
test.mvrCv.parallel <- function() {
    ## Make sure we start with a `clean slate'
    pls.options(parallel = NULL)

    ## Create a baseline version to compare the parallel results with:
    serial <- pls:::mvrCv(gasoline$NIR, gasoline$octane, 5,
                          length.seg = 1, segment.type = "cons")

    ## mclapply:
    if (.Platform$OS.type != "windows") {
        pls.options(parallel = 2)
        parallel <- pls:::mvrCv(gasoline$NIR, gasoline$octane, 5,
                                length.seg = 1, segment.type = "cons")
        checkEquals(serial, parallel, "mclapply")
    }

    ## Auto-created FORK cluster:
    if (.Platform$OS.type != "windows") {
        pls.options(parallel = quote(parallel::makeCluster(2, type = "FORK")))
        parallel <- pls:::mvrCv(gasoline$NIR, gasoline$octane, 5,
                                length.seg = 1, segment.type = "cons")
        checkEquals(serial, parallel, "parLapply, auto-created FORK cluster")
    }

    ## Auto-created PSOCK cluster:
    pls.options(parallel = quote(parallel::makeCluster(2, type = "PSOCK")))
    parallel <- pls:::mvrCv(gasoline$NIR, gasoline$octane, 5,
                            length.seg = 1, segment.type = "cons")
    checkEquals(serial, parallel, "parLapply, auto-created PSOCK cluster")

    ## The rest of the tests use parallel functions directly:
    require(parallel)

    ## Permanent FORK cluster:
    if (.Platform$OS.type != "windows") {
        pls.options(parallel = makeCluster(2, type = "FORK"))
        parallel <- pls:::mvrCv(gasoline$NIR, gasoline$octane, 5,
                                length.seg = 1, segment.type = "cons")
        stopCluster(pls.options()$parallel)
        checkEquals(serial, parallel, "parLapply, permanent FORK cluster")
    }

    ## Permanent PSOCK cluster:
    pls.options(parallel = makeCluster(2, type = "PSOCK"))
    parallel <- pls:::mvrCv(gasoline$NIR, gasoline$octane, 5,
                            length.seg = 1, segment.type = "cons")
    stopCluster(pls.options()$parallel)
    checkEquals(serial, parallel, "parLapply, permanent PSOCK cluster")

    ## Clean up.  Note: this will not run if any check above fails:
    pls.options(parallel = NULL)
}


## A fake test function to make sure parallelism is turned off after
## the test.*.parallel tests:
test.parallel.cleanup <- function() {
    pls.options(parallel = NULL)
}
