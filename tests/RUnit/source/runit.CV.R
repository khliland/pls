### runit.CV.R: test functions for checking cross-validation functions
### By Bjørn-Helge Mevik
### Started 2007-10-18

## Parallel computations, mvrCv:
test.mvrCv.parallel <- function() {
    ## Make sure we start with a `clean slate'
    pls.options(parallel = NULL)

    ## Create a baseline version to compare the parallel results with:
    serial <- mvrCv(gasoline$NIR, gasoline$octane, 5, length.seg = 1,
                    segment.type = "cons")

    ## mclapply:
    pls.options(parallel = 2)
    parallel <- mvrCv(gasoline$NIR, gasoline$octane, 5, length.seg = 1,
                      segment.type = "cons")
    checkEquals(serial, parallel, "mclapply")

    ## Auto-created FORK cluster:
    pls.options(parallel = quote(parallel::makeCluster(2, type = "FORK")))
    parallel <- mvrCv(gasoline$NIR, gasoline$octane, 5, length.seg = 1,
                      segment.type = "cons")
    checkEquals(serial, parallel, "parLapply, auto-created FORK cluster")

    ## Auto-created PSOCK cluster:
    pls.options(parallel = quote(parallel::makeCluster(2, type = "PSOCK")))
    parallel <- mvrCv(gasoline$NIR, gasoline$octane, 5, length.seg = 1,
                      segment.type = "cons")
    checkEquals(serial, parallel, "parLapply, auto-created PSOCK cluster")


    ## The rest of the tests use parallel functions directly:
    require(parallel)

    ## Permanent FORK cluster:
    pls.options(parallel = makeCluster(2, type = "FORK"))
    parallel <- mvrCv(gasoline$NIR, gasoline$octane, 5, length.seg = 1,
                      segment.type = "cons")
    stopCluster(pls.options()$parallel)
    checkEquals(serial, parallel, "parLapply, permanent FORK cluster")

    ## Permanent PSOCK cluster:
    pls.options(parallel = makeCluster(2, type = "PSOCK"))
    parallel <- mvrCv(gasoline$NIR, gasoline$octane, 5, length.seg = 1,
                      segment.type = "cons")
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
