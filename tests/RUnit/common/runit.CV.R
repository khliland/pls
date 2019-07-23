### runit.CV.R: test functions for checking cross-validation functions
### By Bjørn-Helge Mevik
### Started 2007-10-18


## Basic cross-validation:
test.CV <- function() {
    ##  1 resp, 5-fold, specified # comps
    cvmod1 <- mvr(density ~ NIR, ncomp = 10, data = yarn, validation = "CV",
                   segments = 5, segment.type = "interleaved")
    omod <- mvr(density ~ NIR, ncomp = 10, data = yarn)
    cvmod2 <- crossval(omod, segments = 5, segment.type = "interleaved")
    ## Check validation component:
    checkEquals(cvmod1$validation, cvmod2$validation, "1 resp, validation")
    ## Remove components that will not match or have been tested:
    cvmod1$call <- cvmod2$call <- NULL
    cvmod1$fit.time <- cvmod2$fit.time <- NULL
    cvmod1$validation <- cvmod2$validation <- NULL
    checkEquals(cvmod1, cvmod2, "1 resp, the rest")

    ## 5 resps, LOO, unspecified # comps, without standardisation
    cvmod1 <- mvr(sensory ~ chemical, data = oliveoil,
                   validation = "LOO")
    omod <- mvr(sensory ~ chemical, data = oliveoil)
    cvmod2 <- crossval(omod, length.seg = 1)
    ## Check validation component:
    cvmod1$validation$segments <- cvmod2$validation$segments <- NULL # Segment
                                        # order and attributes will not match
    checkEquals(cvmod1$validation, cvmod2$validation, "5 resps, validation")
    ## Remove components that will not match or have been tested:
    cvmod1$call <- cvmod2$call <- NULL
    cvmod1$fit.time <- cvmod2$fit.time <- NULL
    cvmod1$validation <- cvmod2$validation <- NULL
    checkEquals(cvmod1, cvmod2, "5 resps, the rest")

    ## 5 resps, LOO, unspecified # comps, with standardisation
    cvmod1 <- mvr(sensory ~ chemical, data = oliveoil,
                   validation = "LOO", scale = TRUE)
    omod <- mvr(sensory ~ stdize(chemical), data = oliveoil)
    cvmod2 <- crossval(omod, length.seg = 1)
    ## Check the scaling used:
    checkEquals(cvmod1$scale, attr(model.frame(cvmod2)[[2]], "stdized:scale"),
               "5 resps, std, scale")
    ## Check the validation component:
    cvmod1$validation$segments <- cvmod2$validation$segments <- NULL # Segment
                                        # order and attributes will not match
    checkEquals(cvmod1$validation, cvmod2$validation,
                "5 resps, std, validation")
    ## Remove components that will not match or have been tested:
    cvmod1$call <- cvmod2$call <- NULL      # The calls will not match
    cvmod1$Xmeans <- cvmod2$Xmeans <- NULL  # cvmod2$Xmeans == 0
    cvmod1$terms <- cvmod2$terms <- NULL
    cvmod1$model <- cvmod2$model <- NULL
    cvmod1$scale <- NULL                    # crossval() doesn't add scale
    cvmod1$fit.time <- cvmod2$fit.time <- NULL
    cvmod1$validation <- cvmod2$validation <- NULL
    cvmod1 <- scalecomps(cvmod1, sign = TRUE) # Remove sign differences
    cvmod2 <- scalecomps(cvmod2, sign = TRUE) # Remove sign differences
    checkTrue(all.equal(cvmod1, cvmod2, check.attributes = FALSE),
              "5 resps, std, the rest")

    ## 5 resps, LOO, unspecified # comps, without centering
    cvmod1 <- mvr(sensory ~ chemical, data = oliveoil,
                   validation = "LOO", center = FALSE)
    omod <- mvr(sensory ~ chemical, data = oliveoil, center = FALSE)
    cvmod2 <- crossval(omod, length.seg = 1)
    ## Check the validation component:
    cvmod1$validation$segments <- cvmod2$validation$segments <- NULL # Segment
                                        # order and attributes will not match
    checkEquals(cvmod1$validation, cvmod2$validation,
                "5 resps, uncent, validation")
    ## Remove components that will not match or have been tested:
    cvmod1$call <- cvmod2$call <- NULL      # The calls will not match
    cvmod1$fit.time <- cvmod2$fit.time <- NULL
    cvmod1$validation <- cvmod2$validation <- NULL
    checkTrue(all.equal(cvmod1, cvmod2, check.attributes = FALSE),
              "5 resps, uncent, the rest")
}

## Test cvsegments()
test.cvsegments <- function() {
    ## Data set without replicates (nrep = 1)
    N <- 10
    ok_segs <- list(random = list(), consecutive = list(), interleaved = list())
    ok_segs[[c("consecutive", 1)]] <- list(1:10)
    ok_segs[[c("consecutive", 2)]] <- list(1:5, 6:10)
    ok_segs[[c("consecutive", 3)]] <- list(1:4, 5:7, 8:10)
    ok_segs[[c("interleaved", 1)]] <- list(1:10)
    ok_segs[[c("interleaved", 2)]] <- list(c(1,3,5,7,9), c(2,4,6,8,10))
    ok_segs[[c("interleaved", 3)]] <- list(c(1,4,7,10), c(2,5,8), c(3,6,9))
    for (type in c("random", "consecutive", "interleaved")) {
        for (k in c(1,2,3)) {
            segs <- cvsegments(N = N, k = k, type = type)
            test_id <- paste("N = ", N, ", k = ", k, ", type = ", type)
            if (type == "random") {
                switch(as.character(k),
                       "1" = {
                           checkTrue(all.equal(1:N, sort(segs[[1]]),
                                               check.attributes = FALSE),
                                     test_id)
                       },
                       "2" = {
                           checkTrue(
                               length(segs) == k &&
                               all(sapply(segs, length) == N/k) &&
                               length(intersect(segs[[1]], segs[[2]])) == 0,
                               test_id)
                       },
                       "3" = {
                           checkTrue(
                               length(segs) == k &&
                               all(sapply(segs, length) == c(4, 3, 3)) &&
                               length(intersect(segs[[1]], segs[[2]])) == 0 &&
                               length(intersect(segs[[1]], segs[[3]])) == 0 &&
                               length(intersect(segs[[2]], segs[[3]])) == 0,
                               test_id)
                       }
                       )
            } else {
                checkTrue(all.equal(ok_segs[[c(type, k)]], segs,
                                    check.attributes = FALSE),
                          test_id)
            }
        }
    }

    ## Data set with replicates (nrep > 1)
    N <- 20
    nrep <- 2
    ok_segs <- list(random = list(), consecutive = list(), interleaved = list())
    ok_segs[[c("consecutive", 1)]] <- list(1:20)
    ok_segs[[c("consecutive", 2)]] <- list(1:10, 11:20)
    ok_segs[[c("consecutive", 3)]] <- list(1:8, 9:14, 15:20)
    ok_segs[[c("interleaved", 1)]] <- list(1:20)
    ok_segs[[c("interleaved", 2)]] <- list(c(1,2,5,6,9,10,13,14,17,18),
                                           c(3,4,7,8,11,12,15,16,19,20))
    ok_segs[[c("interleaved", 3)]] <- list(c(1,2,7,8,13,14,19,20),
                                           c(3,4,9,10,15,16),
                                           c(5,6,11,12,17,18))
    for (type in c("random", "consecutive", "interleaved")) {
        for (k in c(1,2,3)) {
            if (k == 3) options(warn = -1) # Ignore warning
            segs <- cvsegments(N = N, k = k, nrep = nrep, type = type)
            if (k == 3) options(warn = 0)
            test_id <- paste("N = ", N, ", k = ", k, ", nrep = ", nrep,
                             ", type = ", type)
            if (type == "random") {
                switch(as.character(k),
                       "1" = {
                           checkTrue(all.equal(1:N, sort(segs[[1]]),
                                               check.attributes = FALSE),
                                     test_id)
                       },
                       "2" = {
                           checkTrue(
                               length(segs) == k &&
                               all(sapply(segs, length) == N/k) &&
                               length(intersect(segs[[1]], segs[[2]])) == 0,
                               test_id)
                       },
                       "3" = {
                           checkTrue(
                               length(segs) == k &&
                               all(sapply(segs, length) == c(8, 6, 6)) &&
                               length(intersect(segs[[1]], segs[[2]])) == 0 &&
                               length(intersect(segs[[1]], segs[[3]])) == 0 &&
                               length(intersect(segs[[2]], segs[[3]])) == 0,
                               test_id)
                       }
                       )
            } else {
                checkTrue(all.equal(ok_segs[[c(type, k)]], segs,
                                    check.attributes = FALSE),
                          test_id)
            }
        }
    }
}
