### runit.mvrVal.R: test functions for R2(), MSEP() and RMSEP()
### By Bjørn-Helge Mevik
### Started 2007-06-20

###
### Check for capturing argument errors:
###
test.argErrors <- function() {
    mod <- mvr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
    checkException(R2(mod, comps = 2:5), silent = TRUE)
    checkException(R2(mod, estimate = "adjCV"), silent = TRUE)
    checkException(MSEP(mod, comps = 1:4), silent = TRUE)
}

###
### Checking dimensions and dimnames of output
###
test.dimR2 <- function() {
    ##
    ## Uni-response models:
    ##
    n <- nrow(yarn)
    ncomp <- 7
    comps <- c(1,3,5)
    ## No CV
    mod <- mvr(density ~ NIR, ncomp = ncomp, data = yarn, method = "svdpc")
    res <- R2(mod)
    checkEquals(dim(res$val), c(1,1,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = yarn)
    checkEquals(dim(res$val), c(1,1,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "test")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, ncomp = 2:5)
    checkEquals(dim(res$val), c(1,1,4 + 1))
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, c(0,2:5))
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = yarn, estimate = "all")
    checkEquals(dim(res$val), c(2,1,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], c("train", "test"))
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = yarn, intercept = FALSE, estimate = c("test", "train"))
    checkEquals(dim(res$val), c(2,1,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("test", "train"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, intercept = TRUE, comps = comps)
    checkEquals(dim(res$val), c(1,1,2), "uni, intercept, comps, 1")
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, c(0, comps), "uni, intercept, comps, 3")
    checkIdentical(res$cumulative, FALSE)
    res <- R2(mod, intercept = FALSE, comps = comps)
    checkEquals(dim(res$val), c(1,1,1), "uni, no intercept, comps, 1")
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, comps)
    checkIdentical(res$cumulative, FALSE)
    ## With CV
    mod <- mvr(density ~ NIR, ncomp = ncomp, data = yarn, validation = "CV")
    res <- R2(mod)
    checkEquals(dim(res$val), c(1,1,ncomp + 1), "uni, CV, 1")
    checkIdentical(dimnames(res$val)[[1]], "CV")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = yarn)
    checkEquals(dim(res$val), c(1,1,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "test")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, ncomp = 2:5)
    checkEquals(dim(res$val), c(1,1,4 + 1))
    checkIdentical(dimnames(res$val)[[1]], "CV")
    checkEquals(res$comps, c(0,2:5))
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, intercept = FALSE)
    checkEquals(dim(res$val), c(1,1,ncomp))
    checkIdentical(dimnames(res$val)[[1]], "CV")
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = yarn, estimate = "all")
    checkEquals(dim(res$val), c(3,1,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], c("train", "CV", "test"), "uni, CV, newdata, all, 2")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = yarn, intercept = FALSE, estimate = "CV")
    checkEquals(dim(res$val), c(1,1,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("CV"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    ##
    ## Multi-response models:
    ##
    n <- nrow(oliveoil)
    nresp <- ncol(oliveoil$chemical)
    ncomp <- 4
    comps <- c(2,4)
    ## No CV
    mod <- mvr(chemical ~ sensory, ncomp = ncomp, data = oliveoil)
    res <- R2(mod)
    checkEquals(dim(res$val), c(1,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = oliveoil)
    checkEquals(dim(res$val), c(1,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "test")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, ncomp = 2:4)
    checkEquals(dim(res$val), c(1,nresp,3 + 1), "multi, ncomp, 1")
    checkIdentical(dimnames(res$val)[[1]], "train", "multi, ncomp,2")
    checkEquals(res$comps, c(0,2:4), "multi, ncomp,3")
    checkIdentical(res$cumulative, TRUE, "multi, ncomp,4")
    res <- R2(mod, newdata = oliveoil, estimate = "all")
    checkEquals(dim(res$val), c(2,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], c("train", "test"))
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = oliveoil, intercept = FALSE, estimate = c("test", "train"))
    checkEquals(dim(res$val), c(2,nresp,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("test", "train"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, intercept = TRUE, comps = comps)
    checkEquals(dim(res$val), c(1,nresp,2), "multi, intercept, comps, 1")
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, c(0, comps), "multi, intercept, comps, 3")
    checkIdentical(res$cumulative, FALSE)
    res <- R2(mod, intercept = FALSE, comps = comps)
    checkEquals(dim(res$val), c(1,nresp,1), "multi, no intercept, comps, 1")
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, comps)
    checkIdentical(res$cumulative, FALSE)
    ## With CV
    mod <- mvr(chemical ~ sensory, ncomp = ncomp, data = oliveoil, validation = "CV", method = "svdpc")
    res <- R2(mod)
    checkEquals(dim(res$val), c(1,nresp,ncomp + 1), "multi, CV, 1")
    checkIdentical(dimnames(res$val)[[1]], "CV")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = oliveoil)
    checkEquals(dim(res$val), c(1,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "test")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, ncomp = 2:4)
    checkEquals(dim(res$val), c(1,nresp,3 + 1), "multi, CV, ncomp, 1")
    checkIdentical(dimnames(res$val)[[1]], "CV", "multi, CV, ncomp, 2")
    checkEquals(res$comps, c(0,2:4), "multi, CV, ncomp, 3")
    checkIdentical(res$cumulative, TRUE, "multi, CV, ncomp, 4")
    res <- R2(mod, intercept = FALSE)
    checkEquals(dim(res$val), c(1,nresp,ncomp))
    checkIdentical(dimnames(res$val)[[1]], "CV")
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = oliveoil, estimate = "all")
    checkEquals(dim(res$val), c(3,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], c("train", "CV", "test"), "multi, CV, newdata, all, 2")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- R2(mod, newdata = oliveoil, intercept = FALSE, estimate = "CV")
    checkEquals(dim(res$val), c(1,nresp,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("CV"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
}

test.dimMSEP <- function() {
    ##
    ## Uni-response models:
    ##
    n <- nrow(yarn)
    ncomp <- 7
    comps <- c(1,3,5)
    ## No CV
    mod <- mvr(density ~ NIR, ncomp = ncomp, data = yarn, method = "svdpc")
    res <- MSEP(mod)
    checkEquals(dim(res$val), c(1,1,ncomp + 1), "uni, 1")
    checkIdentical(dimnames(res$val)[[1]], "train", "uni, 2")
    checkEquals(res$comps, 0:ncomp, "uni, 3")
    checkIdentical(res$cumulative, TRUE, "uni, 4")
    res <- MSEP(mod, newdata = yarn)
    checkEquals(dim(res$val), c(1,1,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "test")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, ncomp = 2:5)
    checkEquals(dim(res$val), c(1,1,4 + 1))
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, c(0,2:5))
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = yarn, estimate = "all")
    checkEquals(dim(res$val), c(2,1,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], c("train", "test"))
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = yarn, intercept = FALSE, estimate = c("test", "train"))
    checkEquals(dim(res$val), c(2,1,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("test", "train"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, intercept = TRUE, comps = comps)
    checkEquals(dim(res$val), c(1,1,2), "uni, intercept, comps, 1")
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, c(0, comps), "uni, intercept, comps, 3")
    checkIdentical(res$cumulative, FALSE)
    res <- MSEP(mod, intercept = FALSE, comps = comps)
    checkEquals(dim(res$val), c(1,1,1), "uni, no intercept, comps, 1")
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, comps)
    checkIdentical(res$cumulative, FALSE)
    ## With CV
    mod <- mvr(density ~ NIR, ncomp = ncomp, data = yarn, validation = "CV")
    res <- MSEP(mod)
    checkEquals(dim(res$val), c(2,1,ncomp + 1), "uni, CV, 1")
    checkIdentical(dimnames(res$val)[[1]], c("CV", "adjCV"))
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = yarn)
    checkEquals(dim(res$val), c(1,1,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "test")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, ncomp = 2:5)
    checkEquals(dim(res$val), c(2,1,4 + 1))
    checkIdentical(dimnames(res$val)[[1]], c("CV", "adjCV"))
    checkEquals(res$comps, c(0,2:5))
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, intercept = FALSE)
    checkEquals(dim(res$val), c(2,1,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("CV", "adjCV"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = yarn, estimate = "all")
    checkEquals(dim(res$val), c(4,1,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], c("train", "CV", "adjCV", "test"), "uni, CV, newdata, all, 2")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = yarn, intercept = FALSE, estimate = "CV")
    checkEquals(dim(res$val), c(1,1,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("CV"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = yarn, intercept = FALSE, estimate = "adjCV")
    checkEquals(dim(res$val), c(1,1,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("adjCV"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = yarn, intercept = FALSE, estimate = c("adjCV", "CV"))
    checkEquals(dim(res$val), c(2,1,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("adjCV", "CV"), "uni, newdata, no intercept, adjCV, CV")
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    ##
    ## Multi-response models:
    ##
    n <- nrow(oliveoil)
    nresp <- ncol(oliveoil$chemical)
    ncomp <- 5
    comps <- c(1,3,4)
    ## No CV
    mod <- mvr(chemical ~ sensory, ncomp = ncomp, data = oliveoil, method = "svdpc")
    res <- MSEP(mod)
    checkEquals(dim(res$val), c(1,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = oliveoil)
    checkEquals(dim(res$val), c(1,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "test")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, ncomp = 2:5)
    checkEquals(dim(res$val), c(1,nresp,4 + 1))
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, c(0,2:5))
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = oliveoil, estimate = "all")
    checkEquals(dim(res$val), c(2,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], c("train", "test"))
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = oliveoil, intercept = FALSE, estimate = c("test", "train"))
    checkEquals(dim(res$val), c(2,nresp,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("test", "train"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, intercept = TRUE, comps = comps)
    checkEquals(dim(res$val), c(1,nresp,2), "uni, intercept, comps, 1")
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, c(0, comps), "uni, intercept, comps, 3")
    checkIdentical(res$cumulative, FALSE)
    res <- MSEP(mod, intercept = FALSE, comps = comps)
    checkEquals(dim(res$val), c(1,nresp,1), "uni, no intercept, comps, 1")
    checkIdentical(dimnames(res$val)[[1]], "train")
    checkEquals(res$comps, comps)
    checkIdentical(res$cumulative, FALSE)
    ## With CV
    mod <- mvr(chemical ~ sensory, ncomp = ncomp, data = oliveoil, validation = "CV")
    res <- MSEP(mod)
    checkEquals(dim(res$val), c(2,nresp,ncomp + 1), "uni, CV, 1")
    checkIdentical(dimnames(res$val)[[1]], c("CV", "adjCV"))
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = oliveoil)
    checkEquals(dim(res$val), c(1,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], "test")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, ncomp = 2:5)
    checkEquals(dim(res$val), c(2,nresp,4 + 1))
    checkIdentical(dimnames(res$val)[[1]], c("CV", "adjCV"))
    checkEquals(res$comps, c(0,2:5))
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, intercept = FALSE)
    checkEquals(dim(res$val), c(2,nresp,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("CV", "adjCV"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = oliveoil, estimate = "all")
    checkEquals(dim(res$val), c(4,nresp,ncomp + 1))
    checkIdentical(dimnames(res$val)[[1]], c("train", "CV", "adjCV", "test"), "uni, CV, newdata, all, 2")
    checkEquals(res$comps, 0:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = oliveoil, intercept = FALSE, estimate = "CV")
    checkEquals(dim(res$val), c(1,nresp,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("CV"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = oliveoil, intercept = FALSE, estimate = "adjCV")
    checkEquals(dim(res$val), c(1,nresp,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("adjCV"))
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
    res <- MSEP(mod, newdata = oliveoil, intercept = FALSE, estimate = c("adjCV", "CV"))
    checkEquals(dim(res$val), c(2,nresp,ncomp))
    checkIdentical(dimnames(res$val)[[1]], c("adjCV", "CV"), "uni, newdata, no intercept, adjCV, CV")
    checkEquals(res$comps, 1:ncomp)
    checkIdentical(res$cumulative, TRUE)
}


###
### Check some numerical values
###
test.estimates <- function() {
    mod <- mvr(chemical ~ sensory, data = oliveoil)
    res <- R2(mod, newdata = oliveoil, estimate = "all")
    checkEquals(res$val["train",,], res$val["test",,], "R2, train = test")
    checkEquals(res$val["train","Acidity","2 comps"], cor(fitted(mod)[,"Acidity","2 comps"], oliveoil$chemical[,"Acidity"])^2, "R2, train R^2 = cor^2")
    res <- MSEP(mod, newdata = oliveoil, estimate = "all")
    checkEquals(res$val["train",,], res$val["test",,], "MSEP, train = test")
    checkEqualsNumeric(res$val["train",,-1], colMeans((fitted(mod)- c(oliveoil$chemical))^2), "MSEP, train directly")
    mod <- mvr(chemical ~ sensory, data = oliveoil, validation = "CV")
    res <- MSEP(mod, estimate = "CV")
    checkEqualsNumeric(res$val["CV",,-1],
                colMeans((mod$validation$pred - c(oliveoil$chemical))^2))
    ## FIXME: Temporary check:
    checkEquals(res$val["CV",,1],
                apply(oliveoil$chemical, 2, var) * nrow(oliveoil) / (nrow(oliveoil)-1))
}


###
### Test handling of missing values
###

## Handling of missing data in train and/or test data
test.MSEPmissing <- function() {
    ## Two missing in X, one in Y
    olmiss <- oliveoil
    olmiss$chemical[2,3] <- NA
    olmiss$chemical[3,4] <- NA
    olmiss$sensory[4,2] <- NA
    ## Common segments for comparison:
    ## Parametres:
    ## - mvr: na.action: omit (default) / exclude
    ## - MSEP: ncomp (default) / comps

    ## na.action = na.omit (default)
    mod <- mvr(sensory ~ chemical, data = olmiss, validation = "LOO")
    res1 <- MSEP(mod, estimate = "all", newdata = olmiss)
    checkTrue(all(is.finite(res1$val)), "na.omit, all is finite 1")
    checkEquals(res1$val["train",,], res1$val["test",,],
                "na.omit, train == test")
    checkEqualsNumeric(res1$val["train",,-1],
                       colMeans((fitted(mod) - c(olmiss$sensory[-c(2:4),]))^2),
                       "na.omit, train == fitted")
    checkEqualsNumeric(res1$val["CV",,-1],
                       mod$validation$PRESS / (nrow(olmiss) - 3),
                       "na.omit, CV == PRESS/n")
    ## FIXME: Check adjCV

    res2 <- MSEP(mod, estimate = c("train", "test"), newdata = olmiss,
                 comps = 1:5)
    checkTrue(all(is.finite(res2$val)), "na.omit, all is finite 2")
    checkEquals(drop(res2$val), res1$val[c("train", "test"),,6],
                "na.omit, comps == ncomp")

    ## na.action = na.exclude
    mod <- mvr(sensory ~ chemical, data = olmiss, validation = "LOO",
                na.action = na.exclude)
    res3 <- MSEP(mod, estimate = "all", newdata = olmiss)
## Superfluous:
##     checkTrue(all(is.finite(res3$val)), "na.exclude, all is finite 1")
##     checkEquals(res3$val["train",,], res3$val["test",,],
##                 "na.exclude, train == test")
##     checkEqualsNumeric(res3$val["train",,-1],
##                        colMeans((fitted(mod)[-c(2:4),,] -
##                                  c(olmiss$sensory[-c(2:4),]))^2),
##                        "na.exclude, train == fitted")
##     checkEqualsNumeric(res3$val["CV",,-1],
##                        mod$validation$PRESS / (nrow(olmiss) - 3),
##                        "na.exclude, CV == PRESS/n")
##     ## FIXME: Check adjCV

    res4 <- MSEP(mod, estimate = c("train", "test"), newdata = olmiss,
                 comps = 1:5)
## Superfluous:
##     checkTrue(all(is.finite(res4$val)), "na.exclude, all is finite 2")
##     checkEquals(drop(res4$val), res3$val[c("train", "test"),,6],
##                 "na.exclude, comps == ncomp")

    ## na.omit vs. na.exclude
    checkIdentical(res1, res3, "res1 == res3")
    checkIdentical(res2, res4, "res2 == res4")
}
