### runit.predict.R: test functions for predict.mvr()
### By Bjørn-Helge Mevik
### Started 2007-06-11

###
### Handling of missing values
###

## test.NAtrain: test handling of missing values in training data
test.NAtrain <- function() {
    ## NAs in train X:
    olx <- oliveoil
    olx$chemical[2,3] <- NA
    n <- nrow(olx)
    oltest <- oliveoil[1:5,]
    ntest <- nrow(oltest)
    ## parametres:
    ## - train: na.action (omit, exclude)
    ## - ncomp / comps
    ## - type: response / scores
    ## - predict: fitted (default) / newdata

    ## na.action = omit (default)
    mod <- mvr(sensory ~ chemical, data = olx)
    checkEquals(nrow(predict(mod)), n - 1)
    checkEquals(nrow(predict(mod, type = "scores")), n - 1)
    checkEquals(nrow(predict(mod, comps = 1:mod$ncomp)), n - 1)
    checkEquals(nrow(predict(mod, comps = 1:mod$ncomp, type = "scores")), n - 1)
    checkEquals(nrow(predict(mod, newdata = oltest)), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, type = "scores")), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, comps = 1:mod$ncomp)), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, comps = 1:mod$ncomp, type = "scores")), ntest)
    ## na.action = exclude
    mod <- mvr(sensory ~ chemical, data = olx, na.action = na.exclude)
    checkEquals(nrow(predict(mod)), n)
    checkEquals(nrow(predict(mod, type = "scores")), n)
    checkEquals(nrow(predict(mod, comps = 1:mod$ncomp)), n)
    checkEquals(nrow(predict(mod, comps = 1:mod$ncomp, type = "scores")), n)
    checkEquals(nrow(predict(mod, newdata = oltest)), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, type = "scores")), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, comps = 1:mod$ncomp)), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, comps = 1:mod$ncomp, type = "scores")), ntest)

    ## NAs in train Y:
    oly <- oliveoil
    oly$sensory[3,2] <- NA
    n <- nrow(oly)
    ## parametres:
    ## - train: na.action (omit, exclude)
    ## - ncomp / comps
    ## - type: response / scores

    ## na.action = omit (default)
    mod <- mvr(sensory ~ chemical, data = oly)
    checkEquals(nrow(predict(mod)), n - 1)
    checkEquals(nrow(predict(mod, type = "scores")), n - 1)
    checkEquals(nrow(predict(mod, comps = 1:mod$ncomp)), n - 1)
    checkEquals(nrow(predict(mod, comps = 1:mod$ncomp, type = "scores")), n - 1)
    checkEquals(nrow(predict(mod, newdata = oltest)), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, type = "scores")), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, comps = 1:mod$ncomp)), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, comps = 1:mod$ncomp, type = "scores")), ntest)
    ## na.action = na.exclude
    mod <- mvr(sensory ~ chemical, data = oly, na.action = na.exclude)
    checkEquals(nrow(predict(mod)), n)
    checkEquals(nrow(predict(mod, type = "scores")), n)
    checkEquals(nrow(predict(mod, comps = 1:mod$ncomp)), n)
    checkEquals(nrow(predict(mod, comps = 1:mod$ncomp, type = "scores")), n)
    checkEquals(nrow(predict(mod, newdata = oltest)), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, type = "scores")), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, comps = 1:mod$ncomp)), ntest)
    checkEquals(nrow(predict(mod, newdata = oltest, comps = 1:mod$ncomp, type = "scores")), ntest)
}

## test.NAtest: test handling of missing values in test data
test.NAtest <- function() {
    ## NAs in test X:
    olx <- oliveoil[1:5,]
    olx$chemical[2,3] <- NA
    n <- nrow(olx)
    mod <- mvr(sensory ~ chemical, data = oliveoil)
    ## parametres:
    ## - test: na.action (pass (default), omit, exclude, fail)
    ## - ncomp (default) / comps
    ## - type: response (default) / scores

    ## na.action = pass (default)
    checkEquals(nrow(predict(mod, newdata = olx)), n, "pass, 1")
    checkEquals(nrow(predict(mod, newdata = olx, type = "scores")), n,
                "pass, 2")
    checkEquals(nrow(predict(mod, newdata = olx, comps = 1:mod$ncomp)), n,
                "pass, 3")
    checkEquals(nrow(predict(mod, newdata = olx, comps = 1:mod$ncomp, type =
                             "scores")), n, "pass, 4")

    ## na.action = omit
    checkEquals(nrow(predict(mod, newdata = olx, na.action = na.omit)),
                n - 1, "omit, 1")
    checkEquals(nrow(predict(mod, newdata = olx, type = "scores", na.action
                             = na.omit)), n - 1, "omit, 2")
    checkEquals(nrow(predict(mod, newdata = olx, comps = 1:mod$ncomp,
                             na.action = na.omit)), n - 1, "omit, 3")
    checkEquals(nrow(predict(mod, newdata = olx, comps = 1:mod$ncomp, type =
                             "scores", na.action = na.omit)), n - 1, "omit, 4")

    ## na.action = exclude
    checkEquals(nrow(predict(mod, newdata = olx, na.action = na.exclude)),
                n - 1, "exclude, 1")
    checkEquals(nrow(predict(mod, newdata = olx, type = "scores", na.action
                             = na.exclude)), n - 1, "exclude, 2")
    checkEquals(nrow(predict(mod, newdata = olx, comps = 1:mod$ncomp,
                             na.action = na.exclude)), n - 1, "exclude, 3")
    checkEquals(nrow(predict(mod, newdata = olx, comps = 1:mod$ncomp, type =
                             "scores", na.action = na.exclude)), n - 1,
                "exclude, 4")

    ## na.action = fail
    checkException(predict(mod, newdata = olx, na.action = na.fail), "fail",
                   silent = TRUE)

    ## NAs in test Y:
    oly <- oliveoil[1:5,]
    oly$sensory[3,2] <- NA
    n <- nrow(oly)

    checkEquals(nrow(predict(mod, newdata = oly)), n, "missing Y")
}
