### runit.mvr_wrappers.R: test functions for checking the mvr wrappers
### By Bjørn-Helge Mevik
### Started 2013-07-13

## test.wrapper_results: Check that the wrappers give the same results as mvr:
test.wrapper_results <- function() {
    # plsr
    mvrmod <- mvr(octane ~ NIR, ncomp=10, data = gasoline, x = TRUE, y = TRUE, method = "kernelpls")
    wrapmod <- plsr(octane ~ NIR, ncomp=10, data = gasoline, x = TRUE, y = TRUE)
    checkEquals(mvrmod[-c(14,18)], wrapmod[-c(14,18)],
                "plsr() doesn't give the same as mvr()")

    # pcr
    mvrmod <- mvr(octane ~ NIR, ncomp=10, data = gasoline, x = TRUE, y = TRUE, method = "svdpc")
    wrapmod <- pcr(octane ~ NIR, ncomp=10, data = gasoline, x = TRUE, y = TRUE)
    checkEquals(mvrmod[-c(12,16)], wrapmod[-c(12,16)],
                "pcr() doesn't give the same as mvr()")

    # cppls
    mvrmod <- mvr(oil.type ~ NIR, Y.add = design, ncomp=10, data = mayonnaise, x = TRUE, y = TRUE, method = "cppls")
    wrapmod <- cppls(oil.type ~ NIR, Y.add = design, ncomp=10, data = mayonnaise, x = TRUE, y = TRUE)
    checkEquals(mvrmod[-c(19,23)], wrapmod[-c(19,23)],
                "cppls() doesn't give the same as mvr()")
}

## test.wrapper_eval: Check that we don't pick up variables from the wrappers
test.wrapper_eval <- function() {
    ## ('cl' is a variable that exists in the wrapper.)
    if (exists("cl")) rm(cl)

    ## plsr:
    res <- try(plsr(y ~ X, data = cl), silent = TRUE)
    checkTrue(class(res) == "try-error" && grepl("object 'cl' not found", res),
              "plsr() picked up local variable 'cl'")

    ## pcr:
    res <- try(pcr(y ~ X, data = cl), silent = TRUE)
    checkTrue(class(res) == "try-error" && grepl("object 'cl' not found", res),
              "pcr() picked up local variable 'cl'")

    ## cppls:
    res <- try(cppls(y ~ X, data = cl), silent = TRUE)
    checkTrue(class(res) == "try-error" && grepl("object 'cl' not found", res),
              "cppls() picked up local variable 'cl'")
}
