### test.R: Runs all RUnit tests that work when the functions are accessed by direct
### import (i.e., pls::mvr(...))


## Get warnings at once
options(warn = 1)

## Make sure the global environment is clean:
rm(list = ls(all = TRUE))

## Get the data
data(yarn, package = "pls")
data(oliveoil, package = "pls")
data(gasoline, package = "pls")

## Get RUnit and utility functions
library(RUnit)
source("RUnit/common/utils.R")

## Make checkException silent
opts <- getOption("RUnit")
opts$silent <- TRUE
options(RUnit = opts)

## Define and check test suite
testdirs <- c("RUnit/import")
plsTestSuite <- defineTestSuite("pls", testdirs)
isValidTestSuite(plsTestSuite)

## Run test suite
printTextProtocol(res <- runTestSuite(plsTestSuite))
if (res$pls$nFail > 0 || res$pls$nErr > 0) stop("One or more tests failed")
