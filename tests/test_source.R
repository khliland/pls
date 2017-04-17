### test_source.R: Runs all RUnit tests that work when the code is source()'d


## Get warnings at once
options(warn = 1)

## Make sure the global environment is clean:
rm(list = ls(all = TRUE))

## Get the pls functions and data
plsdir <- "../00_pkg_src/pls"
for (fil in list.files(file.path(plsdir, "R"), ".*\\.R$", full.names = TRUE)) source(fil)
for (fil in list.files(file.path(plsdir, "data"), ".*\\.RData$", full.names = TRUE)) load(fil)
ls()

## Get RUnit and utility functions
library(RUnit)
source("RUnit/common/utils.R")

## Make checkException silent
opts <- getOption("RUnit")
opts$silent <- TRUE
options(RUnit = opts)

## Define and check test suite
testdirs <- c("RUnit/common", "RUnit/source")
plsTestSuite <- defineTestSuite("pls", testdirs)
isValidTestSuite(plsTestSuite)

## Run test suite
printTextProtocol(res <- runTestSuite(plsTestSuite))
if (res$pls$nFail > 0 || res$pls$nErr > 0) stop("One or more tests failed")
