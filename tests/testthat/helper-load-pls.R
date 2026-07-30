# Ensure tests can run both via `R CMD check` and direct `testthat::test_dir()`.
if (!"package:pls" %in% search()) {
  suppressPackageStartupMessages(library(pls))
}
