# The pls R package

This is the source code repository of the CRAN package "pls".

## Installation from CRAN

(Recommended, unless you want to test the newest changes.)

    install.packages("pls")

## Installation from github

(If needed, install the `devtools` package from CRAN first.)

    library(devtools)
    install_github("khliland/pls")

## Usage

See the help pages in the package for instructions, or (if you installed from CRAN): `vignette("pls-manual")`. Documentation is also avaiable as a GitHub page at [https://khliland.github.io/pls/](https://khliland.github.io/pls/).

## New featuress

- 2.9.0: Added `nipals()` (NIPALS PLS) and `nipalspcr()` (NIPALS PCR) with robustness against missing data. Added cross-validation and test set abilities for `scores()` and `scoreplot()`. Added `pkgdown` site with documentation and vignette.
- 2.8-4: Added fac2seg() to create cross-validation segments from a factor.
- 2.8-1: Added vcov() for computing variance-covariance matrices for PCR.