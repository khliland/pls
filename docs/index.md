# The pls R package

This is the source code repository of the CRAN package “pls”.

## Installation from CRAN

(Recommended, unless you want to test the newest changes.)

``` R
install.packages("pls")
```

## Installation from github

(If needed, install the `devtools` package from CRAN first.)

``` R
library(devtools)
install_github("khliland/pls")
```

## Usage

See the help pages in the package for instructions, or (if you installed
from CRAN):
[`vignette("pls-manual")`](https://khliland.github.io/pls/articles/pls-manual.md).
Documentation is also avaiable as a GitHub page at
<https://khliland.github.io/pls/>.

## New featuress

- 2.9.0: Added
  [`nipals()`](https://khliland.github.io/pls/reference/mvr.md) (NIPALS
  PLS) and
  [`nipalspcr()`](https://khliland.github.io/pls/reference/mvr.md)
  (NIPALS PCR) with robustness against missing data. Added
  cross-validation and test set abilities for
  [`scores()`](https://khliland.github.io/pls/reference/scores.md) and
  [`scoreplot()`](https://khliland.github.io/pls/reference/scoreplot.md).
  Added `pkgdown` site with documentation and vignette.
- 2.8-4: Added fac2seg() to create cross-validation segments from a
  factor.
- 2.8-1: Added vcov() for computing variance-covariance matrices for
  PCR.
