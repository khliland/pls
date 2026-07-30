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
[`vignette("pls-manual")`](https://khliland.github.io/pls/articles/pls-manual.md)
and
[`vignette("pls-tidyverse")`](https://khliland.github.io/pls/articles/pls-tidyverse.md).
Documentation is also avaiable as a GitHub page at
<https://khliland.github.io/pls/>.

### Lightweight tidy examples

``` r

library(pls)
data(yarn)

# Keep examples fast by using a small subset and few components.
d <- yarn[1:20, , drop = FALSE]
mod <- plsr(density ~ NIR, ncomp = 2, data = d, validation = "CV")

# Long-format model components
head(tidy(mod, what = c("weights", "loadings", "coefficients")))

# Long-format validation metrics
head(tidy(RMSEP(mod, estimate = c("train", "CV"))))

# One-row model summary
glance(mod)

# Augment training data with fitted values and residuals
head(augment(mod))

# Augment new data with fitted values
head(augment(mod, data = d[1:5, , drop = FALSE]))
```

### ggplot workflows

``` r

library(pls)
data(yarn)
d <- yarn[1:20, , drop = FALSE]
mod <- plsr(density ~ NIR, ncomp = 2, data = d, validation = "CV")

if (requireNamespace("ggplot2", quietly = TRUE)) {
    gg_loadingplot(mod, scatter = TRUE)
    gg_loadingplot(mod)
    gg_predplot(mod, ncomp = 1:2)
    gg_corrplot(mod)
    gg_coefplot(mod, ncomp = 1:2)
}
```

## New features

- 3.0-0: Added tidyverse extensions:
  [`tidy.mvr()`](https://khliland.github.io/pls/reference/tidy.md),
  [`glance.mvr()`](https://khliland.github.io/pls/reference/glance.md),
  [`augment.mvr()`](https://khliland.github.io/pls/reference/augment.md),
  [`tidy.mvrVal()`](https://khliland.github.io/pls/reference/tidy.md),
  ggplot2 graphics:
  [`gg_scoreplot()`](https://khliland.github.io/pls/reference/gg_scoreplot.md),
  [`gg_loadingplot()`](https://khliland.github.io/pls/reference/gg_loadingplot.md),
  [`gg_biplot()`](https://khliland.github.io/pls/reference/gg_biplot.md),
  [`gg_predplot()`](https://khliland.github.io/pls/reference/gg_predplot.md),
  [`gg_corrplot()`](https://khliland.github.io/pls/reference/gg_corrplot.md),
  [`gg_coefplot()`](https://khliland.github.io/pls/reference/gg_coefplot.md),
  [`autoplot.mvr()`](https://khliland.github.io/pls/reference/autoplot.mvr.md),
  and interactive data selection with
  [`refit.mvr()`](https://khliland.github.io/pls/reference/refit.md).
- 2.9-0: Added
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
