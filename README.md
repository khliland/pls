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

See the help pages in the package for instructions, or (if you installed from CRAN): `vignette("pls-manual")` and `vignette("pls-tidyverse")`. Documentation is also avaiable as a GitHub page at [https://khliland.github.io/pls/](https://khliland.github.io/pls/).

### tidy examples

```r
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

```r
library(pls)
data(yarn)
d <- yarn[1:20, , drop = FALSE]
mod <- plsr(density ~ NIR, ncomp = 2, data = d, validation = "CV")

if (requireNamespace("ggplot2", quietly = TRUE)) {
    gg_scoreplot(mod)
    gg_loadingplot(mod, scatter = TRUE)
    gg_loadingplot(mod)
    gg_predplot(mod, ncomp = 1:2)
    gg_corrplot(mod)
    gg_coefplot(mod, ncomp = 1:2)
}
```

## New features

- 3.0-0: Added tidyverse extensions: `tidy.mvr()`, `glance.mvr()`, `augment.mvr()`, `tidy.mvrVal()`, ggplot2 graphics: `gg_scoreplot()`, `gg_loadingplot()`, `gg_biplot()`, `gg_predplot()`, `gg_corrplot()`, `gg_coefplot()`, `gg_valplot()`, `autoplot.mvr()`, `gg_dashboard()`, and interactive data selection with `refit.mvr()`.
- 2.9-0: Added `nipals()` (NIPALS PLS) and `nipalspcr()` (NIPALS PCR) with robustness against missing data. Added cross-validation and test set abilities for `scores()` and `scoreplot()`. Added `pkgdown` site with documentation and vignette.
- 2.8-4: Added fac2seg() to create cross-validation segments from a factor.
- 2.8-1: Added vcov() for computing variance-covariance matrices for PCR.