# ggplot-based Validation Plot for mvr Models

Create a validation-statistic plot from an `mvr` model or an `mvrVal`
object using ggplot2.

## Usage

``` r
gg_valplot(object, ...)

# S3 method for class 'mvr'
gg_valplot(
  object,
  val.type = c("RMSEP", "MSEP", "R2"),
  estimate,
  newdata,
  ncomp,
  comps,
  intercept,
  xlab = "number of components",
  ylab,
  main = NULL,
  ...
)

# S3 method for class 'mvrVal'
gg_valplot(object, xlab = "number of components", ylab, main = NULL, ...)
```

## Arguments

- object:

  model object.

- ...:

  further arguments passed to methods.

- val.type:

  character. Validation metric to plot. One of `"RMSEP"`, `"MSEP"` or
  `"R2"`.

- estimate:

  optional character vector of estimators passed to
  [`RMSEP()`](https://khliland.github.io/pls/reference/mvrVal.md),
  [`MSEP()`](https://khliland.github.io/pls/reference/mvrVal.md) or
  [`R2()`](https://khliland.github.io/pls/reference/mvrVal.md).

- newdata:

  optional data frame used for test-set validation estimates.

- ncomp, comps:

  model size specification passed to validation metrics.

- intercept:

  logical. Whether to include zero-component model in validation
  metrics.

- xlab, ylab:

  axis labels.

- main:

  optional title.

## Value

A `ggplot` object.

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  gg_valplot(mod, val.type = "RMSEP", estimate = c("train", "CV"))
}
```
