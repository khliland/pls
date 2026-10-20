# Autoplot Method for mvr Models

`autoplot.mvr` provides a ggplot2-based plotting entrypoint for `mvr`
objects.

## Usage

``` r
# S3 method for class 'mvr'
autoplot(
  object,
  type = c("scores", "loadings", "biplot", "prediction", "correlation", "coefficients",
    "validation"),
  ...
)
```

## Arguments

- object:

  an `mvr` object.

- type:

  plot type. One of `"scores"`, `"loadings"`, `"biplot"`,
  `"prediction"`, `"correlation"` or `"coefficients"`, `"validation"`,
  `"rawdata"`.

- ...:

  further arguments passed to the selected plotting function.

## Value

A `ggplot` object.

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  autoplot(mod, type = "scores")
  autoplot(mod, type = "loadings")
  autoplot(mod, type = "biplot")
  autoplot(mod, type = "prediction")
  autoplot(mod, type = "correlation")
  autoplot(mod, type = "coefficients")
  autoplot(mod, type = "validation", val.type = "RMSEP", estimate = "CV")
  autoplot(mod, type = "rawdata", type = "line")
}
#> Error in autoplot.mvr(object, ...): formal argument "type" matched by multiple actual arguments
```
