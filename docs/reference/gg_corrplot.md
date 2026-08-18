# ggplot-based Correlation Loadings Plot for mvr Models

Create a correlation loadings plot from an `mvr` model using ggplot2.

## Usage

``` r
gg_corrplot(object, ...)

# S3 method for class 'mvr'
gg_corrplot(
  object,
  comps = 1:2,
  labels = FALSE,
  plotx = TRUE,
  ploty = FALSE,
  radii = c(sqrt(1/2), 1),
  xlab,
  ylab,
  main = NULL,
  plotly = FALSE,
  ...
)
```

## Arguments

- object:

  model object.

- ...:

  further arguments passed to methods.

- comps:

  integer vector of length at least two, giving the components to plot.

- labels:

  optional labels for points. If `TRUE` or `"names"`, row names are
  used; if `"numbers"`, row numbers are used.

- plotx:

  logical. Whether to plot X-variable correlation loadings.

- ploty:

  logical. Whether to plot Y-variable correlation loadings.

- radii:

  numeric vector of circle radii.

- xlab, ylab:

  axis labels.

- main:

  optional title.

- plotly:

  logical. If `TRUE`, use plotly output.

## Value

A `ggplot` object.

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  gg_corrplot(mod)
}
```
