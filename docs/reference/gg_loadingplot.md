# ggplot-based Loading Plot for mvr Models

Create a loading plot from an `mvr` model using ggplot2.

## Usage

``` r
gg_loadingplot(object, ...)

# S3 method for class 'mvr'
gg_loadingplot(
  object,
  comps = 1:2,
  scatter = FALSE,
  labels = FALSE,
  pretty.xlabels = TRUE,
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

  integer vector of length two, giving the loading components to plot.

- scatter:

  logical. If `TRUE`, make a scatter plot of the selected loading
  components. If `FALSE`, draw loading profiles across variables.

- labels:

  optional labels. In scatter mode, labels are used as point labels. In
  line mode, labels are used as x-axis labels.

- pretty.xlabels:

  logical. If `TRUE`, line-plot x-axis labels are thinned when possible.

- plotly:

  logical. If `TRUE`, use plotly output.

## Value

A `ggplot` object.

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  gg_loadingplot(mod, scatter = TRUE)
  gg_loadingplot(mod)
}
```
