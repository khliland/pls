# ggplot-based Biplot for mvr Models

Create a biplot from an `mvr` model using ggplot2. Scores are shown as
points, while loadings are shown either as points (default) or arrows.

## Usage

``` r
gg_biplot(object, ...)

# S3 method for class 'mvr'
gg_biplot(
  object,
  comps = 1:2,
  labels = FALSE,
  arrows = FALSE,
  loading.scale = 0.8,
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

  integer vector of length two, giving the components to plot.

- labels:

  logical. If `TRUE`, loading labels are added.

- arrows:

  logical. If `TRUE`, draw loadings as arrows. Defaults to `FALSE`.

- loading.scale:

  numeric scaling factor for loadings.

- plotly:

  logical. If `TRUE`, use plotly output.

## Value

A `ggplot` object.

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  gg_biplot(mod)
}
```
