# ggplot-based Score Plot for mvr Models

Create a score plot from an `mvr` model using `ggplot2`.

## Usage

``` r
gg_scoreplot(object, ...)

# S3 method for class 'mvr'
gg_scoreplot(
  object,
  comps = 1:2,
  estimate,
  newdata,
  select = FALSE,
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

  integer vector of length two, giving the score components to plot.

- estimate:

  optional character vector passed to
  [`scores()`](https://khliland.github.io/pls/reference/scores.md).

- newdata:

  optional data frame passed to
  [`scores()`](https://khliland.github.io/pls/reference/scores.md) when
  `estimate = "test"`.

- select:

  logical. If `TRUE`, launch an interactive selection app and return
  selected IDs.

- plotly:

  logical. If `TRUE`, use plotly output. With `select = TRUE`, a Shiny
  app is launched and selected IDs are returned from Plotly lasso/box
  selection.

## Value

For `select = FALSE`, a `ggplot` object (or a `plotly` object when
`plotly = TRUE`). For `select = TRUE`, a character vector with selected
sample IDs.

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 6, data = yarn)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- gg_scoreplot(mod)
  print(p)
}


if (FALSE)  # Requires an interactive session
if (requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("ggiraph", quietly = TRUE) &&
    requireNamespace("shiny", quietly = TRUE)) {
  selected_ids <- gg_scoreplot(mod, select = TRUE)
  refit_mod <- refit(mod, selection = selected_ids, exclude = TRUE)
} # \dontrun{}
```
