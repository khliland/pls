# ggplot-based Prediction Plot for mvr Models

Create a prediction plot from an `mvr` model using ggplot2.

## Usage

``` r
gg_predplot(object, ...)

# S3 method for class 'mvr'
gg_predplot(
  object,
  ncomp = 1:object$ncomp,
  which,
  newdata,
  labels = FALSE,
  type = "p",
  line = TRUE,
  line.col = "grey50",
  line.lty = 2,
  line.lwd = 0.5,
  xlab = "measured",
  ylab = "predicted",
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

- ncomp:

  integer vector. The model sizes to use for prediction.

- which:

  character vector. One or more of `"train"`, `"validation"` and
  `"test"`.

- newdata:

  optional data frame for test predictions.

- labels:

  optional labels for points. If `TRUE` or `"names"`, row names are
  used; if `"numbers"`, row numbers are used.

- type:

  character. Plot symbol type passed through to `geom_point`.

- line:

  logical. If `TRUE`, draw a \\y = x\\ reference line.

- line.col, line.lty, line.lwd:

  graphical settings for the reference line.

- xlab, ylab:

  axis labels.

- main:

  optional plot title.

- plotly:

  logical. If `TRUE`, use plotly output.

## Value

A `ggplot` object.

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 6, data = yarn, validation = "CV")
if (requireNamespace("ggplot2", quietly = TRUE)) {
  gg_predplot(mod, ncomp = 1:2)
}
```
