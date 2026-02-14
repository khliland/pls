# Prediction Plots

Functions to plot predicted values against measured values for a fitted
model.

## Usage

``` r
predplot(object, ...)

# Default S3 method
predplot(object, ...)

# S3 method for class 'mvr'
predplot(
  object,
  ncomp = object$ncomp,
  which,
  newdata,
  nCols,
  nRows,
  xlab = "measured",
  ylab = "predicted",
  main,
  ask = nRows * nCols < nPlots && dev.interactive(),
  ...,
  font.main,
  cex.main
)

predplotXy(
  x,
  y,
  line = FALSE,
  labels,
  type = "p",
  main = "Prediction plot",
  xlab = "measured response",
  ylab = "predicted response",
  line.col = par("col"),
  line.lty = NULL,
  line.lwd = NULL,
  ...
)
```

## Arguments

- object:

  a fitted model.

- ...:

  further arguments sent to underlying plot functions.

- ncomp:

  integer vector. The model sizes (numbers of components) to use for
  prediction.

- which:

  character vector. Which types of predictions to plot. Should be a
  subset of `c("train", "validation", "test")`. If not specified,
  `plot.mvr` selects test set predictions if `newdata` is supplied,
  otherwise cross-validated predictions if the model has been
  cross-validated, otherwise fitted values from the calibration data.

- newdata:

  data frame. New data to predict.

- nCols, nRows:

  integer. The number of coloumns and rows the plots will be laid out
  in. If not specified, `plot.mvr` tries to be intelligent.

- xlab, ylab:

  titles for \\x\\ and \\y\\ axes. Typically character strings, but can
  be expressions or lists. See
  [`title`](https://rdrr.io/r/graphics/title.html) for details.

- main:

  optional main title for the plot. See Details.

- ask:

  logical. Whether to ask the user before each page of a plot.

- font.main:

  font to use for main titles. See
  [`par`](https://rdrr.io/r/graphics/par.html) for details. Also see
  Details below.

- cex.main:

  numeric. The magnification to be used for main titles relative to the
  current size. Also see Details below.

- x:

  numeric vector. The observed response values.

- y:

  numeric vector. The predicted response values.

- line:

  logical. Whether a target line should be drawn.

- labels:

  optional. Alternative plot labels to use. Either a vector of labels,
  or `"names"` or `"numbers"` to use the row names or row numbers of the
  data as labels.

- type:

  character. What type of plot to make. Defaults to `"p"` (points). See
  [`plot`](https://rdrr.io/r/graphics/plot.default.html) for a complete
  list of types. The argument is ignored if `labels` is specified.

- line.col, line.lty, line.lwd:

  character or numeric. The `col`, `lty` and `lwd` parametres for the
  target line. See [`par`](https://rdrr.io/r/graphics/par.html) for
  details.

## Value

The functions invisibly return a matrix with the (last) plotted data.

## Details

`predplot` is a generic function for plotting predicted versus measured
response values, with default and `mvr` methods currently implemented.
The default method is very simple, and doesn't handle multiple responses
or new data.

The `mvr` method, handles multiple responses, model sizes and types of
predictions by making one plot for each combination. It can also be
called through the plot method for `mvr`, by specifying
`plottype = "prediction"` (the default).

The argument `main` can be used to specify the main title of the plot.
It is handled in a non-standard way. If there is only on (sub) plot,
`main` will be used as the main title of the plot. If there is *more*
than one (sub) plot, however, the presence of `main` will produce a
corresponding ‘global’ title on the page. Any graphical parametres,
e.g., `cex.main`, supplied to `coefplot` will only affect the ‘ordinary’
plot titles, not the ‘global’ one. Its appearance can be changed by
setting the parameters with
[`par`](https://rdrr.io/r/graphics/par.html), which will affect *both*
titles (with the exception of `font.main` and `cex.main`, which will
only affect the ‘global’ title when there is more than one plot). (To
have different settings for the two titles, one can override the `par`
settings with arguments to `predplot`.)

`predplotXy` is an internal function and is not meant for interactive
use. It is called by the `predplot` methods, and its arguments, e.g,
`line`, can be given in the `predplot` call.

## Note

The `font.main` and `cex.main` must be (completely) named. This is to
avoid that any argument `cex` or `font` matches them.

Tip: If the labels specified with `labels` are too long, they get
clipped at the border of the plot region. This can be avoided by
supplying the graphical parameter `xpd = TRUE` in the plot call.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`plot.mvr`](https://khliland.github.io/pls/reference/plot.mvr.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(yarn)
mod <- plsr(density ~ NIR, ncomp = 10, data = yarn[yarn$train,], validation = "CV")
if (FALSE) { # \dontrun{
predplot(mod, ncomp = 1:6)
plot(mod, ncomp = 1:6) # Equivalent to the previous
## Both cross-validated and test set predictions:
predplot(mod, ncomp = 4:6, which = c("validation", "test"),
         newdata = yarn[!yarn$train,])
} # }

data(oliveoil)
mod.sens <- plsr(sensory ~ chemical, ncomp = 4, data = oliveoil)
if (FALSE) plot(mod.sens, ncomp = 2:4) # Several responses gives several plots # \dontrun{}
```
