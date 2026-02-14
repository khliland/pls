# Validation Plots

Functions to plot validation statistics, such as RMSEP or \\R^2\\, as a
function of the number of components.

## Usage

``` r
validationplot(
  object,
  val.type = c("RMSEP", "MSEP", "R2"),
  estimate,
  newdata,
  ncomp,
  comps,
  intercept,
  ...
)

# S3 method for class 'mvrVal'
plot(
  x,
  nCols,
  nRows,
  type = "l",
  lty = 1:nEst,
  lwd = par("lwd"),
  pch = 1:nEst,
  cex = 1,
  col = 1:nEst,
  legendpos,
  xlab = "number of components",
  ylab = x$type,
  main,
  ask = nRows * nCols < nResp && dev.interactive(),
  ...
)
```

## Arguments

- object:

  an `mvr` object.

- val.type:

  character. What type of validation statistic to plot.

- estimate:

  character. Which estimates of the statistic to calculate. See
  [`RMSEP`](https://khliland.github.io/pls/reference/mvrVal.md).

- newdata:

  data frame. Optional new data used to calculate statistic.

- ncomp, comps:

  integer vector. The model sizes to compute the statistic for. See
  [`RMSEP`](https://khliland.github.io/pls/reference/mvrVal.md).

- intercept:

  logical. Whether estimates for a model with zero components should be
  calculated as well.

- ...:

  Further arguments sent to underlying plot functions.

- x:

  an `mvrVal` object. Usually the result of a
  [`RMSEP`](https://khliland.github.io/pls/reference/mvrVal.md),
  [`MSEP`](https://khliland.github.io/pls/reference/mvrVal.md) or
  [`R2`](https://khliland.github.io/pls/reference/mvrVal.md) call.

- nCols, nRows:

  integers. The number of coloumns and rows the plots will be laid out
  in. If not specified, `plot.mvrVal` tries to be intelligent.

- type:

  character. What type of plots to create. Defaults to `"l"` (lines).
  Alternative types include `"p"` (points) and `"b"` (both). See
  [`plot`](https://rdrr.io/r/graphics/plot.default.html) for a complete
  list of types.

- lty:

  vector of line types (recycled as neccessary). Line types can be
  specified as integers or character strings (see
  [`par`](https://rdrr.io/r/graphics/par.html) for the details).

- lwd:

  vector of positive numbers (recycled as neccessary), giving the width
  of the lines.

- pch:

  plot character. A character string or a vector of single characters or
  integers (recycled as neccessary). See
  [`points`](https://rdrr.io/r/graphics/points.html) for all
  alternatives.

- cex:

  numeric vector of character expansion sizes (recycled as neccessary)
  for the plotted symbols.

- col:

  character or integer vector of colors for plotted lines and symbols
  (recycled as neccessary). See
  [`par`](https://rdrr.io/r/graphics/par.html) for the details.

- legendpos:

  Legend position. Optional. If present, a legend is drawn at the given
  position. The position can be specified symbolically (e.g.,
  `legendpos = "topright"`). This requires \>= 2.1.0. Alternatively, the
  position can be specified explicitly (`legendpos = t(c(x,y))`) or
  interactively
  (`legendpos = `[`locator()`](https://rdrr.io/r/graphics/locator.html)).
  This only works well for plots of single-response models.

- xlab, ylab:

  titles for \\x\\ and \\y\\ axes. Typically character strings, but can
  be expressions (e.g., `expression(R^2)` or lists. See
  [`title`](https://rdrr.io/r/graphics/title.html) for details.

- main:

  optional main title for the plot. See Details.

- ask:

  logical. Whether to ask the user before each page of a plot.

## Details

`validationplot` calls the proper validation function (currently
[`MSEP`](https://khliland.github.io/pls/reference/mvrVal.md),
[`RMSEP`](https://khliland.github.io/pls/reference/mvrVal.md) or
[`R2`](https://khliland.github.io/pls/reference/mvrVal.md)) and plots
the results with `plot.mvrVal`. `validationplot` can be called through
the `mvr` plot method, by specifying `plottype = "validation"`.

`plot.mvrVal` creates one plot for each response variable in the model,
laid out in a rectangle. It uses
[`matplot`](https://rdrr.io/r/graphics/matplot.html) for performing the
actual plotting. If `legendpos` is given, a legend is drawn at the given
position.

The argument `main` can be used to specify the main title of the plot.
It is handled in a non-standard way. If there is only on (sub) plot,
`main` will be used as the main title of the plot. If there is *more*
than one (sub) plot, however, the presence of `main` will produce a
corresponding ‘global’ title on the page. Any graphical parametres,
e.g., `cex.main`, supplied to `coefplot` will only affect the ‘ordinary’
plot titles, not the ‘global’ one. Its appearance can be changed by
setting the parameters with
[`par`](https://rdrr.io/r/graphics/par.html), which will affect *both*
titles. (To have different settings for the two titles, one can override
the `par` settings with arguments to the plot function.)

## Note

[`legend`](https://rdrr.io/r/graphics/legend.html) has many options. If
you want greater control over the appearance of the legend, omit the
`legendpos` argument and call `legend` manually.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`plot.mvr`](https://khliland.github.io/pls/reference/plot.mvr.md),
[`RMSEP`](https://khliland.github.io/pls/reference/mvrVal.md),
[`MSEP`](https://khliland.github.io/pls/reference/mvrVal.md),
[`R2`](https://khliland.github.io/pls/reference/mvrVal.md),
[`matplot`](https://rdrr.io/r/graphics/matplot.html),
[`legend`](https://rdrr.io/r/graphics/legend.html)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(oliveoil)
mod <- plsr(sensory ~ chemical, data = oliveoil, validation = "LOO")
if (FALSE) { # \dontrun{
## These three are equivalent:
validationplot(mod, estimate = "all")
plot(mod, "validation", estimate = "all")
plot(RMSEP(mod, estimate = "all"))
## Plot R2:
plot(mod, "validation", val.type = "R2")
## Plot R2, with a legend:
plot(mod, "validation", val.type = "MSEP", legendpos = "top") # R >= 2.1.0
} # }
```
