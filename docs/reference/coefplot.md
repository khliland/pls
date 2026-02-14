# Plot Regression Coefficients of PLSR and PCR models

Function to plot the regression coefficients of an `mvr` object.

## Usage

``` r
coefplot(
  object,
  ncomp = object$ncomp,
  comps,
  intercept = FALSE,
  separate = FALSE,
  se.whiskers = FALSE,
  nCols,
  nRows,
  labels,
  type = "l",
  lty,
  lwd = NULL,
  pch,
  cex = NULL,
  col,
  legendpos,
  xlab = "variable",
  ylab = "regression coefficient",
  main,
  pretty.xlabels = TRUE,
  xlim,
  ylim,
  ask = nRows * nCols < nPlots && dev.interactive(),
  ...
)
```

## Arguments

- object:

  an `mvr` object. The fitted model.

- ncomp, comps:

  vector of positive integers. The components to plot. See
  [`coef.mvr`](https://khliland.github.io/pls/reference/coef.mvr.md) for
  details.

- intercept:

  logical. Whether coefficients for the intercept should be plotted.
  Ignored if `comps` is specified. Defaults to `FALSE`. See
  [`coef.mvr`](https://khliland.github.io/pls/reference/coef.mvr.md) for
  details.

- separate:

  logical. If `TRUE`, coefficients for different model sizes are blotted
  in separate plots.

- se.whiskers:

  logical. If `TRUE`, whiskers at plus/minus 1 estimated standard error
  are added to the plot. This is only available if the model was
  cross-validated with `jackknife = TRUE`. Also, in the current
  implementation, `intercept` must be `FALSE`, and `separate` must be
  `TRUE` if `length(ncomp) > 1`.

- nCols, nRows:

  integer. The number of coloumns and rows the plots will be laid out
  in. If not specified, `coefplot` tries to be intelligent.

- labels:

  optional. Alternative \\x\\ axis labels. See Details.

- type:

  character. What type of plot to make. Defaults to `"l"` (lines).
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

  Legend position. Optional. Ignored if `separate` is `TRUE`. If
  present, a legend is drawn at the given position. The position can be
  specified symbolically (e.g., `legendpos = "topright"`). This requires
  \>= 2.1.0. Alternatively, the position can be specified explicitly
  (`legendpos = t(c(x,y))`) or interactively
  (`legendpos = `[`locator()`](https://rdrr.io/r/graphics/locator.html)).
  This only works well for plots of single-response models.

- xlab, ylab:

  titles for \\x\\ and \\y\\ axes. Typically character strings, but can
  be expressions (e.g., `expression(R^2)` or lists. See
  [`title`](https://rdrr.io/r/graphics/title.html) for details.

- main:

  optional main title for the plot. See Details.

- pretty.xlabels:

  logical. If `TRUE`, `coefplot` tries to plot the \\x\\ labels more
  nicely. See Details.

- xlim, ylim:

  optional vector of length two, with the \\x\\ or \\y\\ limits of the
  plot.

- ask:

  logical. Whether to ask the user before each page of a plot.

- ...:

  Further arguments sent to the underlying plot functions.

## Details

`coefplot` handles multiple responses by making one plot for each
response. If `separate` is `TRUE`, separate plots are made for each
combination of model size and response. The plots are laid out in a
rectangular fashion.

If `legendpos` is given, a legend is drawn at the given position (unless
`separate` is `TRUE`).

The argument `labels` can be a vector of labels or one of `"names"` and
`"numbers"`. The labels are used as \\x\\ axis labels. If `labels` is
`"names"` or `"numbers"`, the variable names are used as labels, the
difference being that with `"numbers"`, the variable names are converted
to numbers, if possible. Variable names of the forms `"number"` or
`"number text"` (where the space is optional), are handled.

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
the `par` settings with arguments to `coefplot`.)

The argument `pretty.xlabels` is only used when `labels` is specified.
If `TRUE` (default), the code tries to use a ‘pretty’ selection of
labels. If `labels` is `"numbers"`, it also uses the numerical values of
the labels for horisontal spacing. If one has excluded parts of the
spectral region, one might therefore want to use
`pretty.xlabels = FALSE`.

When `separate` is `TRUE`, the arguments `lty`, `col`, and `pch` default
to their [`par()`](https://rdrr.io/r/graphics/par.html) setting.
Otherwise, the default for all of them is `1:nLines`, where `nLines` is
the number of model sizes specified, i.e., the length of `ncomp` or
`comps`.

The function can also be called through the `mvr` plot method by
specifying `plottype = "coefficients"`.

## Note

[`legend`](https://rdrr.io/r/graphics/legend.html) has many options. If
you want greater control over the appearance of the legend, omit the
`legendpos` argument and call `legend` manually.

The handling of `labels` and `pretty.xlabels` is experimental.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`plot.mvr`](https://khliland.github.io/pls/reference/plot.mvr.md),
[`coef.mvr`](https://khliland.github.io/pls/reference/coef.mvr.md),
[`plot`](https://rdrr.io/r/graphics/plot.default.html),
[`legend`](https://rdrr.io/r/graphics/legend.html)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(yarn)
mod.nir <- plsr(density ~ NIR, ncomp = 8, data = yarn)
if (FALSE) { # \dontrun{
coefplot(mod.nir, ncomp = 1:6)
plot(mod.nir, plottype = "coefficients", ncomp = 1:6) # Equivalent to the previous
## Plot with legend:
coefplot(mod.nir, ncom = 1:6, legendpos = "bottomright")
} # }

data(oliveoil)
mod.sens <- plsr(sensory ~ chemical, ncomp = 4, data = oliveoil)
if (FALSE) coefplot(mod.sens, ncomp = 2:4, separate = TRUE) # \dontrun{}
```
