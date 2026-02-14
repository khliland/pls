# Biplots of PLSR and PCR Models.

Biplot method for `mvr` objects.

## Usage

``` r
# S3 method for class 'mvr'
biplot(
  x,
  comps = 1:2,
  which = c("x", "y", "scores", "loadings"),
  var.axes = FALSE,
  xlabs,
  ylabs,
  main,
  ...
)
```

## Arguments

- x:

  an `mvr` object.

- comps:

  integer vector of length two. The components to plot.

- which:

  character. Which matrices to plot. One of `"x"` (X scores and
  loadings), `"y"` (Y scores and loadings), `"scores"` (X and Y scores)
  and `"loadings"` (X and Y loadings).

- var.axes:

  logical. If `TRUE`, the second set of points have arrows representing
  them.

- xlabs:

  either a character vector of labels for the first set of points, or
  `FALSE` for no labels. If missing, the row names of the first matrix
  is used as labels.

- ylabs:

  either a character vector of labels for the second set of points, or
  `FALSE` for no labels. If missing, the row names of the second matrix
  is used as labels.

- main:

  character. Title of plot. If missing, a title is constructed by
  `biplot.mvr`.

- ...:

  Further arguments passed on to `biplot.default`.

## Details

`biplot.mvr` can also be called through the `mvr` plot method by
specifying `plottype = "biplot"`.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`plot.mvr`](https://khliland.github.io/pls/reference/plot.mvr.md),
[`biplot.default`](https://rdrr.io/r/stats/biplot.html)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(oliveoil)
mod <- plsr(sensory ~ chemical, data = oliveoil)
if (FALSE) { # \dontrun{
## These are equivalent
biplot(mod)
plot(mod, plottype = "biplot")

## The four combinations of x and y points:
par(mfrow = c(2,2))
biplot(mod, which = "x") # Default
biplot(mod, which = "y")
biplot(mod, which = "scores")
biplot(mod, which = "loadings")
} # }
```
