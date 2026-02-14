# Plot Method for MVR objects

`plot.mvr` plots predictions, coefficients, scores, loadings, biplots,
correlation loadings or validation plots (RMSEP curves, etc.).

## Usage

``` r
# S3 method for class 'mvr'
plot(
  x,
  plottype = c("prediction", "validation", "coefficients", "scores", "loadings",
    "biplot", "correlation"),
  ...
)
```

## Arguments

- x:

  an object of class `mvr`. The fitted model to plot.

- plottype:

  character. What kind of plot to plot.

- ...:

  further arguments, sent to the underlying plot functions.

## Value

`plot.mvr` returns whatever the underlying plot function returns.

## Details

The function is simply a wrapper for the underlying plot functions used
to make the selected plots. See
[`predplot.mvr`](https://khliland.github.io/pls/reference/predplot.md),
[`validationplot`](https://khliland.github.io/pls/reference/validationplot.md),
[`coefplot`](https://khliland.github.io/pls/reference/coefplot.md),
[`scoreplot`](https://khliland.github.io/pls/reference/scoreplot.md),
[`loadingplot`](https://khliland.github.io/pls/reference/scoreplot.md),
[`biplot.mvr`](https://khliland.github.io/pls/reference/biplot.mvr.md)
or [`corrplot`](https://khliland.github.io/pls/reference/scoreplot.md)
for details. Note that all arguments except `x` and `plottype` must be
named.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`predplot.mvr`](https://khliland.github.io/pls/reference/predplot.md),
[`validationplot`](https://khliland.github.io/pls/reference/validationplot.md),
[`coefplot`](https://khliland.github.io/pls/reference/coefplot.md),
[`scoreplot`](https://khliland.github.io/pls/reference/scoreplot.md),
[`loadingplot`](https://khliland.github.io/pls/reference/scoreplot.md),
[`biplot.mvr`](https://khliland.github.io/pls/reference/biplot.mvr.md),
[`corrplot`](https://khliland.github.io/pls/reference/scoreplot.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(yarn)
nir.pcr <- pcr(density ~ NIR, ncomp = 9, data = yarn, validation = "CV")
if (FALSE) { # \dontrun{
plot(nir.pcr, ncomp = 5) # Plot of cross-validated predictions
plot(nir.pcr, "scores") # Score plot
plot(nir.pcr, "loadings", comps = 1:3) # The three first loadings
plot(nir.pcr, "coef", ncomp = 5) # Coefficients
plot(nir.pcr, "val") # RMSEP curves
plot(nir.pcr, "val", val.type = "MSEP", estimate = "CV") # CV MSEP
} # }
```
