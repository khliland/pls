# Adjust for Missing Values

Use missing value information to adjust residuals and predictions. This
is the ‘mvr equivalent’ of the `naresid.exclude` and `napredict.exclude`
functions.

## Usage

``` r
naExcludeMvr(omit, x, ...)
```

## Arguments

- omit:

  an object produced by an `na.action` function, typically the
  `"na.action"` attribute of the result of `na.omit` or `na.exclude`.

- x:

  a three-dimensional array to be adjusted based upon the missing value
  information in `omit`.

- ...:

  further arguments. Currently not used.

## Value

`x`, padded with `NA`s along the first dimension (‘rows’).

## Details

This is a utility function used to allow `predict.mvr` and
`residuals.mvr` to compensate for the removal of `NA`s in the fitting
process.

It is called only when the `na.action` is `na.exclude`, and pads `x`
with `NA`s in the correct positions to have the same number of rows as
the original data frame.

## See also

[`predict.mvr`](https://khliland.github.io/pls/reference/predict.mvr.md),
[`residuals.mvr`](https://khliland.github.io/pls/reference/coef.mvr.md),
[`napredict`](https://rdrr.io/r/stats/nafns.html),
[`naresid`](https://rdrr.io/r/stats/nafns.html)

## Author

Bjørn-Helge Mevik and Ron Wehrens
