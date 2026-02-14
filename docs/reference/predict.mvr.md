# Predict Method for PLSR and PCR

Prediction for mvr (PCR, PLSR) models. New responses or scores are
predicted using a fitted model and a new matrix of observations.

## Usage

``` r
# S3 method for class 'mvr'
predict(
  object,
  newdata,
  ncomp = 1:object$ncomp,
  comps,
  type = c("response", "scores"),
  na.action = na.pass,
  ...
)
```

## Arguments

- object:

  an `mvr` object. The fitted model

- newdata:

  a data frame. The new data. If missing, the training data is used.

- ncomp, comps:

  vector of positive integers. The components to use in the prediction.
  See below.

- type:

  character. Whether to predict scores or response values

- na.action:

  function determining what should be done with missing values in
  `newdata`. The default is to predict `NA`. See
  [`na.omit`](https://rdrr.io/r/stats/na.fail.html) for alternatives.

- ...:

  further arguments. Currently not used

## Value

When `type` is `"response"`, a three dimensional array of predicted
response values is returned. The dimensions correspond to the
observations, the response variables and the model sizes, respectively.

When `type` is `"scores"`, a score matrix is returned.

## Details

When `type` is `"response"` (default), predicted response values are
returned. If `comps` is missing (or is `NULL`), predictions for
`length(ncomp)` models with `ncomp[1]` components, `ncomp[2]`
components, etc., are returned. Otherwise, predictions for a single
model with the exact components in `comps` are returned. (Note that in
both cases, the intercept is always included in the predictions. It can
be removed by subtracting the `Ymeans` component of the fitted model.)

When `type` is `"scores"`, predicted score values are returned for the
components given in `comps`. If `comps` is missing or `NULL`, `ncomps`
is used instead.

It is also possible to supply a matrix instead of a data frame as
`newdata`, which is then assumed to be the \\X\\ data matrix. Note that
the usual checks for the type of the data are then omitted. Also note
that this is *only* possible with `predict`; it will not work in
functions like
[`predplot`](https://khliland.github.io/pls/reference/predplot.md),
[`RMSEP`](https://khliland.github.io/pls/reference/mvrVal.md) or
[`R2`](https://khliland.github.io/pls/reference/mvrVal.md), because they
also need the response variable of the new data.

## Note

A warning message like
`'newdata' had 10 rows but variable(s) found have 106 rows` means that
not all variables were found in the `newdata` data frame. This (usually)
happens if the formula contains terms like `yarn$NIR`. Do not use such
terms; use the `data` argument instead. See
[`mvr`](https://khliland.github.io/pls/reference/mvr.md) for details.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`summary.mvr`](https://khliland.github.io/pls/reference/summary.mvr.md),
[`coef.mvr`](https://khliland.github.io/pls/reference/coef.mvr.md),
[`plot.mvr`](https://khliland.github.io/pls/reference/plot.mvr.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(yarn)
nir.mvr <- mvr(density ~ NIR, ncomp = 5, data = yarn[yarn$train,])

## Predicted responses for models with 1, 2, 3 and 4 components
pred.resp <- predict(nir.mvr, ncomp = 1:4, newdata = yarn[!yarn$train,])

## Predicted responses for a single model with components 1, 2, 3, 4
predict(nir.mvr, comps = 1:4, newdata = yarn[!yarn$train,])
#>      density
#> 110 51.04992
#> 22  50.72019
#> 31  32.01454
#> 41  34.29076
#> 51  30.35994
#> 61  20.57832
#> 71  19.07786

## Predicted scores
predict(nir.mvr, comps = 1:3, type = "scores", newdata = yarn[!yarn$train,])
#>          Comp 1     Comp 2     Comp 3
#> 110  1.54411104  0.6112014 -0.2783797
#> 22   1.54887982 -0.3204970 -0.1990664
#> 31   0.03391126  1.4202763 -0.3941334
#> 41   0.30033040  0.2952664 -0.3835675
#> 51  -0.06780681 -1.1572169 -0.1909823
#> 61  -1.04444815  1.3565774 -0.1926097
#> 71  -0.90185306 -0.4988215 -0.3715940
```
