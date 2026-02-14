# MSEP, RMSEP and R2 of PLSR and PCR models

Functions to estimate the mean squared error of prediction (MSEP), root
mean squared error of prediction (RMSEP) and \\R^2\\ (A.K.A. coefficient
of multiple determination) for fitted PCR and PLSR models. Test-set,
cross-validation and calibration-set estimates are implemented.

## Usage

``` r
mvrValstats(
  object,
  estimate,
  newdata,
  ncomp = 1:object$ncomp,
  comps,
  intercept = cumulative,
  se = FALSE,
  ...
)

R2(object, ...)

# S3 method for class 'mvr'
R2(
  object,
  estimate,
  newdata,
  ncomp = 1:object$ncomp,
  comps,
  intercept = cumulative,
  se = FALSE,
  ...
)

MSEP(object, ...)

# S3 method for class 'mvr'
MSEP(
  object,
  estimate,
  newdata,
  ncomp = 1:object$ncomp,
  comps,
  intercept = cumulative,
  se = FALSE,
  ...
)

RMSEP(object, ...)

# S3 method for class 'mvr'
RMSEP(object, ...)
```

## Arguments

- object:

  an `mvr` object

- estimate:

  a character vector. Which estimators to use. Should be a subset of
  `c("all", "train", "CV", "adjCV", "test")`. `"adjCV"` is only
  available for (R)MSEP. See below for how the estimators are chosen.

- newdata:

  a data frame with test set data.

- ncomp, comps:

  a vector of positive integers. The components or number of components
  to use. See below.

- intercept:

  logical. Whether estimates for a model with zero components should be
  returned as well.

- se:

  logical. Whether estimated standard errors of the estimates should be
  calculated. Not implemented yet.

- ...:

  further arguments sent to underlying functions or (for `RMSEP`) to
  `MSEP`

## Details

`RMSEP` simply calls `MSEP` and takes the square root of the estimates.
It therefore accepts the same arguments as `MSEP`.

Several estimators can be used. `"train"` is the training or calibration
data estimate, also called (R)MSEC. For `R2`, this is the unadjusted
\\R^2\\. It is overoptimistic and should not be used for assessing
models. `"CV"` is the cross-validation estimate, and `"adjCV"` (for
`RMSEP` and `MSEP`) is the bias-corrected cross-validation estimate.
They can only be calculated if the model has been cross-validated.
Finally, `"test"` is the test set estimate, using `newdata` as test set.

Which estimators to use is decided as follows (see below for
`mvrValstats`). If `estimate` is not specified, the test set estimate is
returned if `newdata` is specified, otherwise the CV and adjusted CV
(for `RMSEP` and `MSEP`) estimates if the model has been
cross-validated, otherwise the training data estimate. If `estimate` is
`"all"`, all possible estimates are calculated. Otherwise, the specified
estimates are calculated.

Several model sizes can also be specified. If `comps` is missing (or is
`NULL`), `length(ncomp)` models are used, with `ncomp[1]` components,
..., `ncomp[length(ncomp)]` components. Otherwise, a single model with
the components `comps[1]`, ..., `comps[length(comps)]` is used. If
`intercept` is `TRUE`, a model with zero components is also used (in
addition to the above).

The \\R^2\\ values returned by `"R2"` are calculated as \\1 - SSE/SST\\,
where \\SST\\ is the (corrected) total sum of squares of the response,
and \\SSE\\ is the sum of squared errors for either the fitted values
(i.e., the residual sum of squares), test set predictions or
cross-validated predictions (i.e., the \\PRESS\\). For
`estimate = "train"`, this is equivalent to the squared correlation
between the fitted values and the response. For `estimate = "train"`,
the estimate is often called the prediction \\R^2\\.

`mvrValstats` is a utility function that calculates the statistics
needed by `MSEP` and `R2`. It is not intended to be used interactively.
It accepts the same arguments as `MSEP` and `R2`. However, the
`estimate` argument must be specified explicitly: no partial matching
and no automatic choice is made. The function simply calculates the
types of estimates it knows, and leaves the other untouched.

## Value

`mvrValstats` returns a list with components

- SSE:

  three-dimensional array of SSE values. The first dimension is the
  different estimators, the second is the response variables and the
  third is the models.

- SST:

  matrix of SST values. The first dimension is the different estimators
  and the second is the response variables.

- nobj:

  a numeric vector giving the number of objects used for each estimator.

- comps:

  the components specified, with `0` prepended if `intercept` is `TRUE`.

- cumulative:

  `TRUE` if `comps` was `NULL` or not specified.

The other functions return an object of class `"mvrVal"`, with
components

- val:

  three-dimensional array of estimates. The first dimension is the
  different estimators, the second is the response variables and the
  third is the models.

- type:

  `"MSEP"`, `"RMSEP"` or `"R2"`.

- comps:

  the components specified, with `0` prepended if `intercept` is `TRUE`.

- cumulative:

  `TRUE` if `comps` was `NULL` or not specified.

- call:

  the function call

## References

Mevik, B.-H., Cederkvist, H. R. (2004) Mean Squared Error of Prediction
(MSEP) Estimates for Principal Component Regression (PCR) and Partial
Least Squares Regression (PLSR). *Journal of Chemometrics*, **18**(9),
422–429.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`crossval`](https://khliland.github.io/pls/reference/crossval.md),
[`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md),
[`validationplot`](https://khliland.github.io/pls/reference/validationplot.md),
[`plot.mvrVal`](https://khliland.github.io/pls/reference/validationplot.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(oliveoil)
mod <- plsr(sensory ~ chemical, ncomp = 4, data = oliveoil, validation = "LOO")
RMSEP(mod)
#> 
#> Response: yellow 
#>        (Intercept)  1 comps  2 comps  3 comps  4 comps
#> CV            20.1    18.97    16.10    16.71    18.11
#> adjCV         20.1    18.91    16.03    16.61    17.93
#> 
#> Response: green 
#>        (Intercept)  1 comps  2 comps  3 comps  4 comps
#> CV           24.26    23.88    20.45    21.35    23.96
#> adjCV        24.26    23.80    20.35    21.20    23.70
#> 
#> Response: brown 
#>        (Intercept)  1 comps  2 comps  3 comps  4 comps
#> CV           5.297    4.019    3.987    3.987    4.107
#> adjCV        5.297    3.990    3.955    3.947    4.050
#> 
#> Response: glossy 
#>        (Intercept)  1 comps  2 comps  3 comps  4 comps
#> CV           6.391    5.109    5.161    5.571    6.446
#> adjCV        6.391    5.087    5.129    5.522    6.363
#> 
#> Response: transp 
#>        (Intercept)  1 comps  2 comps  3 comps  4 comps
#> CV            8.58    7.258    7.158    7.665    8.794
#> adjCV         8.58    7.232    7.118    7.607    8.691
#> 
#> Response: syrup 
#>        (Intercept)  1 comps  2 comps  3 comps  4 comps
#> CV           3.166    2.134    2.325    2.478    2.939
#> adjCV        3.166    2.128    2.310    2.458    2.901
if (FALSE) plot(R2(mod)) # \dontrun{}
```
