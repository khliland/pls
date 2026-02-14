# CPPLS (Indahl et al.)

Fits a PLS model using the CPPLS algorithm.

## Usage

``` r
cppls.fit(
  X,
  Y,
  ncomp,
  Y.add = NULL,
  center = TRUE,
  stripped = FALSE,
  lower = 0.5,
  upper = 0.5,
  trunc.pow = FALSE,
  weights = NULL,
  ...
)
```

## Arguments

- X:

  a matrix of observations. `NA`s and `Inf`s are not allowed.

- Y:

  a vector or matrix of responses. `NA`s and `Inf`s are not allowed.

- ncomp:

  the number of components to be used in the modelling.

- Y.add:

  a vector or matrix of additional responses containing relevant
  information about the observations.

- center:

  logical, determines if the \\X\\ and \\Y\\ matrices are mean centered
  or not. Default is to perform mean centering.

- stripped:

  logical. If `TRUE` the calculations are stripped as much as possible
  for speed; this is meant for use with cross-validation or simulations
  when only the coefficients are needed. Defaults to `FALSE`.

- lower:

  a vector of lower limits for power optimisation. Defaults to `0.5`.

- upper:

  a vector of upper limits for power optimisation. Defaults to `0.5`.

- trunc.pow:

  logical. If `TRUE` an experimental alternative power algorithm is
  used. (Optional)

- weights:

  a vector of individual weights for the observations. (Optional)

- ...:

  other arguments. Currently ignored.

## Value

A list containing the following components is returned:

- coefficients:

  an array of regression coefficients for 1, ..., `ncomp` components.
  The dimensions of `coefficients` are `c(nvar, npred, ncomp)` with
  `nvar` the number of `X` variables and `npred` the number of variables
  to be predicted in `Y`.

- scores:

  a matrix of scores.

- loadings:

  a matrix of loadings.

- loading.weights:

  a matrix of loading weights.

- Yscores:

  a matrix of Y-scores.

- Yloadings:

  a matrix of Y-loadings.

- projection:

  the projection matrix used to convert X to scores.

- Xmeans:

  a vector of means of the X variables.

- Ymeans:

  a vector of means of the Y variables.

- fitted.values:

  an array of fitted values. The dimensions of `fitted.values` are
  `c(nobj, npred, ncomp)` with `nobj` the number samples and `npred` the
  number of Y variables.

- residuals:

  an array of regression residuals. It has the same dimensions as
  `fitted.values`.

- Xvar:

  a vector with the amount of X-variance explained by each component.

- Xtotvar:

  total variance in `X`.

- gammas:

  gamma-values obtained in power optimisation.

- canonical.correlations:

  Canonical correlation values from the calculations of loading weights.

- A:

  matrix containing vectors of weights `a` from canonical correlation
  (`cor(Za,Yb)`).

- smallNorms:

  vector of indices of explanatory variables of length close to or equal
  to 0.

If `stripped` is `TRUE`, only the components `coefficients`, `Xmeans`,
`Ymeans` and `gammas` are returned.

## Details

This function should not be called directly, but through the generic
functions `cppls` or `mvr` with the argument `method="cppls"`. Canonical
Powered PLS (CPPLS) is a generalisation of PLS incorporating discrete
and continuous responses (also simultaneously), additional responses,
individual weighting of observations and power methodology for
sharpening focus on groups of variables. Depending on the input to
`cppls` it can produce the following special cases:

- PLS: uni-response continuous `Y`

- PPLS: uni-response continuous `Y`, `(lower || upper) != 0.5`

- PLS-DA (using correlation maximisation - B/W): dummy-coded descrete
  response `Y`

- PPLS-DA: dummy-coded descrete response `Y`, `(lower || upper) != 0.5`

- CPLS: multi-response `Y` (continuous, discrete or combination)

- CPPLS: multi-response `Y` (continuous, discrete or combination),
  `(lower || upper) != 0.5`

The name "canonical" comes from canonical correlation analysis which is
used when calculating vectors of loading weights, while "powered" refers
to a reparameterisation of the vectors of loading weights which can be
optimised over a given interval.

## References

Indahl, U. (2005) A twist to partial least squares regression. *Journal
of Chemometrics*, **19**, 32–44.

Liland, K.H and Indahl, U.G (2009) Powered partial least squares
discriminant analysis, *Journal of Chemometrics*, **23**, 7–18.

Indahl, U.G., Liland, K.H. and Næs, T. (2009) Canonical partial least
squares - a unified PLS approach to classification and regression
problems. *Journal of Chemometrics*, **23**, 495–504.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md)
[`plsr`](https://khliland.github.io/pls/reference/mvr.md)
[`pcr`](https://khliland.github.io/pls/reference/mvr.md)
[`widekernelpls.fit`](https://khliland.github.io/pls/reference/widekernelpls.fit.md)
[`simpls.fit`](https://khliland.github.io/pls/reference/simpls.fit.md)
[`oscorespls.fit`](https://khliland.github.io/pls/reference/oscorespls.fit.md)

## Author

Kristian Hovde Liland

## Examples

``` r
data(mayonnaise)
# Create dummy response
mayonnaise$dummy <-
    I(model.matrix(~y-1, data.frame(y = factor(mayonnaise$oil.type))))

# Predict CPLS scores for test data
may.cpls <- cppls(dummy ~ NIR, 10, data = mayonnaise, subset = train)
may.test <- predict(may.cpls, newdata = mayonnaise[!mayonnaise$train,], type = "score")

# Predict CPLS scores for test data (experimental used design as additional Y information)
may.cpls.yadd <- cppls(dummy ~ NIR, 10, data = mayonnaise, subset = train, Y.add=design)
may.test.yadd <- predict(may.cpls.yadd, newdata = mayonnaise[!mayonnaise$train,], type = "score")

# Classification by linear discriminant analysis (LDA)
library(MASS)
error <- matrix(ncol = 10, nrow = 2)
dimnames(error) <- list(Model = c('CPLS', 'CPLS (Y.add)'), ncomp = 1:10)
for (i in 1:10) {
    fitdata1 <- data.frame(oil.type = mayonnaise$oil.type[mayonnaise$train],
                           NIR.score = I(may.cpls$scores[,1:i,drop=FALSE]))
    testdata1 <- data.frame(oil.type = mayonnaise$oil.type[!mayonnaise$train],
                            NIR.score = I(may.test[,1:i,drop=FALSE]))
    error[1,i] <-
        (42 - sum(predict(lda(oil.type ~ NIR.score, data = fitdata1),
                  newdata = testdata1)$class == testdata1$oil.type)) / 42
    fitdata2 <- data.frame(oil.type = mayonnaise$oil.type[mayonnaise$train],
                           NIR.score = I(may.cpls.yadd$scores[,1:i,drop=FALSE]))
    testdata2 <- data.frame(oil.type = mayonnaise$oil.type[!mayonnaise$train],
                            NIR.score = I(may.test.yadd[,1:i,drop=FALSE]))
    error[2,i] <-
        (42 - sum(predict(lda(oil.type ~ NIR.score, data = fitdata2),
                  newdata = testdata2)$class == testdata2$oil.type)) / 42
}
round(error,2)
#>               ncomp
#> Model             1    2    3    4    5    6    7    8 9 10
#>   CPLS         0.29 0.29 0.19 0.17 0.24 0.14 0.05 0.02 0  0
#>   CPLS (Y.add) 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0  0
```
