# Orthogonal scores PLSR

Fits a PLSR model with the orthogonal scores algorithm (aka the NIPALS
algorithm).

## Usage

``` r
oscorespls.fit(
  X,
  Y,
  ncomp,
  center = TRUE,
  stripped = FALSE,
  tol = .Machine$double.eps^0.5,
  maxit = 100,
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

- center:

  logical, determines if the \\X\\ and \\Y\\ matrices are mean centered
  or not. Default is to perform mean centering.

- stripped:

  logical. If `TRUE` the calculations are stripped as much as possible
  for speed; this is meant for use with cross-validation or simulations
  when only the coefficients are needed. Defaults to `FALSE`.

- tol:

  numeric. The tolerance used for determining convergence in
  multi-response models.

- maxit:

  positive integer. The maximal number of iterations used in the
  internal Eigenvector calculation.

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

  Total variance in `X`.

If `stripped` is `TRUE`, only the components `coefficients`, `Xmeans`
and `Ymeans` are returned.

## Details

This function should not be called directly, but through the generic
functions `plsr` or `mvr` with the argument `method="oscorespls"`. It
implements the orthogonal scores algorithm, as described in Martens and
Næs (1989). This is one of the two “classical” PLSR algorithms, the
other being the orthogonal loadings algorithm.

## References

Martens, H., Næs, T. (1989) *Multivariate calibration.* Chichester:
Wiley.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md)
[`plsr`](https://khliland.github.io/pls/reference/mvr.md)
[`pcr`](https://khliland.github.io/pls/reference/mvr.md)
[`kernelpls.fit`](https://khliland.github.io/pls/reference/kernelpls.fit.md)
[`widekernelpls.fit`](https://khliland.github.io/pls/reference/widekernelpls.fit.md)
[`simpls.fit`](https://khliland.github.io/pls/reference/simpls.fit.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik
