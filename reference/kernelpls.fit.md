# Kernel PLS (Dayal and MacGregor)

Fits a PLSR model with the kernel algorithm.

## Usage

``` r
kernelpls.fit(X, Y, ncomp, center = TRUE, stripped = FALSE, ...)
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
functions `plsr` or `mvr` with the argument `method="kernelpls"`
(default). Kernel PLS is particularly efficient when the number of
objects is (much) larger than the number of variables. The results are
equal to the NIPALS algorithm. Several different forms of kernel PLS
have been described in literature, e.g. by De Jong and Ter Braak, and
two algorithms by Dayal and MacGregor. This function implements the
fastest of the latter, not calculating the crossproduct matrix of X. In
the Dyal & MacGregor paper, this is “algorithm 1”.

## References

de Jong, S. and ter Braak, C. J. F. (1994) Comments on the PLS kernel
algorithm. *Journal of Chemometrics*, **8**, 169–174.

Dayal, B. S. and MacGregor, J. F. (1997) Improved PLS algorithms.
*Journal of Chemometrics*, **11**, 73–85.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md)
[`plsr`](https://khliland.github.io/pls/reference/mvr.md)
[`cppls`](https://khliland.github.io/pls/reference/mvr.md)
[`pcr`](https://khliland.github.io/pls/reference/mvr.md)
[`widekernelpls.fit`](https://khliland.github.io/pls/reference/widekernelpls.fit.md)
[`simpls.fit`](https://khliland.github.io/pls/reference/simpls.fit.md)
[`oscorespls.fit`](https://khliland.github.io/pls/reference/oscorespls.fit.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik
