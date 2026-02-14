# Multiplicative Scatter Correction

Performs multiplicative scatter/signal correction on a data matrix.

## Usage

``` r
msc(X, reference = NULL)

# S3 method for class 'msc'
predict(object, newdata, ...)

# S3 method for class 'msc'
makepredictcall(var, call)
```

## Arguments

- X, newdata:

  numeric matrices. The data to scatter correct.

- reference:

  numeric vector. Spectre to use as reference. If `NULL`, the column
  means of `X` are used.

- object:

  an object inheriting from class `"msc"`, normally the result of a call
  to `msc` with a single matrix argument.

- ...:

  other arguments. Currently ignored.

- var:

  A variable.

- call:

  The term in the formula, as a call.

## Value

Both `msc` and `predict.msc` return a multiplicative scatter corrected
matrix, with attribute `"reference"` the vector used as reference
spectre. The matrix is given class `c("msc", "matrix")`. For
`predict.msc`, the `"reference"` attribute of `object` is used as
reference spectre.

## Details

`makepredictcall.msc` is an internal utility function; it is not meant
for interactive use. See
[`makepredictcall`](https://rdrr.io/r/stats/makepredictcall.html) for
details.

## References

Martens, H., Næs, T. (1989) *Multivariate calibration.* Chichester:
Wiley.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`pcr`](https://khliland.github.io/pls/reference/mvr.md),
[`plsr`](https://khliland.github.io/pls/reference/mvr.md),
[`stdize`](https://khliland.github.io/pls/reference/stdize.md)

## Author

Bjørn-Helge Mevik and Ron Wehrens

## Examples

``` r
data(yarn)
## Direct correction:
Ztrain <- msc(yarn$NIR[yarn$train,])
Ztest <- predict(Ztrain, yarn$NIR[!yarn$train,])

## Used in formula:
mod <- plsr(density ~ msc(NIR), ncomp = 6, data = yarn[yarn$train,])
pred <- predict(mod, newdata = yarn[!yarn$train,]) # Automatically scatter corrected
```
