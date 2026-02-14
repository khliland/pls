# Standardization of Data Matrices

Performs standardization (centering and scaling) of a data matrix.

## Usage

``` r
stdize(x, center = TRUE, scale = TRUE)

# S3 method for class 'stdized'
predict(object, newdata, ...)

# S3 method for class 'stdized'
makepredictcall(var, call)
```

## Arguments

- x, newdata:

  numeric matrices. The data to standardize.

- center:

  logical value or numeric vector of length equal to the number of
  coloumns of `x`.

- scale:

  logical value or numeric vector of length equal to the number of
  coloumns of `x`.

- object:

  an object inheriting from class `"stdized"`, normally the result of a
  call to `stdize`.

- ...:

  other arguments. Currently ignored.

- var:

  A variable.

- call:

  The term in the formula, as a call.

## Value

Both `stdize` and `predict.stdized` return a scaled and/or centered
matrix, with attributes `"stdized:center"` and/or `"stdized:scale"` the
vector used for centering and/or scaling. The matrix is given class
`c("stdized", "matrix")`.

## Details

`makepredictcall.stdized` is an internal utility function; it is not
meant for interactive use. See
[`makepredictcall`](https://rdrr.io/r/stats/makepredictcall.html) for
details.

If `center` is `TRUE`, `x` is centered by subtracting the coloumn mean
from each coloumn. If `center` is a numeric vector, it is used in place
of the coloumn means.

If `scale` is `TRUE`, `x` is scaled by dividing each coloumn by its
sample standard deviation. If `scale` is a numeric vector, it is used in
place of the standard deviations.

## Note

`stdize` is very similar to
[`scale`](https://rdrr.io/r/base/scale.html). The difference is that
when `scale = TRUE`, `stdize` divides the coloumns by their standard
deviation, while `scale` uses the root-mean-square of the coloumns. If
`center` is `TRUE`, this is equivalent, but in general it is not.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`pcr`](https://khliland.github.io/pls/reference/mvr.md),
[`plsr`](https://khliland.github.io/pls/reference/mvr.md),
[`msc`](https://khliland.github.io/pls/reference/msc.md),
[`scale`](https://rdrr.io/r/base/scale.html)

## Author

Bjørn-Helge Mevik and Ron Wehrens

## Examples

``` r
data(yarn)
## Direct standardization:
Ztrain <- stdize(yarn$NIR[yarn$train,])
Ztest <- predict(Ztrain, yarn$NIR[!yarn$train,])

## Used in formula:
mod <- plsr(density ~ stdize(NIR), ncomp = 6, data = yarn[yarn$train,])
pred <- predict(mod, newdata = yarn[!yarn$train,]) # Automatically standardized
```
