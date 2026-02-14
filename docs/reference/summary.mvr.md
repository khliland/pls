# Summary and Print Methods for PLSR and PCR objects

Summary and print methods for `mvr` and `mvrVal` objects.

## Usage

``` r
# S3 method for class 'mvr'
print(x, ...)

# S3 method for class 'mvr'
summary(
  object,
  what = c("all", "validation", "training"),
  digits = 4,
  print.gap = 2,
  ...
)

# S3 method for class 'mvrVal'
print(x, digits = 4, print.gap = 2, ...)

# S3 method for class 'mvrVal'
as.data.frame(x, row.names = NULL, optional = FALSE, shortAlgs = TRUE, ...)
```

## Arguments

- x, object:

  an `mvr` object

- ...:

  Other arguments sent to underlying methods.

- what:

  one of `"all"`, `"validation"` or `"training"`

- digits:

  integer. Minimum number of significant digits in the output. Default
  is 4.

- print.gap:

  Integer. Gap between coloumns of the printed tables.

- row.names:

  NULL or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  Not used, only included to match signature of `as.data.frame`.

- shortAlgs:

  Logical. Shorten algorithm names (default = TRUE).

## Value

`print.mvr` and `print.mvrVal` return the object invisibly.

## Details

If `what` is `"training"`, the explained variances are given; if it is
`"validation"`, the cross-validated RMSEPs (if available) are given; if
it is `"all"`, both are given.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`pcr`](https://khliland.github.io/pls/reference/mvr.md),
[`plsr`](https://khliland.github.io/pls/reference/mvr.md),
[`RMSEP`](https://khliland.github.io/pls/reference/mvrVal.md),
[`MSEP`](https://khliland.github.io/pls/reference/mvrVal.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(yarn)
nir.mvr <- mvr(density ~ NIR, ncomp = 8, validation = "LOO", data = yarn)
nir.mvr
#> Partial least squares regression, fitted with the kernel algorithm.
#> Cross-validated using 28 leave-one-out segments.
#> Call:
#> mvr(formula = density ~ NIR, ncomp = 8, data = yarn, validation = "LOO")
summary(nir.mvr)
#> Data:    X dimension: 28 268 
#>  Y dimension: 28 1
#> Fit method: kernelpls
#> Number of components considered: 8
#> 
#> VALIDATION: RMSEP
#> Cross-validated using 28 leave-one-out segments.
#>        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
#> CV           27.46    4.600    3.900    2.090   0.7686   0.5004   0.4425
#> adjCV        27.46    4.454    3.973    2.084   0.7570   0.4967   0.4398
#>        7 comps  8 comps
#> CV      0.2966   0.2643
#> adjCV   0.2926   0.2610
#> 
#> TRAINING: % variance explained
#>          1 comps  2 comps  3 comps  4 comps  5 comps  6 comps  7 comps  8 comps
#> X          46.83    98.38    99.46    99.67    99.85    99.97    99.98    99.99
#> density    98.12    98.25    99.64    99.97    99.99    99.99   100.00   100.00
RMSEP(nir.mvr)
#>        (Intercept)  1 comps  2 comps  3 comps  4 comps  5 comps  6 comps
#> CV           27.46    4.600    3.900    2.090   0.7686   0.5004   0.4425
#> adjCV        27.46    4.454    3.973    2.084   0.7570   0.4967   0.4398
#>        7 comps  8 comps
#> CV      0.2966   0.2643
#> adjCV   0.2926   0.2610
# Extract MVR validation statistics as data.frame:
as.data.frame(RMSEP(nir.mvr, estimate = "CV"))
#>   estimate response comps validation method algorithm      value
#> 1       CV  density     0        LOO    mvr    kernel 27.4569014
#> 2       CV  density     1        LOO    mvr    kernel  4.6000678
#> 3       CV  density     2        LOO    mvr    kernel  3.8997929
#> 4       CV  density     3        LOO    mvr    kernel  2.0898916
#> 5       CV  density     4        LOO    mvr    kernel  0.7686120
#> 6       CV  density     5        LOO    mvr    kernel  0.5003516
#> 7       CV  density     6        LOO    mvr    kernel  0.4424918
#> 8       CV  density     7        LOO    mvr    kernel  0.2965848
#> 9       CV  density     8        LOO    mvr    kernel  0.2642607
as.data.frame(R2(nir.mvr))
#>   estimate response comps validation method algorithm       value
#> 1       CV  density     0        LOO    mvr    kernel -0.07544582
#> 2       CV  density     1        LOO    mvr    kernel  0.96981342
#> 3       CV  density     2        LOO    mvr    kernel  0.97830455
#> 4       CV  density     3        LOO    mvr    kernel  0.99376936
#> 5       CV  density     4        LOO    mvr    kernel  0.99915725
#> 6       CV  density     5        LOO    mvr    kernel  0.99964286
#> 7       CV  density     6        LOO    mvr    kernel  0.99972068
#> 8       CV  density     7        LOO    mvr    kernel  0.99987452
#> 9       CV  density     8        LOO    mvr    kernel  0.99990038
```
