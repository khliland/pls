# Tidy mvr Model Components

`tidy.mvr` returns model components as a long data frame for plotting
and tabulation.

## Usage

``` r
tidy(x, ...)

# S3 method for class 'mvr'
tidy(
  x,
  what = c("weights", "loadings", "coefficients"),
  ncomp = x$ncomp,
  intercept = FALSE,
  ...
)

# S3 method for class 'mvrVal'
tidy(x, ...)
```

## Arguments

- x:

  an object.

- ...:

  further arguments passed to methods.

- what:

  character vector. Any subset of
  `c("weights", "loadings", "coefficients")`. Defaults to all.

- ncomp:

  positive integer. Number of components for coefficient output.

- intercept:

  logical. Include intercept in coefficient output.

## Value

A data frame.

## Examples

``` r
data(yarn)
d <- yarn[1:20, , drop = FALSE]
mod <- plsr(density ~ NIR, ncomp = 2, data = d, validation = "CV")

td <- tidy(mod, what = c("weights", "loadings", "coefficients"))
head(td)
#>      kind name response component        value
#> 1 weights NIR1     <NA>         1 0.0008485053
#> 2 weights NIR2     <NA>         1 0.0122387159
#> 3 weights NIR3     <NA>         1 0.0277711693
#> 4 weights NIR4     <NA>         1 0.0404800917
#> 5 weights NIR5     <NA>         1 0.0430030762
#> 6 weights NIR6     <NA>         1 0.0355023891

rm <- RMSEP(mod, estimate = c("train", "CV"))
td_rm <- tidy(rm)
head(td_rm)
#>   metric estimate response component       model cumulative     value
#> 1  RMSEP    train  density         0 (Intercept)       TRUE 29.569810
#> 2  RMSEP       CV  density         0 (Intercept)       TRUE 31.126116
#> 3  RMSEP    train  density         1     1 comps       TRUE  6.141607
#> 4  RMSEP       CV  density         1     1 comps       TRUE  7.393413
#> 5  RMSEP    train  density         2     2 comps       TRUE  3.859350
#> 6  RMSEP       CV  density         2     2 comps       TRUE  4.782807
```
