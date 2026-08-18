# Glance Summary for mvr Models

`glance.mvr` returns one-row summary metrics from an `mvr` model.

## Usage

``` r
glance(x, ...)

# S3 method for class 'mvr'
glance(x, ncomp = x$ncomp, ...)
```

## Arguments

- x:

  an object.

- ...:

  further arguments passed to methods.

- ncomp:

  positive integer. Number of components used for metrics.

## Value

A one-row data frame.

## Examples

``` r
data(yarn)
d <- yarn[1:20, , drop = FALSE]
mod <- plsr(density ~ NIR, ncomp = 2, data = d, validation = "CV")
glance(mod)
#>      method ncomp nobs nresp has_validation r2_train_mean rmsep_train_mean
#> 1 kernelpls     2   20     1           TRUE     0.9829654          3.85935
#>   r2_cv_mean rmsep_cv_mean
#> 1  0.9747301      4.700571
```
