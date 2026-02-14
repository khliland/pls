# Calculate Variance-Covariance Matrix for a Fitted Model Object

Returns the variance-covariance matrix of the coefficients of a
Principal Component Regression.

## Usage

``` r
# S3 method for class 'mvr'
vcov(object, ncomp, ...)
```

## Arguments

- object:

  a fitted PCR object of class `mvr`.

- ncomp:

  number of principal components to estimate `vcov` for.

- ...:

  additional arguments (not used).

## Value

A matrix of estimated covariances between regression coefficients.

## Examples

``` r
data(yarn)
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn)
vc <- vcov(yarn.pcr, 3)

# Standard error of coefficients
se <- sqrt(diag(vc))
beta <- coef(yarn.pcr, ncomp = 3)

# Plot regression coefficients with two standard errors shading.
plot(beta, type = 'l',
     panel.first = polygon(x = c(1:268, 268:1),
                           y = c(beta+2*se, rev(beta-2*se)),
                           col = 'lightblue',
                           border = NA))
```
