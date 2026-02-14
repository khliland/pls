# Jackknife approximate t tests of regression coefficients

Performes approximate t tests of regression coefficients based on
jackknife variance estimates.

## Usage

``` r
jack.test(object, ncomp = object$ncomp, use.mean = TRUE)

# S3 method for class 'jacktest'
print(x, P.values = TRUE, ...)
```

## Arguments

- object:

  an `mvr` object. A cross-validated model fitted with
  `jackknife = TRUE`.

- ncomp:

  the number of components to use for estimating the variances

- use.mean:

  logical. If `TRUE` (default), the mean coefficients are used when
  estimating the (co)variances; otherwise the coefficients from a model
  fitted to the entire data set. See
  [`var.jack`](https://khliland.github.io/pls/reference/var.jack.md) for
  details.

- x:

  an `jacktest` object, the result of `jack.test`.

- P.values:

  logical. Whether to print \\p\\ values (default).

- ...:

  Further arguments sent to the underlying print function
  [`printCoefmat`](https://rdrr.io/r/stats/printCoefmat.html).

## Value

`jack.test` returns an object of class `"jacktest"`, with components

- coefficients :

  The estimated regression coefficients

- sd:

  The square root of the jackknife variance estimates

- tvalues:

  The \\t\\ statistics

- df:

  The \`degrees of freedom' used for calculating \\p\\ values

- pvalues:

  The calculated \\p\\ values

`print.jacktest` returns the `"jacktest"` object (invisibly).

## Details

`jack.test` uses the variance estimates from `var.jack` to perform \\t\\
tests of the regression coefficients. The resulting object has a print
method, `print.jacktest`, which uses
[`printCoefmat`](https://rdrr.io/r/stats/printCoefmat.html) for the
actual printing.

## Warning

The jackknife variance estimates are known to be biased (see
[`var.jack`](https://khliland.github.io/pls/reference/var.jack.md)).
Also, the distribution of the regression coefficient estimates and the
jackknife variance estimates are unknown (at least in PLSR/PCR).
Consequently, the distribution (and in particular, the degrees of
freedom) of the resulting \\t\\ statistics is unknown. The present code
simply assumes a \\t\\ distribution with \\m - 1\\ degrees of freedom,
where \\m\\ is the number of cross-validation segments.

Therefore, the resulting \\p\\ values should not be used uncritically,
and should perhaps be regarded as mere indicator of (non-)significance.

Finally, also keep in mind that as the number of predictor variables
increase, the problem of multiple tests increases correspondingly.

## References

Martens H. and Martens M. (2000) Modified Jack-knife Estimation of
Parameter Uncertainty in Bilinear Modelling by Partial Least Squares
Regression (PLSR). *Food Quality and Preference*, **11**, 5–16.

## See also

[`var.jack`](https://khliland.github.io/pls/reference/var.jack.md),
[`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md)

## Author

Bjørn-Helge Mevik

## Examples

``` r
data(oliveoil)
mod <- pcr(sensory ~ chemical, data = oliveoil, validation = "LOO", jackknife = TRUE)
jack.test(mod, ncomp = 2)
#> Response yellow (2 comps):
#>           Estimate Std. Error Df t value Pr(>|t|)
#> Acidity  -53.88406   32.00643 15 -1.6835   0.1130
#> Peroxide  -1.64614    1.85612 15 -0.8869   0.3891
#> K232      -9.49515   24.15870 15 -0.3930   0.6998
#> K270      -3.66858    3.07335 15 -1.1937   0.2511
#> DK        -0.35815    0.34117 15 -1.0498   0.3105
#> 
#> Response green (2 comps):
#>          Estimate Std. Error Df t value Pr(>|t|)
#> Acidity  68.69511   39.86563 15  1.7232   0.1054
#> Peroxide  1.41003    2.42316 15  0.5819   0.5693
#> K232     12.06057   31.00606 15  0.3890   0.7028
#> K270      4.67437    3.85337 15  1.2131   0.2439
#> DK        0.45638    0.43416 15  1.0512   0.3098
#> 
#> Response brown (2 comps):
#>           Estimate Std. Error Df t value Pr(>|t|)  
#> Acidity  -5.974767   4.183469 15 -1.4282  0.17373  
#> Peroxide  1.271210   0.568083 15  2.2377  0.04084 *
#> K232     -0.958874   4.680049 15 -0.2049  0.84042  
#> K270     -0.401311   0.188443 15 -2.1296  0.05017 .
#> DK       -0.039263   0.030609 15 -1.2827  0.21905  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Response glossy (2 comps):
#>           Estimate Std. Error Df t value Pr(>|t|)  
#> Acidity  -7.693126   6.520910 15 -1.1798  0.25647  
#> Peroxide -1.126323   0.522543 15 -2.1555  0.04778 *
#> K232     -1.413252   4.776935 15 -0.2958  0.77140  
#> K270     -0.527123   0.467543 15 -1.1274  0.27727  
#> DK       -0.051409   0.076280 15 -0.6739  0.51060  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Response transp (2 comps):
#>            Estimate Std. Error Df t value Pr(>|t|)  
#> Acidity  -13.630902   8.573695 15 -1.5899  0.13272  
#> Peroxide  -1.286214   0.722367 15 -1.7806  0.09524 .
#> K232      -2.458184   8.325620 15 -0.2953  0.77185  
#> K270      -0.931303   0.695580 15 -1.3389  0.20055  
#> DK        -0.090869   0.109167 15 -0.8324  0.41825  
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Response syrup (2 comps):
#>          Estimate Std. Error Df t value  Pr(>|t|)    
#> Acidity  1.848304   2.947628 15  0.6270 0.5400554    
#> Peroxide 0.666474   0.130938 15  5.0900 0.0001331 ***
#> K232     0.365127   0.887538 15  0.4114 0.6866016    
#> K270     0.128133   0.187759 15  0.6824 0.5053697    
#> DK       0.012474   0.022187 15  0.5622 0.5822789    
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
