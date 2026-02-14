# Jackknife Variance Estimates of Regression Coefficients

Calculates jackknife variance or covariance estimates of regression
coefficients.

The original (Tukey) jackknife variance estimator is defined as
\\(g-1)/g \sum\_{i=1}^g(\tilde\beta\_{-i} - \bar\beta)^2\\, where \\g\\
is the number of segments, \\\tilde\beta\_{-i}\\ is the estimated
coefficient when segment \\i\\ is left out (called the jackknife
replicates), and \\\bar\beta\\ is the mean of the \\\tilde\beta\_{-i}\\.
The most common case is delete-one jackknife, with \\g = n\\, the number
of observations.

This is the definition `var.jack` uses by default.

However, Martens and Martens (2000) defined the estimator as \\(g-1)/g
\sum\_{i=1}^g(\tilde\beta\_{-i} - \hat\beta)^2\\, where \\\hat\beta\\ is
the coefficient estimate using the entire data set. I.e., they use the
original fitted coefficients instead of the mean of the jackknife
replicates. Most (all?) other jackknife implementations for PLSR use
this estimator. `var.jack` can be made to use this definition with
`use.mean = FALSE`. In practice, the difference should be small if the
number of observations is sufficiently large. Note, however, that all
theoretical results about the jackknife refer to the \`proper'
definition. (Also note that this option might disappear in a future
version.)

## Usage

``` r
var.jack(object, ncomp = object$ncomp, covariance = FALSE, use.mean = TRUE)
```

## Arguments

- object:

  an `mvr` object. A cross-validated model fitted with
  `jackknife = TRUE`.

- ncomp:

  the number of components to use for estimating the (co)variances

- covariance:

  logical. If `TRUE`, covariances are calculated; otherwise only
  variances. The default is `FALSE`.

- use.mean:

  logical. If `TRUE` (default), the mean coefficients are used when
  estimating the (co)variances; otherwise the coefficients from a model
  fitted to the entire data set. See Details.

## Value

If `covariance` is `FALSE`, an \\p\times q \times c\\ array of variance
estimates, where \\p\\ is the number of predictors, \\q\\ is the number
of responses, and \\c\\ is the number of components.

If `covariance` id `TRUE`, an \\pq\times pq \times c\\ array of
variance-covariance estimates.

## Warning

Note that the Tukey jackknife variance estimator is not unbiased for the
variance of regression coefficients (Hinkley 1977). The bias depends on
the \\X\\ matrix. For ordinary least squares regression (OLSR), the bias
can be calculated, and depends on the number of observations \\n\\ and
the number of parameters \\k\\ in the mode. For the common case of an
orthogonal design matrix with \\\pm 1\\ levels, the delete-one jackknife
estimate equals \\(n-1)/(n-k)\\ times the classical variance estimate
for the regression coefficients in OLSR. Similar expressions hold for
delete-d estimates. Modifications have been proposed to reduce or
eliminate the bias for the OLSR case, however, they depend on the number
of parameters used in the model. See e.g. Hinkley (1977) or Wu (1986).

Thus, the results of `var.jack` should be used with caution.

## References

Tukey J.W. (1958) Bias and Confidence in Not-quite Large Samples.
(Abstract of Preliminary Report). *Annals of Mathematical Statistics*,
**29**(2), 614.

Martens H. and Martens M. (2000) Modified Jack-knife Estimation of
Parameter Uncertainty in Bilinear Modelling by Partial Least Squares
Regression (PLSR). *Food Quality and Preference*, **11**, 5–16.

Hinkley D.V. (1977), Jackknifing in Unbalanced Situations.
*Technometrics*, **19**(3), 285–292.

Wu C.F.J. (1986) Jackknife, Bootstrap and Other Resampling Methods in
Regression Analysis. *Te Annals of Statistics*, **14**(4), 1261–1295.

## See also

[`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md),
[`jack.test`](https://khliland.github.io/pls/reference/jack.test.md)

## Author

Bjørn-Helge Mevik

## Examples

``` r
data(oliveoil)
mod <- pcr(sensory ~ chemical, data = oliveoil, validation = "LOO",
           jackknife = TRUE)
var.jack(mod, ncomp = 2)
#> , , 2 comps
#> 
#>                yellow        green        brown       glossy      transp
#> Acidity  1024.4116919 1589.2686000 1.750141e+01 42.522264128 73.50823993
#> Peroxide    3.4451819    5.8716926 3.227187e-01  0.273051034  0.52181445
#> K232      583.6428901  961.3757680 2.190286e+01 22.819112503 69.31594523
#> K270        9.4454718   14.8484347 3.551073e-02  0.218596282  0.48383108
#> DK          0.1163998    0.1884952 9.368976e-04  0.005818676  0.01191753
#>                 syrup
#> Acidity  8.6885127205
#> Peroxide 0.0171447602
#> K232     0.7877230726
#> K270     0.0352534553
#> DK       0.0004922534
#> 
```
