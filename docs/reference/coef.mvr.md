# Extract Information From a Fitted PLSR or PCR Model

Functions to extract information from `mvr` objects: Regression
coefficients, fitted values, residuals, the model frame, the model
matrix, names of the variables and components, and the \\X\\ variance
explained by the components.

## Usage

``` r
# S3 method for class 'mvr'
coef(object, ncomp = object$ncomp, comps, intercept = FALSE, ...)

# S3 method for class 'mvr'
fitted(object, ...)

# S3 method for class 'mvr'
residuals(object, ...)

# S3 method for class 'mvr'
model.frame(formula, ...)

# S3 method for class 'mvr'
model.matrix(object, ...)

respnames(object)

prednames(object, intercept = FALSE)

compnames(object, comps, explvar = FALSE, ...)

explvar(object)
```

## Arguments

- object, formula:

  an `mvr` object. The fitted model.

- ncomp, comps:

  vector of positive integers. The components to include in the
  coefficients or to extract the names of. See below.

- intercept:

  logical. Whether coefficients for the intercept should be included.
  Ignored if `comps` is specified. Defaults to `FALSE`.

- ...:

  other arguments sent to underlying functions. Currently only used for
  `model.frame.mvr` and `model.matrix.mvr`.

- explvar:

  logical. Whether the explained \\X\\ variance should be appended to
  the component names.

## Value

`coef.mvr` returns an array of regression coefficients.

`fitted.mvr` returns an array with fitted values.

`residuals.mvr` returns an array with residuals.

`model.frame.mvr` returns a data frame.

`model.matrix.mvr` returns the \\X\\ matrix.

`prednames`, `respnames` and `compnames` return a character vector with
the corresponding names.

`explvar` returns a numeric vector with the explained variances, or
`NULL` if not available.

## Details

These functions are mostly used inside other functions. (Functions
`coef.mvr`, `fitted.mvr` and `residuals.mvr` are usually called through
their generic functions [`coef`](https://rdrr.io/r/stats/coef.html),
[`fitted`](https://rdrr.io/r/stats/fitted.values.html) and
[`residuals`](https://rdrr.io/r/stats/residuals.html), respectively.)

`coef.mvr` is used to extract the regression coefficients of a model,
i.e. the \\B\\ in \\y = XB\\ (for the \\Q\\ in \\y = TQ\\ where \\T\\ is
the scores, see
[`Yloadings`](https://khliland.github.io/pls/reference/scores.md)). An
array of dimension `c(nxvar, nyvar, length(ncomp))` or
`c(nxvar, nyvar, length(comps))` is returned.

If `comps` is missing (or is `NULL`), `coef()[,,ncomp[i]]` are the
coefficients for models with `ncomp[i]` components, for \\i = 1, \ldots,
length(ncomp)\\. Also, if `intercept = TRUE`, the first dimension is
\\nxvar + 1\\, with the intercept coefficients as the first row.

If `comps` is given, however, `coef()[,,comps[i]]` are the coefficients
for a model with only the component `comps[i]`, i.e. the contribution of
the component `comps[i]` on the regression coefficients.

`fitted.mvr` and `residuals.mvr` return the fitted values and residuals,
respectively. If the model was fitted with `na.action = na.exclude` (or
after setting the default `na.action` to `"na.exclude"` with
[`options`](https://rdrr.io/r/base/options.html)), the fitted values (or
residuals) corresponding to excluded observations are returned as `NA`;
otherwise, they are omitted.

`model.frame.mvr` returns the model frame; i.e. a data frame with all
variables neccessary to generate the model matrix. See
[`model.frame`](https://rdrr.io/r/stats/model.frame.html) for details.

`model.matrix.mvr` returns the (possibly coded) matrix used as \\X\\ in
the fitting. See
[`model.matrix`](https://rdrr.io/r/stats/model.matrix.html) for details.

`prednames`, `respnames` and `compnames` extract the names of the \\X\\
variables, responses and components, respectively. With
`intercept = TRUE` in `prednames`, the name of the intercept variable
(i.e. `"(Intercept)"`) is returned as well. `compnames` can also extract
component names from score and loading matrices. If `explvar = TRUE` in
`compnames`, the explained variance for each component (if available) is
appended to the component names. For optimal formatting of the explained
variances when not all components are to be used, one should specify the
desired components with the argument `comps`.

`explvar` extracts the amount of \\X\\ variance (in per cent) explained
by each component in the model. It can also handle score and loading
matrices returned by
[`scores`](https://khliland.github.io/pls/reference/scores.md) and
[`loadings`](https://khliland.github.io/pls/reference/scores.md).

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md),
[`coef`](https://rdrr.io/r/stats/coef.html),
[`fitted`](https://rdrr.io/r/stats/fitted.values.html),
[`residuals`](https://rdrr.io/r/stats/residuals.html),
[`model.frame`](https://rdrr.io/r/stats/model.frame.html),
[`model.matrix`](https://rdrr.io/r/stats/model.matrix.html),
[`na.omit`](https://rdrr.io/r/stats/na.fail.html)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(yarn)
mod <- pcr(density ~ NIR, data = yarn[yarn$train,], ncomp = 5)
B <- coef(mod, ncomp = 3, intercept = TRUE)
## A manual predict method:
stopifnot(drop(B[1,,] + yarn$NIR[!yarn$train,] %*% B[-1,,]) ==
          drop(predict(mod, ncomp = 3, newdata = yarn[!yarn$train,])))

## Note the difference in formatting:
mod2 <- pcr(density ~ NIR, data = yarn[yarn$train,])
compnames(mod2, explvar = TRUE)[1:3]
#> [1] "Comp 1 (5.2e+01 %)" "Comp 2 (4.7e+01 %)" "Comp 3 (7.3e-01 %)"
compnames(mod2, comps = 1:3, explvar = TRUE)
#> [1] "Comp 1 (52.05 %)" "Comp 2 (46.73 %)" "Comp 3 (0.73 %)" 
```
