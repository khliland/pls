# Partial Least Squares and Principal Component Regression

Functions to perform partial least squares regression (PLSR), canonical
powered partial least squares (CPPLS) or principal component regression
(PCR), with a formula interface. Cross-validation can be used.
Prediction, model extraction, plot, print and summary methods exist.

## Usage

``` r
mvr(
  formula,
  ncomp,
  Y.add,
  data,
  subset,
  na.action,
  method = pls.options()$mvralg,
  scale = FALSE,
  center = TRUE,
  validation = c("none", "CV", "LOO"),
  model = TRUE,
  x = FALSE,
  y = FALSE,
  ...
)

plsr(..., method = pls.options()$plsralg)

pcr(..., method = pls.options()$pcralg)

cppls(..., Y.add, weights, method = pls.options()$cpplsalg)

nipals(..., weights, method = "nipalspls")

nipalspcr(..., method = "nipalspc")
```

## Arguments

- formula:

  a model formula. Most of the [`lm`](https://rdrr.io/r/stats/lm.html)
  formula constructs are supported. See below.

- ncomp:

  the number of components to include in the model (see below).

- Y.add:

  a vector or matrix of additional responses containing relevant
  information about the observations. Only used for `cppls`.

- data:

  an optional data frame with the data to fit the model from.

- subset:

  an optional vector specifying a subset of observations to be used in
  the fitting process.

- na.action:

  a function which indicates what should happen when the data contain
  missing values. The default is set by the `na.action` setting of
  [`options`](https://rdrr.io/r/base/options.html), and is
  [`na.fail`](https://rdrr.io/r/stats/na.fail.html) if that is unset.
  The ‘factory-fresh’ default is
  [`na.omit`](https://rdrr.io/r/stats/na.fail.html). Another possible
  value is `NULL`, no action. Value
  [`na.exclude`](https://rdrr.io/r/stats/na.fail.html) can be useful.
  See [`na.omit`](https://rdrr.io/r/stats/na.fail.html) for other
  alternatives.

- method:

  the multivariate regression method to be used. If `"model.frame"`, the
  model frame is returned.

- scale:

  numeric vector, or logical. If numeric vector, \\X\\ is scaled by
  dividing each variable with the corresponding element of `scale`. If
  `scale` is `TRUE`, \\X\\ is scaled by dividing each variable by its
  sample standard deviation. If cross-validation is selected, scaling by
  the standard deviation is done for every segment.

- center:

  logical, determines if the \\X\\ and \\Y\\ matrices are mean centered
  or not. Default is to perform mean centering.

- validation:

  character. What kind of (internal) validation to use. See below.

- model:

  a logical. If `TRUE`, the model frame is returned.

- x:

  a logical. If `TRUE`, the model matrix is returned.

- y:

  a logical. If `TRUE`, the response is returned.

- ...:

  additional optional arguments, passed to the underlying fit functions,
  and [`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md).

  Currently, the fit functions
  [`oscorespls.fit`](https://khliland.github.io/pls/reference/oscorespls.fit.md)
  and
  [`widekernelpls.fit`](https://khliland.github.io/pls/reference/widekernelpls.fit.md)
  implement these extra arguments:

  tol:

  :   numeric. Tolerance used for determining convergence.

  maxit:

  :   positive integer. The maximal number of iterations used.

  and
  [`cppls.fit`](https://khliland.github.io/pls/reference/cppls.fit.md)
  implements:

  lower:

  :   a vector of lower limits for power optimisation.

  upper:

  :   a vector of upper limits for power optimisation.

  trunc.pow:

  :   logical. Whether to use an experimental alternative power
      algorithm.

  [`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md)
  implements several arguments; the following are probably the most
  useful of them:

  segments:

  :   the number of segments to use, or a list with segments.

  segment.type:

  :   the type of segments to use.

  length.seg:

  :   Positive integer. The length of the segments to use.

  jackknife:

  :   logical. Whether to perform jackknifing of regression
      coefficients.

  See the functions' documentation for details.

- weights:

  a vector of individual weights for the observations. Only used for
  `cppls`. (Optional)

## Value

If `method = "model.frame"`, the model frame is returned. Otherwise, an
object of class `mvr` is returned. The object contains all components
returned by the underlying fit function. In addition, it contains the
following components:

- validation:

  if validation was requested, the results of the cross-validation. See
  [`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md) for
  details.

- fit.time:

  the elapsed time for the fit. This is used by
  [`crossval`](https://khliland.github.io/pls/reference/crossval.md) to
  decide whether to turn on tracing.

- na.action:

  if observations with missing values were removed, `na.action` contains
  a vector with their indices. The class of this vector is used by
  functions like `fitted` to decide how to treat the observations.

- ncomp:

  the number of components of the model.

- method:

  the method used to fit the model. See the argument `method` for
  possible values.

- center:

  use of centering in the model

- scale:

  if scaling was requested (with `scale`), the scaling used.

- call:

  the function call.

- terms:

  the model terms.

- model:

  if `model = TRUE`, the model frame.

- x:

  if `x = TRUE`, the model matrix.

- y:

  if `y = TRUE`, the model response.

## Details

The functions fit PLSR, CPPLS or PCR models with 1, \\\ldots\\, `ncomp`
number of components. Multi-response models are fully supported.

The type of model to fit is specified with the `method` argument. Four
PLSR algorithms are available: the kernel algorithm (`"kernelpls"`), the
wide kernel algorithm (`"widekernelpls"`), SIMPLS (`"simpls"`) and the
classical orthogonal scores algorithm (`"oscorespls"`). One CPPLS
algorithm is available (`"cppls"`) providing several extensions to PLS.
One PCR algorithm is available: using the singular value decomposition
(`"svdpc"`). If `method` is `"model.frame"`, the model frame is
returned. The functions `pcr`, `plsr` and `cppls` are wrappers for
`mvr`, with different values for `method`.

The `formula` argument should be a symbolic formula of the form
`response ~ terms`, where `response` is the name of the response vector
or matrix (for multi-response models) and `terms` is the name of one or
more predictor matrices, usually separated by `+`, e.g., `water ~ FTIR`
or `y ~ X + Z`. See [`lm`](https://rdrr.io/r/stats/lm.html) for a
detailed description. The named variables should exist in the supplied
`data` data frame or in the global environment. Note: Do not use
`mvr(mydata$y ~ mydata$X, ...{})`, instead use
`mvr(y ~ X, data = mydata, ...{})`. Otherwise,
[`predict.mvr`](https://khliland.github.io/pls/reference/predict.mvr.md)
will not work properly. The chapter `Statistical models in R` of the
manual `An Introduction to R` distributed with is a good reference on
formulas in .

The number of components to fit is specified with the argument `ncomp`.
It this is not supplied, the maximal number of components is used
(taking account of any cross-validation).

All implemented algorithms mean-center both predictor and response
matrices. This can be turned off by specifying `center = FALSE`. See
Seasholtz and Kowalski for a discussion about centering in PLS
regression.

If `validation = "CV"`, cross-validation is performed. The number and
type of cross-validation segments are specified with the arguments
`segments` and `segment.type`. See
[`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md) for
details. If `validation = "LOO"`, leave-one-out cross-validation is
performed. It is an error to specify the segments when
`validation = "LOO"` is specified.

By default, the cross-validation will be performed serially. However, it
can be done in parallel using functionality in the
[`parallel`](https://rdrr.io/r/parallel/parallel-package.html) package
by setting the option `parallel` in
[`pls.options`](https://khliland.github.io/pls/reference/pls.options.md).
See
[`pls.options`](https://khliland.github.io/pls/reference/pls.options.md)
for the differnt ways to specify the parallelism. See also Examples
below.

Note that the cross-validation is optimised for speed, and some
generality has been sacrificed. Especially, the model matrix is
calculated only once for the complete cross-validation, so models like
`y ~ msc(X)` will not be properly cross-validated. However, scaling
requested by `scale = TRUE` is properly cross-validated. For proper
cross-validation of models where the model matrix must be
updated/regenerated for each segment, use the separate function
[`crossval`](https://khliland.github.io/pls/reference/crossval.md).

## References

Martens, H., Næs, T. (1989) *Multivariate calibration.* Chichester:
Wiley.

Seasholtz, M. B. and Kowalski, B. R. (1992) The effect of mean centering
on prediction in multivariate calibration. *Journal of Chemometrics*,
**6**(2), 103–111.

## See also

[`kernelpls.fit`](https://khliland.github.io/pls/reference/kernelpls.fit.md),
[`widekernelpls.fit`](https://khliland.github.io/pls/reference/widekernelpls.fit.md),
[`simpls.fit`](https://khliland.github.io/pls/reference/simpls.fit.md),
[`oscorespls.fit`](https://khliland.github.io/pls/reference/oscorespls.fit.md),
[`nipals.fit`](https://khliland.github.io/pls/reference/nipals.fit.md),
[`cppls.fit`](https://khliland.github.io/pls/reference/cppls.fit.md),
[`svdpc.fit`](https://khliland.github.io/pls/reference/svdpc.fit.md),
[`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md),
[`crossval`](https://khliland.github.io/pls/reference/crossval.md),
[`loadings`](https://rdrr.io/r/stats/loadings.html),
[`scores`](https://khliland.github.io/pls/reference/scores.md),
[`loading.weights`](https://khliland.github.io/pls/reference/scores.md),
[`coef.mvr`](https://khliland.github.io/pls/reference/coef.mvr.md),
[`predict.mvr`](https://khliland.github.io/pls/reference/predict.mvr.md),
[`R2`](https://khliland.github.io/pls/reference/mvrVal.md),
[`MSEP`](https://khliland.github.io/pls/reference/mvrVal.md),
[`RMSEP`](https://khliland.github.io/pls/reference/mvrVal.md),
[`plot.mvr`](https://khliland.github.io/pls/reference/plot.mvr.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(yarn)
## Default methods:
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.cppls <- cppls(density ~ NIR, 6, data = yarn, validation = "CV")

## Alternative methods:
yarn.oscorespls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                      method = "oscorespls")
yarn.simpls <- mvr(density ~ NIR, 6, data = yarn, validation = "CV",
                  method = "simpls")
# See ?simpls.fit for example of numerical instability in SIMPLS

if (FALSE) { # \dontrun{
## Parallelised cross-validation, using transient cluster:
pls.options(parallel = 4) # use mclapply
pls.options(parallel = quote(makeCluster(4, type = "PSOCK"))) # use parLapply
## A new cluster is created and stopped for each cross-validation:
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")

## Parallelised cross-validation, using persistent cluster:
library(parallel)
## This creates the cluster:
pls.options(parallel = makeCluster(4, type = "PSOCK"))
## The cluster can be used several times:
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
## The cluster should be stopped manually afterwards:
stopCluster(pls.options()$parallel)

## Parallelised cross-validation, using persistent MPI cluster:
## This requires the packages snow and Rmpi to be installed
library(parallel)
## This creates the cluster:
pls.options(parallel = makeCluster(4, type = "MPI"))
## The cluster can be used several times:
yarn.pls <- plsr(density ~ NIR, 6, data = yarn, validation = "CV")
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV")
## The cluster should be stopped manually afterwards:
stopCluster(pls.options()$parallel)
## It is good practice to call mpi.exit() or mpi.quit() afterwards:
mpi.exit()
} # }

## Multi-response models:
data(oliveoil)
sens.pcr <- pcr(sensory ~ chemical, ncomp = 4, scale = TRUE, data = oliveoil)
sens.pls <- plsr(sensory ~ chemical, ncomp = 4, scale = TRUE, data = oliveoil)

## Classification
# A classification example utilizing additional response information
# (Y.add) is found in the cppls.fit manual ('See also' above).
```
