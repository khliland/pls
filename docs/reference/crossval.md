# Cross-validation of PLSR and PCR models

A “stand alone” cross-validation function for `mvr` objects.

## Usage

``` r
crossval(
  object,
  segments = 10,
  segment.type = c("random", "consecutive", "interleaved"),
  length.seg,
  jackknife = FALSE,
  trace = 15,
  ...
)
```

## Arguments

- object:

  an `mvr` object; the regression to cross-validate.

- segments:

  the number of segments to use, or a list with segments (see below).

- segment.type:

  the type of segments to use. Ignored if `segments` is a list.

- length.seg:

  Positive integer. The length of the segments to use. If specified, it
  overrides `segments` unless `segments` is a list.

- jackknife:

  logical. Whether jackknifing of regression coefficients should be
  performed.

- trace:

  if `TRUE`, tracing is turned on. If numeric, it denotes a time limit
  (in seconds). If the estimated total time of the cross-validation
  exceeds this limit, tracing is turned on.

- ...:

  additional arguments, sent to the underlying fit function.

## Value

The supplied `object` is returned, with an additional component
`validation`, which is a list with components

- method:

  euqals `"CV"` for cross-validation.

- pred:

  an array with the cross-validated predictions.

- coefficients:

  (only if `jackknife` is `TRUE`) an array with the jackknifed
  regression coefficients. The dimensions correspond to the predictors,
  responses, number of components, and segments, respectively.

- PRESS0:

  a vector of PRESS values (one for each response variable) for a model
  with zero components, i.e., only the intercept.

- PRESS:

  a matrix of PRESS values for models with 1, ..., `ncomp` components.
  Each row corresponds to one response variable.

- adj:

  a matrix of adjustment values for calculating bias corrected MSEP.
  `MSEP` uses this.

- segments:

  the list of segments used in the cross-validation.

- ncomp:

  the number of components.

- gammas:

  if method `cppls` is used, gamma values for the powers of each CV
  segment are returned.

## Details

This function performs cross-validation on a model fit by `mvr`. It can
handle models such as `plsr(y ~ msc(X), ...{})` or other models where
the predictor variables need to be recalculated for each segment. When
recalculation is not needed, the result of `crossval(mvr(...{}))` is
identical to `mvr(...{}, validation = "CV")`, but slower.

Note that to use `crossval`, the data *must* be specified with a `data`
argument when fitting `object`.

If `segments` is a list, the arguments `segment.type` and `length.seg`
are ignored. The elements of the list should be integer vectors
specifying the indices of the segments. See
[`cvsegments`](https://khliland.github.io/pls/reference/cvsegments.md)
for details.

Otherwise, segments of type `segment.type` are generated. How many
segments to generate is selected by specifying the number of segments in
`segments`, or giving the segment length in `length.seg`. If both are
specified, `segments` is ignored.

If `jackknife` is `TRUE`, jackknifed regression coefficients are
returned, which can be used for for variance estimation
([`var.jack`](https://khliland.github.io/pls/reference/var.jack.md)) or
hypothesis testing
([`jack.test`](https://khliland.github.io/pls/reference/jack.test.md)).

When tracing is turned on, the segment number is printed for each
segment.

By default, the cross-validation will be performed serially. However, it
can be done in parallel using functionality in the
[`parallel`](https://rdrr.io/r/parallel/parallel-package.html) package
by setting the option `parallel` in
[`pls.options`](https://khliland.github.io/pls/reference/pls.options.md).
See
[`pls.options`](https://khliland.github.io/pls/reference/pls.options.md)
for the different ways to specify the parallelism. See also Examples
below.

## Note

The `PRESS0` is always cross-validated using leave-one-out
cross-validation. This usually makes little difference in practice, but
should be fixed for correctness.

The current implementation of the jackknife stores all
jackknife-replicates of the regression coefficients, which can be very
costly for large matrices. This might change in a future version.

## References

Mevik, B.-H., Cederkvist, H. R. (2004) Mean Squared Error of Prediction
(MSEP) Estimates for Principal Component Regression (PCR) and Partial
Least Squares Regression (PLSR). *Journal of Chemometrics*, **18**(9),
422–429.

## See also

[`mvr`](https://khliland.github.io/pls/reference/mvr.md)
[`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md)
[`cvsegments`](https://khliland.github.io/pls/reference/cvsegments.md)
[`MSEP`](https://khliland.github.io/pls/reference/mvrVal.md)
[`var.jack`](https://khliland.github.io/pls/reference/var.jack.md)
[`jack.test`](https://khliland.github.io/pls/reference/jack.test.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(yarn)
yarn.pcr <- pcr(density ~ msc(NIR), 6, data = yarn)
yarn.cv <- crossval(yarn.pcr, segments = 10)
if (FALSE) plot(MSEP(yarn.cv)) # \dontrun{}

if (FALSE) { # \dontrun{
## Parallelised cross-validation, using transient cluster:
pls.options(parallel = 4) # use mclapply (not available on Windows)
pls.options(parallel = quote(parallel::makeCluster(4, type = "PSOCK"))) # parLapply
## A new cluster is created and stopped for each cross-validation:
yarn.cv <- crossval(yarn.pcr)
yarn.loocv <- crossval(yarn.pcr, length.seg = 1)

## Parallelised cross-validation, using persistent cluster:
library(parallel)
## This creates the cluster:
pls.options(parallel = makeCluster(4, type = "FORK")) # not available on Windows
pls.options(parallel = makeCluster(4, type = "PSOCK"))
## The cluster can be used several times:
yarn.cv <- crossval(yarn.pcr)
yarn.loocv <- crossval(yarn.pcr, length.seg = 1)
## The cluster should be stopped manually afterwards:
stopCluster(pls.options()$parallel)

## Parallelised cross-validation, using persistent MPI cluster:
## This requires the packages snow and Rmpi to be installed
library(parallel)
## This creates the cluster:
pls.options(parallel = makeCluster(4, type = "MPI"))
## The cluster can be used several times:
yarn.cv <- crossval(yarn.pcr)
yarn.loocv <- crossval(yarn.pcr, length.seg = 1)
## The cluster should be stopped manually afterwards:
stopCluster(pls.options()$parallel)
## It is good practice to call mpi.exit() or mpi.quit() afterwards:
mpi.exit()
} # }
```
