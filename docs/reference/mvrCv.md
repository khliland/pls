# Cross-validation

Performs the cross-validation calculations for `mvr`.

This function is not meant to be called directly, but through the
generic functions `pcr`, `plsr`, `cppls` or `mvr` with the argument
`validation` set to `"CV"` or `"LOO"`. All arguments to `mvrCv` can be
specified in the generic function call.

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

`X` and `Y` do not need to be centered.

Note that this function cannot be used in situations where \\X\\ needs
to be recalculated for each segment (except for scaling by the standard
deviation), for instance with `msc` or other preprocessing. For such
models, use the more general (but slower) function
[`crossval`](https://khliland.github.io/pls/reference/crossval.md).

Also note that if needed, the function will silently(!) reduce `ncomp`
to the maximal number of components that can be cross-validated, which
is \\n - l - 1\\, where \\n\\ is the number of observations and \\l\\ is
the length of the longest segment. The (possibly reduced) number of
components is returned as the component `ncomp`.

By default, the cross-validation will be performed serially. However, it
can be done in parallel using functionality in the
[`parallel`](https://rdrr.io/r/parallel/parallel-package.html) package
by setting the option `parallel` in
[`pls.options`](https://khliland.github.io/pls/reference/pls.options.md).
See
[`pls.options`](https://khliland.github.io/pls/reference/pls.options.md)
for the different ways to specify the parallelism.

## Usage

``` r
mvrCv(
  X,
  Y,
  ncomp,
  Y.add = NULL,
  weights = NULL,
  method = pls.options()$mvralg,
  scale = FALSE,
  segments = 10,
  segment.type = c("random", "consecutive", "interleaved"),
  length.seg,
  jackknife = FALSE,
  trace = FALSE,
  ...
)
```

## Arguments

- X:

  a matrix of observations. `NA`s and `Inf`s are not allowed.

- Y:

  a vector or matrix of responses. `NA`s and `Inf`s are not allowed.

- ncomp:

  the number of components to be used in the modelling.

- Y.add:

  a vector or matrix of additional responses containing relevant
  information about the observations. Only used for `cppls`.

- weights:

  a vector of individual weights for the observations. Only used for
  `cppls`. (Optional)

- method:

  the multivariate regression method to be used.

- scale:

  logical. If `TRUE`, the learning \\X\\ data for each segment is scaled
  by dividing each variable by its sample standard deviation. The
  prediction data is scaled by the same amount.

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

  logical; if `TRUE`, the segment number is printed for each segment.

- ...:

  additional arguments, sent to the underlying fit function.

## Value

A list with the following components:

- method:

  equals `"CV"` for cross-validation.

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

  the actual number of components used.

- gamma:

  if method `cppls` is used, gamma values for the powers of each CV
  segment are returned.

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
[`crossval`](https://khliland.github.io/pls/reference/crossval.md)
[`cvsegments`](https://khliland.github.io/pls/reference/cvsegments.md)
[`MSEP`](https://khliland.github.io/pls/reference/mvrVal.md)
[`var.jack`](https://khliland.github.io/pls/reference/var.jack.md)
[`jack.test`](https://khliland.github.io/pls/reference/jack.test.md)

## Author

Ron Wehrens and Bjørn-Helge Mevik

## Examples

``` r
data(yarn)
yarn.pcr <- pcr(density ~ NIR, 6, data = yarn, validation = "CV", segments = 10)
if (FALSE) plot(MSEP(yarn.pcr)) # \dontrun{}
```
