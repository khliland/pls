# NIPALS PLS with missing values

A NIPALS implementation that tolerates `NA`s in both `X` and `Y` by
ignoring them when updating scores and loadings. This is useful when the
design matrix is incomplete but the number of components is relatively
low.

## Usage

``` r
nipals.fit(
  X,
  Y,
  ncomp,
  center = TRUE,
  stripped = FALSE,
  maxiter = 500,
  tol = 1e-06,
  ...
)
```

## Arguments

- X:

  numeric matrix (or coercible) of predictors. Missing values are
  allowed and handled internally.

- Y:

  numeric matrix (or coercible) of responses. Missing values are also
  handled internally.

- ncomp:

  number of PLS components to extract.

- center:

  logical whether to center `X` and `Y` before fitting. Means ignore
  missing entries.

- stripped:

  logical. If `TRUE` only the coefficients and the mean vectors are
  returned.

- maxiter:

  maximum number of inner iterations to force convergence on each
  component.

- tol:

  tolerance used to stop the inner loop when the direction vector
  changes very little.

- ...:

  currently ignored.

## Value

A list with the same components as `nipals.fit`, but the computations
never fail in the presence of missing entries.
