# NIPALS PCR with missing values

A NIPALS-based PCR that tolerates missing entries in both predictors and
responses by only using observed cells when updating scores and
loadings. It follows the same API as `svdpc.fit` so it can be used
whenever low-level PCR needs to handle incomplete data.

## Usage

``` r
nipalspc.fit(
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
  handled internally during the final regression step.

- ncomp:

  number of PCR components to extract.

- center:

  logical. If `TRUE` both `X` and `Y` are centered column-wise (ignoring
  missing entries).

- stripped:

  logical. When `TRUE` only the coefficients and mean vectors are
  returned for faster use in resampling.

- maxiter:

  maximum number of inner iterations per component.

- tol:

  convergence tolerance used when the direction vector stabilizes.

- ...:

  currently ignored.

## Value

A list mirroring the return value of `svdpc.fit` but computed via the
NA-robust NIPALS PCR updates.
