# Set or return options for the pls package

A function to set options for the pls package, or to return the current
options.

## Usage

``` r
pls.options(...)
```

## Arguments

- ...:

  a single list, a single character vector, or any number of named
  arguments (`name = value`).

## Value

A list with the (possibly changed) options. If any named argument (or
list element) was provided, the list is returned invisibly.

## Details

If called with no arguments, or with an empty list as the single
argument, `pls.options` returns the current options.

If called with a character vector as the single argument, a list with
the arguments named in the vector are returned.

If called with a non-empty list as the single argument, the list
elements should be named, and are treated as named arguments to the
function.

Otherwise, `pls.options` should be called with one or more named
arguments `name = value`. For each argument, the option named `name`
will be given the value `value`.

The recognised options are:

- mvralg:

  The fit method to use in
  [`mvr`](https://khliland.github.io/pls/reference/mvr.md) and
  [`mvrCv`](https://khliland.github.io/pls/reference/mvrCv.md). The
  value should be one of the allowed methods. Defaults to `"kernelpls"`.
  Can be overridden with the argument `method` in `mvr` and `mvrCv`.

- pcralg:

  The fit method to use in
  [`pcr`](https://khliland.github.io/pls/reference/mvr.md). The value
  should be one of the allowed methods. Defaults to `"svdpc"`. Can be
  overridden with the argument `method` in `pcr`.

- plsralg:

  The fit method to use in
  [`plsr`](https://khliland.github.io/pls/reference/mvr.md). The value
  should be one of the allowed methods. Defaults to `"kernelpls"`. Can
  be overridden with the argument `method` in `plsr`.

- cpplsalg:

  The fit method to use in
  [`cppls`](https://khliland.github.io/pls/reference/mvr.md). The value
  should be one of the allowed methods. Defaults to `"cppls"`. Can be
  overridden with the argument `method` in `cppls`.

- parallel:

  Specification of how the cross-validation (CV) in
  [`mvr`](https://khliland.github.io/pls/reference/mvr.md) should be
  performed. If the specification is `NULL` (default) or `1`, the CV is
  done serially, otherwise it is done in parallel using functionality
  from the
  [`parallel`](https://rdrr.io/r/parallel/parallel-package.html)
  package.

  If it is an integer greater than 1, the CV is done in parallel with
  the specified number of processes, using
  [`mclapply`](https://rdrr.io/r/parallel/mclapply.html).

  If it is a cluster object created by
  [`makeCluster`](https://rdrr.io/r/parallel/makeCluster.html), the CV
  is done in parallel on that cluster, using
  [`parLapply`](https://rdrr.io/r/parallel/clusterApply.html). The user
  should stop the cluster herself when it is no longer needed, using
  [`stopCluster`](https://rdrr.io/r/parallel/makeCluster.html).

  Finally, if the specification is an unevaluated call to
  [`makeCluster`](https://rdrr.io/r/parallel/makeCluster.html), the call
  is evaluated, and the CV is done in parallel on the resulting cluster,
  using [`parLapply`](https://rdrr.io/r/parallel/clusterApply.html). In
  this case, the cluster will be stopped (with
  [`stopCluster`](https://rdrr.io/r/parallel/makeCluster.html)) after
  the CV. Thus, in the final case, the cluster is created and destroyed
  for each CV, just like when using
  [`mclapply`](https://rdrr.io/r/parallel/mclapply.html).

- w.tol:

  The tolerance used for removing values close to 0 in the vectors of
  loading weights in
  [`cppls`](https://khliland.github.io/pls/reference/mvr.md). Defaults
  to .Machine\$double.eps.

- X.tol:

  The tolerance used for removing predictor variables with L1 norms
  close to 0 in
  [`cppls`](https://khliland.github.io/pls/reference/mvr.md). Defaults
  to 10^-12.

## Note

The function is a slight modification of the function
[`sm.options`](https://rdrr.io/pkg/sm/man/sm.options.html) from the
package sm.

## Author

Bjørn-Helge Mevik and Ron Wehrens

## Examples

``` r
## Return current options:
pls.options()
#> $mvralg
#> [1] "kernelpls"
#> 
#> $plsralg
#> [1] "kernelpls"
#> 
#> $cpplsalg
#> [1] "cppls"
#> 
#> $pcralg
#> [1] "svdpc"
#> 
#> $parallel
#> NULL
#> 
#> $w.tol
#> [1] 2.220446e-16
#> 
#> $X.tol
#> [1] 1e-12
#> 
pls.options("plsralg")
#> $plsralg
#> [1] "kernelpls"
#> 
pls.options(c("plsralg", "pcralg"))
#> $plsralg
#> [1] "kernelpls"
#> 
#> $pcralg
#> [1] "svdpc"
#> 

## Set options:
pls.options(plsralg = "simpls", mvralg = "simpls")
pls.options(list(plsralg = "simpls", mvralg = "simpls")) # Equivalent
pls.options()
#> $mvralg
#> [1] "simpls"
#> 
#> $plsralg
#> [1] "simpls"
#> 
#> $cpplsalg
#> [1] "cppls"
#> 
#> $pcralg
#> [1] "svdpc"
#> 
#> $parallel
#> NULL
#> 
#> $w.tol
#> [1] 2.220446e-16
#> 
#> $X.tol
#> [1] 1e-12
#> 

## Restore `factory settings':
pls.options(list(mvralg = "kernelpls", plsralg = "kernelpls", cpplsalg = "cppls",
                 pcralg = "svdpc", parallel = NULL,
                 w.tol = .Machine$double.eps, X.tol = 10^-12))
pls.options()
#> $mvralg
#> [1] "kernelpls"
#> 
#> $plsralg
#> [1] "kernelpls"
#> 
#> $cpplsalg
#> [1] "cppls"
#> 
#> $pcralg
#> [1] "svdpc"
#> 
#> $parallel
#> NULL
#> 
#> $w.tol
#> [1] 2.220446e-16
#> 
#> $X.tol
#> [1] 1e-12
#> 
```
