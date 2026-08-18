# Factor to Segments

Factor to Segments

## Usage

``` r
fac2seg(fac)
```

## Arguments

- fac:

  A factor where each level represents a segment

## Value

A list of vectors, each vector contains the indices of the elements of
the corresponding segment

## See also

CV segments by various patterns:
[`cvsegments`](https://khliland.github.io/pls/reference/cvsegments.md).

## Examples

``` r
fac <- factor(c("a", "b", "a", "b", "c", "c"))
fac2seg(fac)
#> [[1]]
#> [1] 1 3
#> 
#> [[2]]
#> [1] 2 4
#> 
#> [[3]]
#> [1] 5 6
#> 
```
