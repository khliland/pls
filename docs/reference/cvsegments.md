# Generate segments for cross-validation

The function generates a list of segments for cross-validation. It can
generate random, consecutive and interleaved segments, and supports
keeping replicates in the same segment.

## Usage

``` r
cvsegments(
  N,
  k,
  length.seg = ceiling(N/k),
  nrep = 1,
  type = c("random", "consecutive", "interleaved"),
  stratify = NULL
)
```

## Arguments

- N:

  Integer. The number of rows in the data set.

- k:

  Integer. The number of segments to return.

- length.seg:

  Integer. The length of the segments. If given, it overrides `k`.

- nrep:

  Integer. The number of (consecutive) rows that are replicates of the
  same object. Replicates will always be kept in the same segment.

- type:

  One of `"random"`, `"consecutive"` and `"interleaved"`. The type of
  segments to generate. Default is `"random"`.

- stratify:

  Either a `list` of indices or an integer `vector` indicating which
  stratum each sample (or set of replicates) belongs to (see Details).

## Value

A list of vectors. Each vector contains the indices for one segment. The
attribute `"incomplete"` contains the number of incomplete segments, and
the attribute `"type"` contains the type of segments.

## Details

If `length.seg` is specified, it is used to calculate the number of
segments to generate. Otherwise `k` must be specified. If
\\k\*length.seg \ne N\\, the \\k\*length.seg - N\\ last segments will
contain only \\length.seg - 1\\ indices.

If `type` is `"random"`, the indices are allocated to segments in random
order. If it is `"consecutive"`, the first segment will contain the
first \\length.seg\\ indices, and so on. If `type` is `"interleaved"`,
the first segment will contain the indices \\1, length.seg+1,
2\*lenght.seg+1, \ldots, (k-1)\*length.seg+1\\, and so on.

If \\nrep \> \\, it is assumed that each `nrep` consecutive rows are
replicates (repeated measurements) of the same object, and care is taken
that replicates are never put in different segments.

Warning: If `k` does not divide `N`, a specified `length.seg` does not
divide `N`, or `nrep` does not divide `length.seg`, the number of
segments and/or the segment length will be adjusted as needed. Warnings
are printed for some of these cases, and one should always inspect the
resulting segments to make sure they are as expected.

Stratification of samples is enabled by the `stratify` argument. This is
useful if there are sub-groups in the data set that should have a
proportional representation in the cross-validation segments or if the
response is categorical (classifiation). If `stratify` is combined with
`nrep`, `stratify` corresponds to the sets of replicates (see example).

## See also

Converting a factor to segments:
[`fac2seg`](https://khliland.github.io/pls/reference/fac2seg.md).

## Author

Bjørn-Helge Mevik, Ron Wehrens and Kristian Hovde Liland

## Examples

``` r

## Segments for 10-fold randomised cross-validation:
cvsegments(100, 10)
#> $V1
#>  [1] 58 53 74 61 89 25 83 45 50 63
#> 
#> $V2
#>  [1] 91 86 59 64 97 35 98 94 40 46
#> 
#> $V3
#>  [1]  85 100  29  43  27  90  55  48  71  37
#> 
#> $V4
#>  [1] 66 87 75 99 77 22 28 12 36 70
#> 
#> $V5
#>  [1] 88 19 14 18 95 13 39 57 38 42
#> 
#> $V6
#>  [1]  5 92 80 76 51  2 68 62  6 26
#> 
#> $V7
#>  [1] 52 34 20 30 67 54 15 93 10 84
#> 
#> $V8
#>  [1]  4 17 24 69  1  7 65 11 96 73
#> 
#> $V9
#>  [1] 23 49 72 81 47 32 31 44 21 79
#> 
#> $V10
#>  [1] 33 41 78  8 60 82  3 16  9 56
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "random"

## Segments with four objects, taken consecutive:
(segs <- cvsegments(60, length.seg = 4, type = "cons"))
#> $V1
#> [1] 1 2 3 4
#> 
#> $V2
#> [1] 5 6 7 8
#> 
#> $V3
#> [1]  9 10 11 12
#> 
#> $V4
#> [1] 13 14 15 16
#> 
#> $V5
#> [1] 17 18 19 20
#> 
#> $V6
#> [1] 21 22 23 24
#> 
#> $V7
#> [1] 25 26 27 28
#> 
#> $V8
#> [1] 29 30 31 32
#> 
#> $V9
#> [1] 33 34 35 36
#> 
#> $V10
#> [1] 37 38 39 40
#> 
#> $V11
#> [1] 41 42 43 44
#> 
#> $V12
#> [1] 45 46 47 48
#> 
#> $V13
#> [1] 49 50 51 52
#> 
#> $V14
#> [1] 53 54 55 56
#> 
#> $V15
#> [1] 57 58 59 60
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "consecutive"
data(gasoline)
plsr(octane ~ NIR, data=gasoline, ncomp=5, validation="CV", segments=segs)
#> Partial least squares regression, fitted with the kernel algorithm.
#> Cross-validated using 15 consecutive segments.
#> Call:
#> pls::plsr(formula = octane ~ NIR, ncomp = 5, data = gasoline,     validation = "CV", segments = segs)

## Incomplete segments
segs <- cvsegments(50, length.seg = 3)
#> Warning: Required segment length does not divide the number of observations.
#>   A best effort segment size will be used.
attr(segs, "incomplete")
#> [1] 1

## Leave-one-out cross-validation:
cvsegments(100, 100)
#> $V1
#> [1] 52
#> 
#> $V2
#> [1] 48
#> 
#> $V3
#> [1] 56
#> 
#> $V4
#> [1] 82
#> 
#> $V5
#> [1] 17
#> 
#> $V6
#> [1] 83
#> 
#> $V7
#> [1] 24
#> 
#> $V8
#> [1] 22
#> 
#> $V9
#> [1] 89
#> 
#> $V10
#> [1] 90
#> 
#> $V11
#> [1] 73
#> 
#> $V12
#> [1] 5
#> 
#> $V13
#> [1] 86
#> 
#> $V14
#> [1] 60
#> 
#> $V15
#> [1] 93
#> 
#> $V16
#> [1] 31
#> 
#> $V17
#> [1] 7
#> 
#> $V18
#> [1] 72
#> 
#> $V19
#> [1] 63
#> 
#> $V20
#> [1] 10
#> 
#> $V21
#> [1] 75
#> 
#> $V22
#> [1] 94
#> 
#> $V23
#> [1] 68
#> 
#> $V24
#> [1] 1
#> 
#> $V25
#> [1] 78
#> 
#> $V26
#> [1] 15
#> 
#> $V27
#> [1] 35
#> 
#> $V28
#> [1] 46
#> 
#> $V29
#> [1] 98
#> 
#> $V30
#> [1] 2
#> 
#> $V31
#> [1] 55
#> 
#> $V32
#> [1] 91
#> 
#> $V33
#> [1] 11
#> 
#> $V34
#> [1] 97
#> 
#> $V35
#> [1] 84
#> 
#> $V36
#> [1] 42
#> 
#> $V37
#> [1] 64
#> 
#> $V38
#> [1] 51
#> 
#> $V39
#> [1] 54
#> 
#> $V40
#> [1] 71
#> 
#> $V41
#> [1] 65
#> 
#> $V42
#> [1] 28
#> 
#> $V43
#> [1] 26
#> 
#> $V44
#> [1] 38
#> 
#> $V45
#> [1] 30
#> 
#> $V46
#> [1] 59
#> 
#> $V47
#> [1] 74
#> 
#> $V48
#> [1] 9
#> 
#> $V49
#> [1] 96
#> 
#> $V50
#> [1] 77
#> 
#> $V51
#> [1] 57
#> 
#> $V52
#> [1] 25
#> 
#> $V53
#> [1] 29
#> 
#> $V54
#> [1] 13
#> 
#> $V55
#> [1] 53
#> 
#> $V56
#> [1] 50
#> 
#> $V57
#> [1] 87
#> 
#> $V58
#> [1] 45
#> 
#> $V59
#> [1] 69
#> 
#> $V60
#> [1] 76
#> 
#> $V61
#> [1] 58
#> 
#> $V62
#> [1] 4
#> 
#> $V63
#> [1] 79
#> 
#> $V64
#> [1] 12
#> 
#> $V65
#> [1] 23
#> 
#> $V66
#> [1] 66
#> 
#> $V67
#> [1] 41
#> 
#> $V68
#> [1] 92
#> 
#> $V69
#> [1] 36
#> 
#> $V70
#> [1] 16
#> 
#> $V71
#> [1] 99
#> 
#> $V72
#> [1] 33
#> 
#> $V73
#> [1] 34
#> 
#> $V74
#> [1] 67
#> 
#> $V75
#> [1] 95
#> 
#> $V76
#> [1] 61
#> 
#> $V77
#> [1] 44
#> 
#> $V78
#> [1] 27
#> 
#> $V79
#> [1] 8
#> 
#> $V80
#> [1] 19
#> 
#> $V81
#> [1] 100
#> 
#> $V82
#> [1] 18
#> 
#> $V83
#> [1] 39
#> 
#> $V84
#> [1] 88
#> 
#> $V85
#> [1] 85
#> 
#> $V86
#> [1] 62
#> 
#> $V87
#> [1] 21
#> 
#> $V88
#> [1] 40
#> 
#> $V89
#> [1] 49
#> 
#> $V90
#> [1] 80
#> 
#> $V91
#> [1] 70
#> 
#> $V92
#> [1] 20
#> 
#> $V93
#> [1] 32
#> 
#> $V94
#> [1] 37
#> 
#> $V95
#> [1] 14
#> 
#> $V96
#> [1] 43
#> 
#> $V97
#> [1] 3
#> 
#> $V98
#> [1] 81
#> 
#> $V99
#> [1] 47
#> 
#> $V100
#> [1] 6
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "leave-one-out"
## Leave-one-out with variable/unknown data set size n:
n <- 50
cvsegments(n, length.seg = 1)
#> $V1
#> [1] 46
#> 
#> $V2
#> [1] 22
#> 
#> $V3
#> [1] 18
#> 
#> $V4
#> [1] 49
#> 
#> $V5
#> [1] 11
#> 
#> $V6
#> [1] 17
#> 
#> $V7
#> [1] 3
#> 
#> $V8
#> [1] 2
#> 
#> $V9
#> [1] 31
#> 
#> $V10
#> [1] 28
#> 
#> $V11
#> [1] 15
#> 
#> $V12
#> [1] 33
#> 
#> $V13
#> [1] 35
#> 
#> $V14
#> [1] 13
#> 
#> $V15
#> [1] 44
#> 
#> $V16
#> [1] 40
#> 
#> $V17
#> [1] 25
#> 
#> $V18
#> [1] 26
#> 
#> $V19
#> [1] 41
#> 
#> $V20
#> [1] 39
#> 
#> $V21
#> [1] 16
#> 
#> $V22
#> [1] 10
#> 
#> $V23
#> [1] 34
#> 
#> $V24
#> [1] 21
#> 
#> $V25
#> [1] 4
#> 
#> $V26
#> [1] 1
#> 
#> $V27
#> [1] 29
#> 
#> $V28
#> [1] 14
#> 
#> $V29
#> [1] 42
#> 
#> $V30
#> [1] 9
#> 
#> $V31
#> [1] 5
#> 
#> $V32
#> [1] 27
#> 
#> $V33
#> [1] 8
#> 
#> $V34
#> [1] 50
#> 
#> $V35
#> [1] 30
#> 
#> $V36
#> [1] 48
#> 
#> $V37
#> [1] 6
#> 
#> $V38
#> [1] 32
#> 
#> $V39
#> [1] 38
#> 
#> $V40
#> [1] 12
#> 
#> $V41
#> [1] 43
#> 
#> $V42
#> [1] 37
#> 
#> $V43
#> [1] 20
#> 
#> $V44
#> [1] 19
#> 
#> $V45
#> [1] 23
#> 
#> $V46
#> [1] 7
#> 
#> $V47
#> [1] 36
#> 
#> $V48
#> [1] 24
#> 
#> $V49
#> [1] 45
#> 
#> $V50
#> [1] 47
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "leave-one-out"

## Data set with replicates
cvsegments(100, 25, nrep = 2)
#> $V1
#> [1] 31 32 77 78
#> 
#> $V2
#> [1] 33 34 67 68
#> 
#> $V3
#> [1] 71 72 57 58
#> 
#> $V4
#> [1]  3  4 13 14
#> 
#> $V5
#> [1] 25 26 65 66
#> 
#> $V6
#> [1] 73 74 61 62
#> 
#> $V7
#> [1]  1  2 51 52
#> 
#> $V8
#> [1] 19 20 75 76
#> 
#> $V9
#> [1] 95 96 39 40
#> 
#> $V10
#> [1] 47 48 53 54
#> 
#> $V11
#> [1] 45 46 91 92
#> 
#> $V12
#> [1] 29 30 17 18
#> 
#> $V13
#> [1] 79 80 49 50
#> 
#> $V14
#> [1]  5  6 63 64
#> 
#> $V15
#> [1] 23 24 93 94
#> 
#> $V16
#> [1] 37 38 35 36
#> 
#> $V17
#> [1] 83 84 97 98
#> 
#> $V18
#> [1]  43  44  99 100
#> 
#> $V19
#> [1] 11 12 55 56
#> 
#> $V20
#> [1] 15 16 87 88
#> 
#> $V21
#> [1] 27 28 21 22
#> 
#> $V22
#> [1] 69 70 85 86
#> 
#> $V23
#> [1]  9 10 89 90
#> 
#> $V24
#> [1]  7  8 41 42
#> 
#> $V25
#> [1] 81 82 59 60
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "random"
## Note that rows 1 and 2 are in the same segment, rows 3 and 4 in the
## same segment, and so on.

## Stratification
cvsegments(10, 3, type = "consecutive", stratify = c(rep(1,7), rep(2,3)))
#> $V1
#> [1] 1 2 3 8
#> 
#> $V2
#> [1] 4 5 9
#> 
#> $V3
#> [1]  6  7 10
#> 
#> attr(,"incomplete")
#> [1] 2
#> attr(,"type")
#> [1] "consecutive"
## Note that the last three samples are spread across the segments
## according to the stratification vector.
cvsegments(20, 3, type = "consecutive", nrep = 2, stratify = c(rep(1,7), rep(2,3)))
#> Warning: Segment length is not a multiple of the number of replicates.
#>   A best effort segment size will be used.
#> $V1
#> [1]  1  2  3  4  5  6 15 16
#> 
#> $V2
#> [1]  7  8  9 10 17 18
#> 
#> $V3
#> [1] 11 12 13 14 19 20
#> 
#> attr(,"incomplete")
#> [1] 2
#> attr(,"type")
#> [1] "consecutive"
## Note the length of stratify matching number of replicate sets, not samples.

## Converting a factor to segments
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
## Starting from a numeric vector
num <- c(1, 2, 1, 2, 3, 3)
fac2seg(factor(num))
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
