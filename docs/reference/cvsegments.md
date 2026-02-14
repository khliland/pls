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
#>  [1] 52 19 14 18  1 13 92 73 10 38
#> 
#> $V2
#>  [1] 99 96 84 80 47 46 28 37 21 63
#> 
#> $V3
#>  [1]  4 34 20 30 66  2 67 98 48  8
#> 
#> $V4
#>  [1] 97 17 24 85 25 79 54 11  9 88
#> 
#> $V5
#>  [1]  23  49  72  90 100   7  15  76  45  36
#> 
#> $V6
#>  [1] 33 41 82 27 35 32 69 16 40 39
#> 
#> $V7
#>  [1] 53 74 61 78 93 44 31 81 57 83
#> 
#> $V8
#>  [1] 86 59 64  5 60 55 62 42  3 77
#> 
#> $V9
#>  [1] 58 29 43 51 56 91 94 26 50 70
#> 
#> $V10
#>  [1] 87 75 95 71 22 89 12 65 68  6
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
#> [1] 97
#> 
#> $V2
#> [1] 31
#> 
#> $V3
#> [1] 7
#> 
#> $V4
#> [1] 72
#> 
#> $V5
#> [1] 92
#> 
#> $V6
#> [1] 83
#> 
#> $V7
#> [1] 63
#> 
#> $V8
#> [1] 90
#> 
#> $V9
#> [1] 10
#> 
#> $V10
#> [1] 75
#> 
#> $V11
#> [1] 24
#> 
#> $V12
#> [1] 68
#> 
#> $V13
#> [1] 1
#> 
#> $V14
#> [1] 89
#> 
#> $V15
#> [1] 15
#> 
#> $V16
#> [1] 35
#> 
#> $V17
#> [1] 46
#> 
#> $V18
#> [1] 56
#> 
#> $V19
#> [1] 2
#> 
#> $V20
#> [1] 78
#> 
#> $V21
#> [1] 55
#> 
#> $V22
#> [1] 70
#> 
#> $V23
#> [1] 84
#> 
#> $V24
#> [1] 11
#> 
#> $V25
#> [1] 94
#> 
#> $V26
#> [1] 98
#> 
#> $V27
#> [1] 42
#> 
#> $V28
#> [1] 64
#> 
#> $V29
#> [1] 51
#> 
#> $V30
#> [1] 54
#> 
#> $V31
#> [1] 82
#> 
#> $V32
#> [1] 74
#> 
#> $V33
#> [1] 76
#> 
#> $V34
#> [1] 60
#> 
#> $V35
#> [1] 38
#> 
#> $V36
#> [1] 30
#> 
#> $V37
#> [1] 95
#> 
#> $V38
#> [1] 28
#> 
#> $V39
#> [1] 85
#> 
#> $V40
#> [1] 9
#> 
#> $V41
#> [1] 17
#> 
#> $V42
#> [1] 88
#> 
#> $V43
#> [1] 66
#> 
#> $V44
#> [1] 25
#> 
#> $V45
#> [1] 29
#> 
#> $V46
#> [1] 13
#> 
#> $V47
#> [1] 61
#> 
#> $V48
#> [1] 49
#> 
#> $V49
#> [1] 81
#> 
#> $V50
#> [1] 58
#> 
#> $V51
#> [1] 69
#> 
#> $V52
#> [1] 100
#> 
#> $V53
#> [1] 71
#> 
#> $V54
#> [1] 77
#> 
#> $V55
#> [1] 26
#> 
#> $V56
#> [1] 4
#> 
#> $V57
#> [1] 93
#> 
#> $V58
#> [1] 12
#> 
#> $V59
#> [1] 23
#> 
#> $V60
#> [1] 91
#> 
#> $V61
#> [1] 47
#> 
#> $V62
#> [1] 5
#> 
#> $V63
#> [1] 50
#> 
#> $V64
#> [1] 39
#> 
#> $V65
#> [1] 40
#> 
#> $V66
#> [1] 33
#> 
#> $V67
#> [1] 37
#> 
#> $V68
#> [1] 62
#> 
#> $V69
#> [1] 8
#> 
#> $V70
#> [1] 19
#> 
#> $V71
#> [1] 67
#> 
#> $V72
#> [1] 52
#> 
#> $V73
#> [1] 18
#> 
#> $V74
#> [1] 45
#> 
#> $V75
#> [1] 73
#> 
#> $V76
#> [1] 32
#> 
#> $V77
#> [1] 16
#> 
#> $V78
#> [1] 41
#> 
#> $V79
#> [1] 27
#> 
#> $V80
#> [1] 21
#> 
#> $V81
#> [1] 87
#> 
#> $V82
#> [1] 57
#> 
#> $V83
#> [1] 59
#> 
#> $V84
#> [1] 44
#> 
#> $V85
#> [1] 79
#> 
#> $V86
#> [1] 22
#> 
#> $V87
#> [1] 20
#> 
#> $V88
#> [1] 36
#> 
#> $V89
#> [1] 6
#> 
#> $V90
#> [1] 80
#> 
#> $V91
#> [1] 43
#> 
#> $V92
#> [1] 65
#> 
#> $V93
#> [1] 96
#> 
#> $V94
#> [1] 34
#> 
#> $V95
#> [1] 99
#> 
#> $V96
#> [1] 3
#> 
#> $V97
#> [1] 14
#> 
#> $V98
#> [1] 53
#> 
#> $V99
#> [1] 48
#> 
#> $V100
#> [1] 86
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "leave-one-out"
## Leave-one-out with variable/unknown data set size n:
n <- 50
cvsegments(n, length.seg = 1)
#> $V1
#> [1] 28
#> 
#> $V2
#> [1] 15
#> 
#> $V3
#> [1] 44
#> 
#> $V4
#> [1] 46
#> 
#> $V5
#> [1] 33
#> 
#> $V6
#> [1] 42
#> 
#> $V7
#> [1] 35
#> 
#> $V8
#> [1] 13
#> 
#> $V9
#> [1] 3
#> 
#> $V10
#> [1] 49
#> 
#> $V11
#> [1] 37
#> 
#> $V12
#> [1] 25
#> 
#> $V13
#> [1] 26
#> 
#> $V14
#> [1] 10
#> 
#> $V15
#> [1] 21
#> 
#> $V16
#> [1] 29
#> 
#> $V17
#> [1] 1
#> 
#> $V18
#> [1] 40
#> 
#> $V19
#> [1] 39
#> 
#> $V20
#> [1] 14
#> 
#> $V21
#> [1] 4
#> 
#> $V22
#> [1] 38
#> 
#> $V23
#> [1] 9
#> 
#> $V24
#> [1] 5
#> 
#> $V25
#> [1] 50
#> 
#> $V26
#> [1] 22
#> 
#> $V27
#> [1] 8
#> 
#> $V28
#> [1] 11
#> 
#> $V29
#> [1] 16
#> 
#> $V30
#> [1] 34
#> 
#> $V31
#> [1] 18
#> 
#> $V32
#> [1] 36
#> 
#> $V33
#> [1] 48
#> 
#> $V34
#> [1] 31
#> 
#> $V35
#> [1] 20
#> 
#> $V36
#> [1] 45
#> 
#> $V37
#> [1] 47
#> 
#> $V38
#> [1] 41
#> 
#> $V39
#> [1] 6
#> 
#> $V40
#> [1] 19
#> 
#> $V41
#> [1] 30
#> 
#> $V42
#> [1] 2
#> 
#> $V43
#> [1] 27
#> 
#> $V44
#> [1] 24
#> 
#> $V45
#> [1] 23
#> 
#> $V46
#> [1] 7
#> 
#> $V47
#> [1] 32
#> 
#> $V48
#> [1] 12
#> 
#> $V49
#> [1] 43
#> 
#> $V50
#> [1] 17
#> 
#> attr(,"incomplete")
#> [1] 0
#> attr(,"type")
#> [1] "leave-one-out"

## Data set with replicates
cvsegments(100, 25, nrep = 2)
#> $V1
#> [1] 29 30 53 54
#> 
#> $V2
#> [1] 97 98 21 22
#> 
#> $V3
#> [1] 45 46 63 64
#> 
#> $V4
#> [1]  5  6 41 42
#> 
#> $V5
#> [1]  79  80  99 100
#> 
#> $V6
#> [1] 23 24 33 34
#> 
#> $V7
#> [1] 37 38 83 84
#> 
#> $V8
#> [1] 89 90  3  4
#> 
#> $V9
#> [1] 43 44 39 40
#> 
#> $V10
#> [1] 11 12 93 94
#> 
#> $V11
#> [1] 91 92 55 56
#> 
#> $V12
#> [1] 27 28 17 18
#> 
#> $V13
#> [1] 87 88 51 52
#> 
#> $V14
#> [1] 61 62 75 76
#> 
#> $V15
#> [1]  7  8 73 74
#> 
#> $V16
#> [1] 47 48  9 10
#> 
#> $V17
#> [1] 13 14 31 32
#> 
#> $V18
#> [1] 85 86 25 26
#> 
#> $V19
#> [1] 65 66 77 78
#> 
#> $V20
#> [1] 15 16  1  2
#> 
#> $V21
#> [1] 71 72 19 20
#> 
#> $V22
#> [1] 57 58 59 60
#> 
#> $V23
#> [1] 49 50 95 96
#> 
#> $V24
#> [1] 81 82 67 68
#> 
#> $V25
#> [1] 35 36 69 70
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
